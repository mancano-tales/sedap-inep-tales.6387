library(tidyverse)
library(ggridges)
library(janitor)
library(data.table)
library(nnet)
library(marginaleffects)
library(writexl)
library(Hmisc)
library(here)

# Setup ------------------------------------------------------------------------

rm(list = ls())

# Loading data -----------------------------------------------------------------

load(here("1_Data", "processed", "R_evironment_estimatedModel.RData"))
source(here("2_Scripts", "INE_functions.R"))

# Figure 1 ---------------------------------------------------------------------

p1 <- d %>% 
        mutate(income_fct = fct_recode(income_D, 
                                       "Bottom Decile (1D)" = "1D",
                                       "Top Decile (10D)"    = "10D")) %>%
        ggplot(aes(x = desempenho, y = income_fct, fill = income_fct)) + 
        stat_density_ridges(quantile_lines = T, quantiles = 2, alpha = .9) +
        scale_x_continuous("Performance Scores",
                           minor_breaks = F, 
                           breaks = seq(0, 1000, 100), 
                           limits = c(300, 800)) +
        scale_y_discrete("Household income per capita deciles") +
        theme_bw() +
        theme(legend.position = "none",
              axis.title = element_text(face = "bold"))

ggsave(plot     = p1, 
       filename = here("3_Output", "figure1_density_plots.png"),
       device = "png", dpi = 300, width = 5.5, height = 4)

# Figure 2 ---------------------------------------------------------------------

t_decile_observed <- d_aggregIncome_ContrEdupar %>%
        filter(income_D %in% c("1D", "10D")) %>%
        count(choice, income_D, performance, wt = n, name = "n_choice_ses_performance") %>%
        group_by(income_D, performance) %>%
        mutate(n_ses_performance = sum(n_choice_ses_performance),
               P = n_choice_ses_performance/n_ses_performance) %>%
        group_by(income_D, choice) %>%
        mutate(n_ses = sum(n_ses_performance )) %>% 
        ungroup() %>%
        mutate(I = n_ses_performance/n_ses,
               performance = as.numeric(performance),
               ses         = as.numeric(factor(income_D, 
                                               levels = c("1D", "10D"), 
                                               ordered = T))) %>%
        select(choice, performance, ses, P, I)

t_decile_estimated_wadc = prep_table(data        = d_aggregIncome, 
                                     reg         = reg_IncomeDecile,
                                     performance = "performance",
                                     ses         = "income_D",
                                     strategy    = "wadc")


t = t_decile_estimated_wadc %>% mutate(type = "Estimated") %>%
        mutate(choice  = factor(choice, 
                                levels = c("No Access", "Public", "Private"),
                                ordered = T),
               ses      = factor(ses,
                                 levels = c(1, 2),
                                 labels = c("1D", "10D"),
                                 ordered = T),
               type     = factor(type, 
                                 levels = c("Observed", "Estimated"),
                                 ordered = T))
                       

fig2 = t %>%
        ggplot(aes(x = performance, y = P, size = I, color = ses)) +
        geom_point(alpha = .5) +
        scale_size(range = c(1, 10)) +
        facet_grid(~choice) +
        scale_x_continuous(breaks = 1:10, 
                           minor_breaks = F) +
        scale_y_continuous(breaks = round(seq(0, 1, .1), 1), 
                           minor_breaks = F, limits = c(0, .9)) +
        scale_color_viridis_d(option = "C", direction = -1, begin = .2, end = .8) +
        labs(x = "Performance Decile",
             y = "Predicted Probability",
             color = "Income\nDecile",
             size  = "Cell Prop.") +
        theme_bw() +
        theme(axis.title   = element_text(face = "bold"), 
              legend.title = element_text(face = "bold"), 
              strip.background = element_rect(fill = "black"),
              strip.text       = element_text(face = "bold", colour = "white"))
        
ggsave(plot     = fig2, 
       filename = here("3_Output", "figure2_propOnlyPredicted.png"),
       device = "png", dpi = 300, width = 8.5, height = 3)

# Figure 3 ---------------------------------------------------------------------


adc_null = average_discrete_changes(data     = d_aggregIncome_ContrEdupar,
                                    reg      = reg_null,
                                    contrast = list(income_D = c("10D", "1D")),
                                    weight   = "n", 
                                    nsim = 5000)

adc_standard = average_discrete_changes(data     = d_aggregIncome_ContrEdupar,
                                        reg      = reg_standard,
                                        contrast = list(income_D = c("10D", "1D")),
                                        weight   = "n", 
                                        nsim = 5000)

adc_full = average_discrete_changes(data     = d_aggregIncome_ContrEdupar,
                                    reg      = reg_IncomeDecile,
                                    contrast = list(income_D = c("10D", "1D")),
                                    weight   = "n", 
                                    nsim = 5000, 
                                    cl = parallel::detectCores() - 1)

p025 = function(x){
        quantile(x, probs = .025)
}

p975 = function(x){
        quantile(x, probs = .975)
}

#t = adc_full

transform_adc = function(t){
        t %>%
                select(-sim) %>%
                group_by(cat) %>%
                summarise_all(.funs = list(mean = mean, lb = p025, ub = p975, sd = sd, var = var)) %>%
                pivot_longer(-cat, 
                             names_to = "series",
                             values_to = "values") %>%
                separate(series, into = c("series", "quantity"), sep = "_") %>%
                pivot_wider(names_from = quantity,
                            values_from = values)      
} 


adc_df <- bind_rows(transform_adc(adc_null)     %>% mutate(model = "Null"),
                    transform_adc(adc_standard) %>% mutate(model = "Standard"),
                    transform_adc(adc_full)     %>% mutate(model = "Full")) %>%
        mutate(ub_5sigma = mean + 5*sd,
               lb_5sigma = mean - 5*sd)

fig3 = adc_df %>%
        filter(series == "avg.diff") %>%
        mutate(model = factor(model, 
                              levels = c("Null", "Standard", "Full"),
                              ordered = T)) %>%
        ggplot(aes(x = cat, y = mean, color = model)) +
        geom_pointrange(aes(ymin = lb_5sigma,
                            ymax = ub_5sigma),
                        position = position_dodge2(width = .15), 
                        fatten = 2.5) +
        geom_hline(yintercept = 0, linetype = 2, color = "black") +
        scale_y_continuous(breaks = round(seq(-.6, .6, .1), 2), 
                           minor_breaks = F) +
        theme_bw() +
        labs(x = "Choice",
             y = "Average Discrete Change\n(Income: 10D - 1D)",
             color = "Model") +
        theme(axis.title = element_text(face = "bold"),
              legend.position = "bottom")


ggsave(plot     = fig3, 
       filename = here("3_Output", "figure3_averageDiscreteChangeModels.png"),
       device = "png", dpi = 300, width = 5, height = 4)


# Table 1 - INE Components -----------------------------------------------------

t = prep_table(data = d_aggregIncome,
               reg = reg_IncomeDecile, 
               ses = "income_D", 
               performance = "performance", 
               strategy = "wadc")

t = t %>%
        mutate(ses = factor(ses,
                            levels = 1:2,
                            labels = c("1", "10"),
                            ordered = T),
               
               choice  = factor(choice, 
                                levels = c("No Access", "Public", "Private"),
                                ordered = T))
               
table.1 = t %>% 
        mutate(P.I = P * I) %>%
        pivot_wider(names_from = "ses",
                    values_from = c(I, P, P.I)) %>%
        select(choice, performance, P_1, I_1, P.I_1, P_10, I_10, P.I_10) %>%
        mutate(INE_d = P.I_10 - P.I_1) %>%
        arrange(choice, performance)

write_xlsx(table.1, here("3_Output", "table1.xlsx"))

# Table 2 ----------------------------------------------------------------------


ine_results <- ine(data        = d_aggregIncome, 
                   reg         = reg_IncomeDecile, 
                   ses         = "income_D",
                   performance = "performance",
                   strategy    = "wadc")


absolute      = ine_results[[1]] %>% 
        mutate(type = "Absolute",
               choice  = factor(choice, 
                                levels = c("No Access", "Public", "Private"),
                                ordered = T)) %>%
        arrange(choice)
        
contributions = absolute %>%
        mutate_if(is.numeric, .funs = ~(./INE)) %>%
        mutate(type = "contributions")
        
table.2 = bind_rows(absolute, contributions)


write_xlsx(table.2, here("3_Output", "table2.xlsx"))


# Figure 4 ---------------------------------------------------------------------

t_fig4 = ine_results[[2]] %>%
        select(choice, performance, ends_with("Eff")) %>%
        pivot_longer(cols = ends_with("Eff"), 
                     names_to = "Effect",
                     values_to = "values") %>%
        mutate(Effect = ifelse(Effect == "Comp_Eff", "Primary Effect", "Secondary Effect"),
               values = 100*values,
               choice  = factor(choice, 
                                levels = c("No Access", "Public", "Private"),
                                ordered = T)) 

fig4 = t_fig4 %>%
        ggplot(aes(x = performance, weight = values, fill = Effect)) +
        geom_bar() +
        facet_wrap(~choice) +
        scale_x_continuous("Performance Deciles",
                           breaks = 1:10,
                           minor_breaks = F) +
        scale_y_continuous("Effect Size\n(in percentage points)",
                           breaks = round(seq(-35, 35, 5), 0),
                           minor_breaks = F) +
        scale_fill_viridis_d(option = "C", direction = -1, begin = .2, end = .8) +
        theme_bw() +
        theme(legend.position = "bottom", 
              axis.title = element_text(face = "bold"), 
              strip.background = element_rect(fill = "black"),
              strip.text = element_text(face = "bold", color = "white"))


ggsave(plot     = fig4, 
       filename = here("3_Output", "figure4_strackedBar.png"),
       device = "png", dpi = 300, width = 6.5, height = 3.5)

# Parametric Bootstrap ---------------------------------------------------------

#load_or_run = "run"
load_or_run = "load"

if(load_or_run == "run"){

        boots_atMean = ine_parametricBoots(data = d_aggregIncome,
                                           reg  = reg_IncomeDecile,
                                           performance = "performance",
                                           ses = "ses", 
                                           nsim = 5000,
                                           parallel = T,
                                           cl = parallel::detectCores()-1,
                                           strategy = "at_mean",
                                           upper_ses = NULL,
                                           lower_ses = NULL)
        
        boots_wadc = ine_parametricBoots(data = d_aggregIncome,
                                         reg  = reg_IncomeDecile,
                                         performance = "performance",
                                         ses = "ses", 
                                         nsim = 5000,
                                         parallel = T,
                                         cl = parallel::detectCores()-1,
                                         strategy = "wadc",
                                         upper_ses = NULL,
                                         lower_ses = NULL)

        save(list = ls(), 
             file = here("1_Data", "processed", "R_evironment_results_withBootstrap.RData"))        
}else{
        load(here("1_Data", "processed", "R_evironment_results_withBootstrap.RData"))
}


# Figure 5 ---------------------------------------------------------------------

t_fig5 <- boots_wadc[[2]] %>%
        select(choice, performance, CA, Bernardi_CA) %>%
        pivot_longer(cols = -c(choice, performance),
                     names_to = "var",
                     values_to = "values") %>%
        group_by(choice, performance, var) %>%
        summarise(mean = 100 * mean(values), 
                  p025 = 100 * quantile(values, probs = .025),
                  p975 = 100 * quantile(values, probs = .975)) %>%
        ungroup() %>%
        mutate(var = factor(var, 
                            levels = c("CA", "Bernardi_CA"),
                            labels = c('CA', 'B&T (2020)'),
                            ordered = T),
               
               choice  = factor(choice, 
                                levels = c("No Access", "Public", "Private"),
                                ordered = T))

fig5 =  t_fig5 %>%
        ggplot(aes(x = performance, y = mean, fill = var)) +
        geom_ribbon(aes(ymin = p025, ymax = p975), alpha = .6) +
        geom_line(color = "black", linewidth = .5) +
        geom_hline(yintercept = 0, linetype = 2, col = "black") +
        facet_wrap(~choice) +
        scale_x_continuous(breaks = 1:10, 
                           minor_breaks = F) +
        scale_y_continuous(breaks = round(seq(-10, 10, 1),0), 
                           minor_breaks = F, 
                           limits = c(-6, 6)) +
        scale_color_viridis_d(option = "C", 
                              direction = -1, 
                              begin = .2, 
                              end = .8) +
        labs(x = "Performance Deciles",
             y = "Effect Size\n(in percentage points)") +
        theme_bw() +
        theme(axis.title = element_text(face = "bold"),
              legend.position = "bottom", 
              legend.title = element_blank(),
              strip.background = element_rect(fill = "black"),
              strip.text       = element_text(face = "bold", color = "white"))


ggsave(plot     = fig5, 
       filename = here("3_Output", "figure5_CA_by_performanceStrata.png"),
       device = "png", dpi = 300, width = 6.5, height = 3.5)







