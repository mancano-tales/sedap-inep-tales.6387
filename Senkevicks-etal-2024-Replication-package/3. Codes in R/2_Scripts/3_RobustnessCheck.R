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

load(here("1_Data", "processed", "R_evironment_results_withBootstrap.RData"))
source(here("2_Scripts", "INE_functions.R"))


# Pairwise and total results ---------------------------------------------------

#load_or_run = "run"
load_or_run = "load"

if(load_or_run == "run"){
        pairwise_result = pairwise_total_ine(data = d_aggregIncome,
                                             reg  = reg_IncomeDecile,
                                             ses         = "income_D",
                                             performance = "performance",
                                             choice      = "choice",
                                             strategy    = "wadc", 
                                             parallel = T,
                                             cl = parallel::detectCores() - 1)  
        
        save(list = ls(), 
             file = here("1_Data", "processed", "R_evironment_results_pairwiseTotal.RData"))        
}else{
        load(here("1_Data", "processed", "R_evironment_results_pairwiseTotal.RData"))
}




# Table 4- Pairwise ------------------------------------------------------------------------

M_ine_noAccess    = M_ine_Public    = M_ine_Private    = matrix("-", 10, 10)
M_meanEf_noAccess = M_meanEf_Public = M_meanEf_Private = matrix("-", 10, 10)
M_compEf_noAccess = M_compEf_Public = M_compEf_Private = matrix("-", 10, 10)
M_CA_noAccess     = M_CA_Public     = M_CA_Private     = matrix("-", 10, 10)

tmp = pairwise_result$pairwise_ine$result_aggregated  %>%
        filter(choice == "No Access")
for(i in 1:nrow(tmp)){
        M_ine_noAccess[tmp$U[i], tmp$L[i]]    <- round(tmp$INE[i],      15)
        M_meanEf_noAccess[tmp$U[i], tmp$L[i]] <- round(tmp$Mean_Eff[i], 15)
        M_compEf_noAccess[tmp$U[i], tmp$L[i]] <- round(tmp$Comp_Eff[i], 15)
        M_CA_noAccess[tmp$U[i], tmp$L[i]]     <- round(tmp$CA[i],       15)
}

tmp = pairwise_result$pairwise_ine$result_aggregated  %>%
        filter(choice == "Public")
for(i in 1:nrow(tmp)){
        M_ine_Public[tmp$U[i], tmp$L[i]]    <- round(tmp$INE[i],      15)
        M_meanEf_Public[tmp$U[i], tmp$L[i]] <- round(tmp$Mean_Eff[i], 15)
        M_compEf_Public[tmp$U[i], tmp$L[i]] <- round(tmp$Comp_Eff[i], 15)
        M_CA_Public[tmp$U[i], tmp$L[i]]     <- round(tmp$CA[i],       15)
}

tmp = pairwise_result$pairwise_ine$result_aggregated  %>%
        filter(choice == "Private")
for(i in 1:nrow(tmp)){
        M_ine_Private[tmp$U[i], tmp$L[i]]    <- round(tmp$INE[i],      15)
        M_meanEf_Private[tmp$U[i], tmp$L[i]] <- round(tmp$Mean_Eff[i], 15)
        M_compEf_Private[tmp$U[i], tmp$L[i]] <- round(tmp$Comp_Eff[i], 15)
        M_CA_Private[tmp$U[i], tmp$L[i]]     <- round(tmp$CA[i],       15)
}

colnames(M_ine_noAccess)    <- colnames(M_ine_Public)    <- colnames(M_ine_Private)    <- paste("ine_",    1:10)
colnames(M_compEf_noAccess) <- colnames(M_compEf_Public) <- colnames(M_compEf_Private) <- paste("compEf_", 1:10)
colnames(M_meanEf_noAccess) <- colnames(M_meanEf_Public) <- colnames(M_meanEf_Private) <- paste("meanEf_", 1:10)
colnames(M_CA_noAccess)     <- colnames(M_CA_Public)     <- colnames(M_CA_Private)     <- paste("CA_",     1:10)

noAccess = cbind(M_ine_noAccess    , ".",
                 M_compEf_noAccess , ".",
                 M_meanEf_noAccess , ".",
                 M_CA_noAccess)

public = cbind(M_ine_Public,     ".",
               M_compEf_Public,  ".",
               M_meanEf_Public,  ".",
               M_CA_Public)

private = cbind(M_ine_Private,    ".",
                M_compEf_Private, ".",
                M_meanEf_Private, ".",
                M_CA_Private)

emptyLine = matrix(".", ncol = ncol(noAccess), nrow = 1)
title_noAccess = title_public = title_private = emptyLine

title_noAccess[, 1] <- "noAccess"
title_public[, 1  ] <- "public"
title_private[, 1]  <- "private"

auxTable = rbind(title_noAccess, 
                  noAccess,
                  emptyLine,
                  
                  title_public,
                  public,
                  emptyLine,
                  
                  title_private,
                  private)

write_xlsx(x = as_tibble(auxTable), here("3_Output", "table_appendix_pairwise.xlsx"))

# Table 3 - Total ------------------------------------------------------------------------

number_of_comparisons = nrow(pairwise_result$pairwise_ine$result_aggregated %>% filter(choice == "No Access"))

table3a = pairwise_result$total_ine$result_aggregated %>%
        mutate(series = "absolute") %>%
        select(series, everything()) %>%
        mutate(choice  = factor(choice, 
                                levels = c("No Access", "Public", "Private"),
                                ordered = T)) %>%
        arrange(choice)

table3b = table3a %>%
        mutate_at(c("Comp_Eff", "Mean_Eff", "CA", "Bernardi_CA", "INE"), 
                  ~(./INE)) %>%
        mutate(series = "contribution")

table3 = bind_rows(table3a, table3b) %>%
        arrange(series, choice)

write_xlsx(table3, here("3_Output", "table3.xlsx"))

# Robustness - model specification ------------------------------------------------------------------------

t_decile <- prep_table(data        = d_aggregIncome, 
                       reg         = reg_IncomeDecile, 
                       ses         = "income_D",
                       performance = "performance",
                       strategy = "wadc")

t_quintile <- prep_table(data        = d_aggregIncome, 
                         reg         = reg_IncomeQuintile, 
                         ses         = "income",
                         performance = "performance",
                         strategy = "wadc")

t_edupar <- prep_table(data        = d_aggregEduPar, 
                       reg         = reg_IncomeEdupar,
                       ses         = "edupar",
                       performance = "performance",
                       strategy = "wadc")

t_income_contrEdupar <- prep_table(data        = d_aggregIncome_ContrEdupar, 
                                   reg         = reg_IncomeDecile_contrEdupar,
                                   ses         = "income_D",
                                   performance = "performance",
                                   strategy = "wadc")

t_income_equiScale_rt2 <- prep_table(data        = d_aggregIncome_equiScale_rt2, 
                                     reg         = reg_IncomeDecile_equiScale_rt2_D,
                                     ses         = "income_equiScale_rt2_D",
                                     performance = "performance",
                                     strategy = "wadc")

ine_quintile              = ine_internal_function(t_quintile, ses = "ses", performance = "performance", choice = "choice")
ine_decile                = ine_internal_function(t_decile,   ses = "ses", performance = "performance", choice = "choice")
ine_edupar                = ine_internal_function(t_edupar,   ses = "ses", performance = "performance", choice = "choice")
ine_income_contrEdupar    = ine_internal_function(t_income_contrEdupar,   ses = "ses", performance = "performance", choice = "choice")
ine_income_equiScale_rt2  = ine_internal_function(t_income_equiScale_rt2,   ses = "ses", performance = "performance", choice = "choice")

INE_sensitivity = ine_decile[[1]]               %>% select(choice, decile                 = INE) %>%
        left_join(ine_quintile[[1]]             %>% select(choice, quintile               = INE)) %>%
        left_join(ine_edupar[[1]]               %>% select(choice, eduPar                 = INE)) %>%
        left_join(ine_income_contrEdupar[[1]]   %>% select(choice, decile_contrEdupar     = INE)) %>%
        left_join(ine_income_equiScale_rt2[[1]] %>% select(choice, income_equiScale_rt2_D = INE)) 

COMP_sensitivity = ine_decile[[1]]              %>% select(choice, decile          = Comp_Eff) %>%
        left_join(ine_quintile[[1]]           %>% select(choice, quintile          = Comp_Eff)) %>%
        left_join(ine_edupar[[1]]             %>% select(choice, eduPar            = Comp_Eff)) %>%
        left_join(ine_income_contrEdupar[[1]] %>% select(choice, decile_contrEdupar= Comp_Eff)) %>%
        left_join(ine_income_equiScale_rt2[[1]] %>% select(choice, income_equiScale_rt2_D = Comp_Eff)) 

CA_sensitivity = ine_decile[[1]]              %>% select(choice, decile            = CA) %>%
        left_join(ine_quintile[[1]]           %>% select(choice, quintile          = CA)) %>%
        left_join(ine_edupar[[1]]             %>% select(choice, eduPar            = CA)) %>%
        left_join(ine_income_contrEdupar[[1]] %>% select(choice, decile_contrEdupar= CA)) %>%
        left_join(ine_income_equiScale_rt2[[1]] %>% select(choice, income_equiScale_rt2_D = CA)) 

modelSensitivity = bind_rows(INE_sensitivity  %>% mutate(series = "INE"),
                             COMP_sensitivity %>% mutate(series = "Primary"),
                             CA_sensitivity   %>% mutate(series = "CA"))

write_xlsx(modelSensitivity, here("3_Output", "table4_sensitivity.xlsx"))

# -------------------------------------------------------------------------------

save_robustness = F

if(save_robustness == T){
        save(list = ls(), 
             file = here("1_Data", "processed", "R_evironment_results_withRobustness.RData"))        
}

#load(here("1_Data", "processed", "R_evironment_results_withRobustness.RData"))        









