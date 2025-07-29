rm(list = ls());gc()

library(tidyverse)
library(janitor)
library(data.table)
library(deflateBR)
library(nnet)
library(here)

# setup ------------------------------------------------------------------------

#run_or_load = "run"
run_or_load = "load"

if(run_or_load == "run"){
        # Functions ------------------------------------------------------------
        
        source(here("2_Scripts", "INE_functions.R"))
        
        # Loading data ---------------------------------------------------------
        
        d_raw <- fread(here("1_Data", "data_students.csv"))
        
        data_SM = tribble(~ano_enem, ~SM,
                          2012, 622,
                          2013, 678,
                          2014, 724,
                          2015, 788,
                          2016, 880) %>%
                
                mutate(real_SM = inpc(SM, 
                                      as.Date(paste0(ano_enem, "-05-01")),
                                      "03/2023"))
        
        # Recoding -------------------------------------------------------------
        
        d <- d_raw %>%
                left_join(data_SM) %>%
                
                mutate(renda = real_SM * (ponto_medio_renda/qtde_hab),
                       
                       renda_equiScale_rt2 = real_SM * (ponto_medio_renda/(qtde_hab^(1/2))),
                       
                       renda_equiScale_rt3 = real_SM * (ponto_medio_renda/(qtde_hab^(1/3))),
                       
                       income = cut(renda, 
                                    breaks = quantile(renda, probs = seq(0, 1, .2)), 
                                    labels = paste0(1:5, "Q"),
                                    include.lowest = T),
                       
                       income_D = cut(renda, 
                                    breaks = quantile(renda, probs = seq(0, 1, .1)), 
                                    labels = paste0(1:10, "D"),
                                    include.lowest = T),
                       
                       income_equiScale_rt2_D = cut(renda_equiScale_rt2, 
                                      breaks = quantile(renda_equiScale_rt2, probs = seq(0, 1, .1)), 
                                      labels = paste0(1:10, "D"),
                                      include.lowest = T),
                       
                       income_equiScale_rt3_D = cut(renda_equiScale_rt3, 
                                                    breaks = quantile(renda_equiScale_rt2, probs = seq(0, 1, .1)), 
                                                    labels = paste0(1:10, "D"),
                                                    include.lowest = T),
                       
                       performance = cut(desempenho, 
                                         breaks = quantile(desempenho, probs = seq(0, 1, .1)), 
                                         labels = paste0(1:10, "D"),
                                         include.lowest = T),
                       
                       choice = factor(dep_var, 
                                       levels = c("Não ingresso", "Pública", "Privada"),
                                       labels = c("No Access", "Public", "Private"), 
                                       ordered = T),
                       
                       ano_enem = factor(ano_enem))
        
        # Formulas ----------------------------------------------------------------
        
        formula_eduPar = choice ~ edupar*performance + 
                as.factor(ano_enem) +  sexo + cor + poly(idade, 2) + depadm + rural + uf
        
        formula_incomeQuintile = choice ~ income*performance + as.factor(ano_enem) + 
                sexo + cor + poly(idade, 2) + depadm + rural + uf
        
        formula_incomeDecile = choice ~ income_D*performance + as.factor(ano_enem) + 
                sexo + cor + poly(idade, 2) + depadm + rural + uf
        
        formula_income_equiScale_rt2_D = choice ~ income_equiScale_rt2_D*performance + as.factor(ano_enem) + 
                sexo + cor + poly(idade, 2) + depadm + rural + uf
        
        formula_incomeDecile_contrEdupar = choice ~ income_D*performance + 
                edupar + as.factor(ano_enem) + sexo + cor + poly(idade, 2) + depadm + rural + uf
        
        formula_incomeDecile_null     = choice ~ income_D 
        
        formula_incomeDecile_standard = choice ~ income_D*performance 
        
        
        # Data frames for the models -------------------------------------------
        
        d_aggregIncome = d %>% 
                select_at(c(all.vars(formula_incomeQuintile), "income_D")) %>%
                group_by_all() %>%
                count() %>%
                ungroup()
        
        
        d_aggregIncome_equiScale_rt2 = d %>% 
                select_at(c(all.vars(formula_incomeQuintile), "income_equiScale_rt2_D")) %>%
                group_by_all() %>%
                count() %>%
                ungroup()
        
        
        d_aggregEduPar = d %>% 
                select_at(c(all.vars(formula_eduPar))) %>%
                group_by_all() %>%
                count() %>%
                ungroup()
        
        d_aggregIncome_ContrEdupar = d %>% 
                select_at(c(all.vars(formula_incomeDecile_contrEdupar), "income")) %>%
                group_by_all() %>%
                count() %>%
                ungroup()
        
        gc()
        
        # Models ---------------------------------------------------------------
        
        reg_IncomeQuintile = multinom(formula_incomeQuintile, 
                                      data = d_aggregIncome, 
                                      weights = n,
                                      maxit = 1000, 
                                      Hess = T);gc()
        
        reg_IncomeDecile = multinom(formula_incomeDecile, 
                                      data = d_aggregIncome, 
                                      weights = n,
                                      maxit = 1000, 
                                      Hess = T);gc()
        
        reg_IncomeDecile_equiScale_rt2_D = multinom(formula_income_equiScale_rt2_D, 
                                                    data = d_aggregIncome_equiScale_rt2, 
                                                    weights = n,
                                                    maxit = 1000, 
                                                    Hess = T);gc()
        
        reg_IncomeEdupar = multinom(formula_eduPar, 
                                      data = d_aggregEduPar, 
                                      weights = n,
                                      maxit = 1000, 
                                      Hess = T);gc()
        
        reg_IncomeDecile_contrEdupar = multinom(formula_incomeDecile_contrEdupar, 
                                                data = d_aggregIncome_ContrEdupar, 
                                                weights = n,
                                                maxit = 1000, 
                                                Hess = T);gc()
        
        reg_null = multinom(formula_incomeDecile_null, 
                            data = d_aggregIncome, 
                            weights = n,
                            maxit = 1000, 
                            Hess = T);gc()
        
        reg_standard = multinom(formula_incomeDecile_standard, 
                            data = d_aggregIncome, 
                            weights = n,
                            maxit = 1000, 
                            Hess = T);gc()
        
        save(list = ls(), 
             file = here("1_Data", "processed", "R_evironment_estimatedModel.RData"))
        
}else{
        
        load(here("1_Data", "processed", "R_evironment_estimatedModel.RData"))
}

