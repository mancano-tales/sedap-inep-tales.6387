# Functions --------------------------------------------------------------------

wtd.mode = function(x, w){
        
        table = Hmisc::wtd.table(x, w)
        
        table$x[which(table$sum.of.weights == max(table$sum.of.weights))]
        
}

ipf_set_margins = function(data, target_margins, weight){
        
        #target_margin_i = target_margins[[1]]
        for(target_margin_i in target_margins){
                
                var_target_i = names(target_margin_i)[1]
                
                check_target  = target_margin_i[[var_target_i]] %in% data[[var_target_i]]
                not_in_target = target_margin_i[[var_target_i]][!check_target]
                
                check_source  = unique(data[[var_target_i]]) %in% target_margin_i[[var_target_i]]
                not_in_source = unique(data[[var_target_i]])[!check_source]
                
                if(length(not_in_target) > 0){
                        stop(paste("The following categories of variable", var_target_i, "are present in the source data, but not in the target marginal distribution:", not_in_target))
                }
                
                if(length(not_in_source) > 0){
                        stop(paste("The following categories of variable", var_target_i, "are present in the target marginal distribution, but not in the source data:", not_in_source))
                }
                
        }
        
        target_vars = names(target_margins)
        source_vars = setdiff(names(data), c(target_vars, weight))
        
        source_margins <- lapply(source_vars, \(x){
                t = data %>% 
                        count(!!rlang::sym(x), wt = !!rlang::sym(weight)) %>%
                        mutate(n = n/sum(n))
                names(t)[1] <- x
                t
        })
        names(source_margins) <- source_vars
        
        margins_to_reach = c(source_margins, target_margins)
        margins_to_reach <- lapply(margins_to_reach, FUN = \(x){
                x %>% rename(target_n = n)
        })
        
        data$old_weight = data[[weight]]
        
        var_margins = names(margins_to_reach)
        
        #i = 1
        #var_margins_i = var_margins[i]
        diff = 1
        while(diff > .001){
                
                diff_rodada = rep(0, length(var_margins))
                
                for(i in 1:length(var_margins)){
                        
                        var_margins_i = var_margins[i]
                        
                        #print(var_margins_i)
                        
                        change_in_margins = data %>%
                                count(!!rlang::sym(var_margins_i), wt = old_weight) %>%
                                mutate(source_n = n/sum(n)) %>%
                                left_join(margins_to_reach[[var_margins_i]], by = var_margins_i) %>%
                                mutate(ratio = target_n/source_n) %>%
                                select(-source_n, -target_n, -n)
                        
                        data = data %>% 
                                
                                left_join(change_in_margins, by = var_margins_i) %>%
                                
                                mutate(new_weight = old_weight * ratio,
                                       diff       = new_weight - old_weight) %>%
                                
                                select(-ratio)
                        
                        diff_rodada[i] = sum(data$diff^2)
                        #print(diff_rodada)
                        
                        data$old_weight <- data$new_weight
                        data$new_weight = NULL
                        data$diff = NULL
                        
                }
                
                diff = sum(diff_rodada)
                
                print(diff)
                
        }
        
        data[[weight]] <- data$old_weight
        data$old_weight = NULL
        data
}

simulate_beta = function(reg, nsim){
        
        beta_matrix = coef(reg)
        Sigma  = vcov(reg)
        
        names_cat = rownames(beta_matrix)
        n_cat     = nrow(beta_matrix)
        n_coef    = ncol(beta_matrix)
        
        beta = NULL
        for(i in 1:n_cat){
                beta = c(beta, beta_matrix[i, ])
        }
        
        beta_sim = MASS::mvrnorm(n = nsim, 
                                 mu = beta, 
                                 Sigma = Sigma)
        
        coef_start = 1
        coef_end   = n_coef
        beta_list  = list()
        for(i in 1:n_cat){
                
                beta_list[[ names_cat[i] ]] <- beta_sim[, coef_start:coef_end]
                
                coef_start = coef_end + 1
                coef_end   = coef_end + n_coef
        }
        
        beta_list
}


get_sim_betaMatrix_i = function(beta_list, sim_i){
        
        beta_matrix_i = do.call(what = rbind, args = lapply(beta_list, \(x) x[sim_i, ]))
        beta_matrix_i
}

get_multinom_pred = function(beta_matrix, X, ref_cat_name = NULL){
        
        exp_XB = cbind(rep(1, nrow(X)), exp(X %*% t(beta_matrix)))
        prob_pred = exp_XB/rowSums(exp_XB)
        
        if(!is.null(ref_cat_name)){
                colnames(prob_pred)[1] <- ref_cat_name
        }
        
        prob_pred
}


get_X_w_wgtAverageDiscreteChange = function(data,
                                            reg,
                                            performance,
                                            ses,
                                            upper_ses   = NULL,
                                            lower_ses   = NULL){
        
        f = formula(reg)
        
        data <- data %>%
                select_at(c(all.vars(f)))
        
        if("weight" %in% names(data)){
                weight_name = paste(sample(letters, size = 15, replace = T), collapse = "")
                data[[weight_name]] <- as.numeric(reg$weights)
        }else{
                weight_name = "weight"
                data[["weight"]] <- as.numeric(reg$weights)
        }
        
        #data[["ses"]]         = data[[ses]]
        #data[["performance"]] = data[[performance]]
        #data[[ses]]         <- NULL
        #data[[performance]] <- NULL
        
        
        if(is.null(upper_ses)){
                upper_ses =  last(sort(unique(data[[ses]])))        
        }
        
        if(is.null(lower_ses)){
                lower_ses = first(sort(unique(data[[ses]])))
        }
        
        target_margin_U = list(performance = data %>% 
                                       filter_at(.vars = ses, ~(.  %in% c(upper_ses))) %>%
                                       count(!!rlang::sym(performance), wt = !!rlang::sym(weight_name)) %>%
                                       mutate(n = n/sum(n)))
        
        target_margin_L = list(performance = data %>% 
                                       filter_at(.vars = ses, ~(.  %in% c(lower_ses))) %>%
                                       count(!!rlang::sym(performance), wt = !!rlang::sym(weight_name)) %>%
                                       mutate(n = n/sum(n)))
        
        data_upperMargin = ipf_set_margins(data = data, 
                                           target_margins = target_margin_U, 
                                           weight = weight_name)
        
        data_lowerMargin = ipf_set_margins(data = data, 
                                           target_margins = target_margin_L, 
                                           weight = weight_name)
        
        data_upperMargin[[ses]] <- upper_ses
        data_lowerMargin[[ses]] <- lower_ses
        
        terms <- delete.response(reg$terms)
        
        X_U <- model.matrix(terms, 
                            data = data_upperMargin, 
                            na.action = na.omit, 
                            xlev = reg$xlevels)
        
        X_L <- model.matrix(terms, 
                            data = data_lowerMargin, 
                            na.action = na.omit, 
                            xlev = reg$xlevels)
        
        w_U = data_upperMargin %>% mutate(w = !!rlang::sym(weight_name)/sum(!!rlang::sym(weight_name))) %>% select_at(c(ses, performance, "w")) %>% setNames(c("ses", "performance", "w"))
        w_L = data_lowerMargin %>% mutate(w = !!rlang::sym(weight_name)/sum(!!rlang::sym(weight_name))) %>% select_at(c(ses, performance, "w")) %>% setNames(c("ses", "performance", "w"))
        
        list_X_w = list(X_U = X_U,
                        X_L = X_L,
                        w_U = w_U,
                        w_L = w_L)
        
        list_X_w
}


get_X_w_atMean = function(data,
                          reg,
                          performance,
                          ses,
                          upper_ses   = NULL,
                          lower_ses   = NULL){
        
        
        f = formula(reg)
        
        data <- data %>%
                select_at(c(all.vars(f)))
        
        if("weight" %in% names(data)){
                weight = paste(sample(letters, size = 15, replace = T), collapse = "")
                data[[weight]] <- as.numeric(reg$weights)
        }else{
                weight = "weight"
                data[["weight"]] <- as.numeric(reg$weights)
        }
        
        if(is.null(upper_ses)){
                upper_ses =  last(sort(unique(data[[ses]])))        
        }
        
        if(is.null(lower_ses)){
                lower_ses = first(sort(unique(data[[ses]])))
        }
        
        non_numerical <- data %>%
                select_at(setdiff(names(.), c(ses, performance, weight, as.character(formula(reg)[[2]])))) %>%
                select_if(~!is.numeric(.)) %>%
                summarise_all(~wtd.mode(., data[[weight]]))
        
        numerical <- data %>%
                select_at(setdiff(names(.), c(ses, performance, weight, as.character(formula(reg)[[2]])))) %>%
                select_if(is.numeric) %>%
                summarise_all(~wtd.mean(., data[[weight]]))
        
        datagrid_U = expand.grid(performance = sort(unique(data[[performance]])),
                                 ses = upper_ses) %>%
                setNames(c(performance, ses))
        
        datagrid_L = expand.grid(performance = sort(unique(data[[performance]])),
                                 ses = lower_ses) %>%
                setNames(c(performance, ses))
        
        datagrid_U <- bind_cols(datagrid_U, bind_cols(numerical, non_numerical)[rep(1, nrow(datagrid_U)),])
        datagrid_L <- bind_cols(datagrid_L, bind_cols(numerical, non_numerical)[rep(1, nrow(datagrid_L)),])
        
        terms <- delete.response(reg$terms)
        
        X_U <- model.matrix(terms, 
                            data = datagrid_U, 
                            na.action = na.omit, 
                            xlev = reg$xlevels)
        
        X_L <- model.matrix(terms, 
                            data = datagrid_L, 
                            na.action = na.omit, 
                            xlev = reg$xlevels)
        
        d_prop <- data %>% 
                select_at(c(ses, performance, weight)) %>%
                setNames(c("ses", "performance", 'weight')) %>%
                filter(ses %in% c(upper_ses, lower_ses)) %>%
                arrange(ses, performance) %>%
                group_by(ses, performance) %>%
                summarise(n = sum(weight), .groups = "drop") 
        
        w_U = d_prop %>% filter(ses == upper_ses) %>% mutate(w = n/sum(n)) %>% select(-n)
        w_L = d_prop %>% filter(ses == lower_ses) %>% mutate(w = n/sum(n)) %>% select(-n)
        
        list(X_U = X_U,
             X_L = X_L,
             w_U = w_U,
             w_L = w_L)
}



#list_X_w = get_X_w_wgtAverageDiscreteChange(data = d_aggregIncome,
#                                            reg = reg_IncomeDecile,
#                                            performance = "performance",
#                                            ses = "income_D")
#
#beta_matrix = coef(reg_IncomeDecile)
#ref_cat_name = reg$lab[1]
        
get_table_for_ine = function(list_X_w,
                             beta_matrix,
                             ref_cat_name = NULL){
        
        
        P_U = get_multinom_pred(X = list_X_w$X_U,
                                beta_matrix  = beta_matrix,
                                ref_cat_name = ref_cat_name)
        
        P_L = get_multinom_pred(X = list_X_w$X_L,
                                beta_matrix  = beta_matrix,
                                ref_cat_name = ref_cat_name)
        
        
        I_U = as_tibble(list_X_w$w_U[, "w"]) %>% setNames("I_U")
        I_L = as_tibble(list_X_w$w_L[, "w"]) %>% setNames("I_L")
        
        multiple_I_U = I_U %>% setNames(paste0("I_U", 1))
        multiple_I_L = I_L %>% setNames(paste0("I_L", 1))
        
        for(i in 2:ncol(P_U)){
                
                multiple_I_U = bind_cols(multiple_I_U,
                                         I_U %>% setNames(paste0("I_U", i)))
                
                multiple_I_L = bind_cols(multiple_I_L,
                                         I_L %>% setNames(paste0("I_L", i)))
        }
        
        t = bind_cols(tibble(performance = list_X_w$w_U$performance),
                      (as_tibble(P_U)  * multiple_I_U) %>% setNames(paste0("P-", names(.),"_U")),
                      (as_tibble(P_L)  * multiple_I_L) %>% setNames(paste0("P-", names(.),"_L")),
                      I_U, 
                      I_L) %>%
                group_by_at(.vars = c("performance")) %>%
                summarise_all(sum) %>%
                pivot_longer(cols = -c(performance)) %>%
                separate(name, into = c("var", "ses"), sep = "_") %>%
                pivot_wider(names_from = var,
                            values_from = value) %>%
                pivot_longer(cols = -c(performance, ses, I),
                             names_to = "choice",
                             values_to = "P") %>%
                mutate(choice = str_remove(choice, "^P-"),
                       P = P/I, 
                       ses = as.numeric(as.ordered(ses)),
                       performance = as.numeric(as.ordered(performance))) %>%
                arrange(ses, performance, choice) %>%
                select(choice, performance, ses, P, I)
        
        t
        
}


ine_internal_function = function(t, ses, performance, choice = NULL, upper_ses = NULL, lower_ses = NULL){
        
        t[["ses"]]         = t[[ses]]
        t[["performance"]] = t[[performance]]
        
        if(is.null(upper_ses)){
                upper_ses =  last(sort(unique(t$ses)))        
        }
        
        if(is.null(lower_ses)){
                lower_ses = first(sort(unique(t$ses)))
        }
        
        
        max_performance = last(sort(unique(t$performance)))
        
        if(is.null(choice)){
                t$choice = "tmp"
        }else{
                t[["choice"]] = t[[choice]]
        }
        
        
        result_disaggregated  <- 
                t %>%
                filter(ses %in% c(lower_ses, upper_ses)) %>%
                select(choice, ses, performance, P, I) %>%
                arrange(choice, performance, ses) %>%
                group_by(choice, performance) %>%
                summarise(P_L = P[which(ses == lower_ses)],
                          
                          I_U = I[which(ses == upper_ses)],
                          I_L = I[which(ses == lower_ses)],
                          
                          P_bar = mean(P),
                          I_bar = mean(I),
                          
                          diff_P = diff(P),
                          diff_I = diff(I), .groups = "drop") %>%
                group_by(choice) %>%
                mutate(diff_P_bar  = mean(diff_P),
                       diff_P_max = diff_P[which(performance == max_performance)],
                       
                       Comp_Eff      = P_bar*diff_I,
                       Mean_Eff      = diff_P*I_bar,
                       
                       CA = (diff_P - diff_P_max)*I_bar,
                       INE_S = ((P_L + diff_P_max)*I_U - P_L*I_L),
                       Bernardi_CA = (Comp_Eff + Mean_Eff) - INE_S) %>%
                ungroup()
        
        
        result_aggregated <- result_disaggregated %>%
                select(-ends_with("_stratum")) %>%
                group_by(choice) %>%
                summarise(Comp_Eff    = sum(Comp_Eff),
                          Mean_Eff    = sum(Mean_Eff),
                          CA          = sum(CA),
                          Bernardi_CA = sum(Bernardi_CA),
                          .groups = "drop") %>%
                mutate(INE = Comp_Eff + Mean_Eff)
        
        if(is.null(choice)){
                result$choice = NULL
        }
        
        list(result_aggregated = result_aggregated,
             result_disaggregated = result_disaggregated)
}



ine_parametricBoots = function(data, 
                               reg,
                               performance,
                               ses,
                               nsim = 2000,
                               upper_ses = NULL,
                               lower_ses = NULL,
                               parallel = T,
                               cl = 4,
                               strategy = c("wadc", "at_mean")){
        
        
        if(length(strategy) > 1){
                strategy = strategy[1]
        }
        
        ref_cat_name = reg$lab[1]
        
        if(strategy == "at_mean"){
                list_X_w <- get_X_w_atMean(data = data, 
                                           reg  = reg,
                                           performance = "performance",
                                           ses = "income_D", 
                                           upper_ses = upper_ses,
                                           lower_ses = lower_ses)       
        }
        
        if(strategy == "wadc"){
                list_X_w <- get_X_w_wgtAverageDiscreteChange(data = data,
                                                             reg  = reg,
                                                             performance = "performance",
                                                             ses = "income_D", 
                                                             upper_ses = upper_ses,
                                                             lower_ses = lower_ses)        
        }
        
        beta_list = simulate_beta(reg = reg, nsim = nsim)
        
        #i = 1
        sim = function(i){
                beta_matrix_i = get_sim_betaMatrix_i(beta_list = beta_list, sim_i = i)
                
                t = get_table_for_ine(list_X_w = list_X_w,
                                      beta_matrix = beta_matrix_i,
                                      ref_cat_name = ref_cat_name)
                
                ine_internal_function(t = t, 
                                      ses = "ses", 
                                      performance = "performance", 
                                      choice = "choice",
                                      upper_ses = upper_ses,
                                      lower_ses = lower_ses)        
        }
        
        
        #nsim = 25
        
        if(parallel == F){
                sims = lapply(1:nsim, \(i) sim(i))
                
                result_aggregated <- do.call(bind_rows, 
                                             lapply(1:length(sims), \(i){
                                                     x = sims[[i]]
                                                     tmp =  x$result_aggregated
                                                     tmp$sim = i
                                                     tmp
                                             }))
                
                result_disaggregated <- do.call(bind_rows, 
                                                lapply(1:length(sims), \(i){
                                                        x = sims[[i]]
                                                        tmp =  x$result_disaggregated
                                                        tmp$sim = i
                                                        tmp
                                                }))
                
        }else{
                library(foreach)
                library(doParallel)
                doParallel::registerDoParallel(cl)
                sims = foreach::foreach(i= 1:nsim, .combine=c, .verbose = T,
                                        .packages = c("dplyr", "tidyr", "purrr", "tibble", "stringr"), 
                                        .export = c("ine_internal_function", "get_sim_betaMatrix_i", "get_table_for_ine", "get_multinom_pred",
                                                    "performance")) %dopar% {
                                                            
                                                            sim_i = sim(i) 
                                                            sim_i[[1]]$sim = i
                                                            sim_i[[2]]$sim = i
                                                            
                                                            sim_i
                                                            
                                                    }
                doParallel::stopImplicitCluster()
                
                result_aggregated <- do.call(bind_rows, 
                                             sims[which(names(sims) %in% "result_aggregated")])
                
                result_disaggregated <- do.call(bind_rows, 
                                                sims[which(names(sims) %in% "result_disaggregated")])
                
                
        }
        
        list(result_aggregated    = result_aggregated,
             result_disaggregated = result_disaggregated)
        
}



#data        = d_aggregIncome
#reg         = reg_IncomeQuintile
#performance = "performance"
#ses         = "income"
#strategy    = "wadc"

prep_table = function(data, 
                      reg,
                      performance,
                      ses,
                      upper_ses = NULL,
                      lower_ses = NULL,
                      strategy = c("wadc", "at_mean")){
        
        if(length(strategy) > 1){
                strategy = strategy[1]
        }
        
        ref_cat_name = reg$lab[1]
        
        
        if(strategy == "at_mean"){
                list_X_w <- get_X_w_atMean(data = data, 
                                           reg  = reg,
                                           performance = performance,
                                           ses = ses, 
                                           upper_ses = upper_ses,
                                           lower_ses = lower_ses)       
        }
        
        if(strategy == "wadc"){
                list_X_w <- get_X_w_wgtAverageDiscreteChange(data = data,
                                                             reg  = reg,
                                                             performance = performance,
                                                             ses = ses,  
                                                             upper_ses = upper_ses,
                                                             lower_ses = lower_ses)        
        }
        
        
        beta_matrix = coef(reg)
        
        t = get_table_for_ine(list_X_w = list_X_w,
                              beta_matrix = beta_matrix,
                              ref_cat_name = ref_cat_name)
        
        #t_benchmark == t
        t
}


ine = function(data, 
               reg,
               performance,
               ses,
               upper_ses = NULL,
               lower_ses = NULL,
               strategy = c("wadc", "at_mean")){
        
        
        t = prep_table(data = data, 
                       reg = reg,
                       performance = performance,
                       ses = ses,
                       upper_ses = upper_ses,
                       lower_ses = lower_ses,
                       strategy = strategy)
        
        
        ine_internal_function(t = t, 
                              ses = "ses", 
                              performance = "performance", 
                              choice = "choice",
                              upper_ses = upper_ses,
                              lower_ses = lower_ses)        
        
}


#data = d_aggregIncome
#reg  = reg_IncomeDecile
#ses  = "income_D"
#performance = "performance"
#choice = "choice" 
#strategy = "wadc"
#parallel = T
#cl = 4
        
        
pairwise_total_ine = function(data, 
                              reg, 
                              ses, 
                              performance, 
                              choice = NULL, 
                              strategy = c("wadc", "at_mean"),
                              parallel = T,
                              cl = 4){
        
        data[["ses"]]         = data[[ses]]
        data[["performance"]] = data[[performance]]
        
        if(is.null(choice)){
                data$choice = "tmp"
        }else{
                data[["choice"]] = data[[choice]]
        }
        
        UL = expand.grid(U = unique(data$ses),
                         L = unique(data$ses)) %>%
                as_tibble() %>%
                mutate(U_numeric = as.numeric(factor(U, 
                                                     levels = sort(unique(U)),
                                                     ordered = T)),
                       L_numeric = as.numeric(factor(L, 
                                                     levels = sort(unique(L)),
                                                     ordered = T))) %>%
                filter(U_numeric > L_numeric) %>%
                arrange(U_numeric, L_numeric) %>%
                select(-U_numeric, -L_numeric)
        
        
        #i = 1
        pairwise_calc = function(i, 
                                 UL,
                                 data,
                                 reg,
                                 ses,
                                 performance,
                                 choice,
                                 strategy){
                
                beta_matrix = coef(reg)
                ref_cat_name = reg$lab[1]
                
                U_i = UL$U[i]
                L_i = UL$L[i]
                
                if(strategy == "wadc"){
                        list_X_w <- get_X_w_wgtAverageDiscreteChange(data = data,
                                                                     reg  = reg,
                                                                     performance = performance,
                                                                     ses = ses, 
                                                                     upper_ses = U_i,
                                                                     lower_ses = L_i)
                }
                
                if(strategy == "at_mean"){
                        list_X_w <- get_X_w_atMean(data = data, 
                                                   reg  = reg,
                                                   performance = performance,
                                                   ses = ses, 
                                                   upper_ses = U_i,
                                                   lower_ses = L_i)
                }
                
                t = get_table_for_ine(list_X_w = list_X_w, 
                                      beta_matrix = beta_matrix, 
                                      ref_cat_name = ref_cat_name)
                
                ine_result = ine_internal_function(t = t, 
                                                   ses = "ses", 
                                                   performance = "performance", 
                                                   choice = "choice", 
                                                   upper_ses = NULL, 
                                                   lower_ses = NULL) 
                
                ine_result[[1]]$U = U_i
                ine_result[[1]]$L = L_i
                
                ine_result[[2]]$U = U_i
                ine_result[[2]]$L = L_i
                
                ine_result
        }
        
        
        
        if(parallel == F){
                pairwise_result = map(.x = 1:nrow(UL), 
                                      .f = \(i){
                                              pairwise_calc(i           = i, 
                                                            UL          = UL,
                                                            data        = data,
                                                            reg         = reg,
                                                            ses         = ses,
                                                            performance = performance,
                                                            choice      = choice,
                                                            strategy    = strategy)
                                      })
                
                pairwise_result_aggregated <- do.call(bind_rows, 
                                             lapply(1:length(pairwise_result), \(i){
                                                     x = pairwise_result[[i]]
                                                     tmp =  x$result_aggregated
                                                     tmp
                                             }))
                
                pairwise_result_disaggregated <- do.call(bind_rows, 
                                                lapply(1:length(pairwise_result), \(i){
                                                        x = pairwise_result[[i]]
                                                        tmp =  x$result_disaggregated
                                                        
                                                        tmp
                                                }))
                
        }else{
                library(foreach)
                library(doParallel)
                doParallel::registerDoParallel(cl)
                pairwise_result = foreach::foreach(i = 1:nrow(UL), 
                                                   .combine = c, 
                                                   .verbose = T,
                                                   .packages = c("dplyr", "tidyr", "purrr", "tibble", "stringr", "rlang", "nnet","Hmisc"), 
                                                   .export = c("wtd.mode", "get_table_for_ine", "ine_internal_function", "ipf_set_margins",
                                                               "get_X_w_atMean", "get_X_w_wgtAverageDiscreteChange", "get_multinom_pred",
                                                               "data",
                                                               "reg",
                                                               "ses",
                                                               "performance",
                                                               "choice",
                                                               "strategy",
                                                               "UL")
                                                   ) %dopar% {
                                                                       
                                                                       pairwise_calc(i           = i, 
                                                                                     UL          = UL,
                                                                                     data        = data,
                                                                                     reg         = reg,
                                                                                     ses         = ses,
                                                                                     performance = performance,
                                                                                     choice      = choice,
                                                                                     strategy    = strategy)
                                                                       
                                                               }
                doParallel::stopImplicitCluster()
                
                pairwise_result_aggregated = do.call(bind_rows,
                                                     pairwise_result[which(names(pairwise_result) == "result_aggregated")])
                
                pairwise_result_disaggregated = do.call(bind_rows,
                                                     pairwise_result[which(names(pairwise_result) == "result_disaggregated")])
                
        }
        
        
        pairwise_ine = list(result_aggregated    = pairwise_result_aggregated,
                            result_disaggregated = pairwise_result_disaggregated)
        
        
        total_ine_aggregated = pairwise_result_aggregated %>%
                group_by_at(c(choice)) %>%
                summarise_if(is.numeric, sum) %>%
                ungroup()
        
        total_ine_dissagregated = pairwise_result_disaggregated %>%
                group_by_at(c(choice, performance)) %>%
                summarise_if(is.numeric, sum) %>%
                ungroup()
                
        total_ine = list(result_aggregated    = total_ine_aggregated,
                         result_disaggregated = total_ine_dissagregated)
        
        list(total_ine    = total_ine,
             pairwise_ine = pairwise_ine)
}



#data     = d_aggregIncome_ContrEdupar
#reg      = reg_null
#contrast = list(income_D = c("10D", "1D"))
#weight   = "n"

average_discrete_changes = function(data,
                                    reg,
                                    contrast,
                                    weight,
                                    nsim = 2000,
                                    parallel = T,
                                    cl =4){
        
        data <- data %>%
                group_by_at(all.vars(formula(reg)[-1])) %>%
                summarise(n = sum(!!rlang::sym(weight)), .groups = "drop")
                
        data_U = data
        data_L = data
        
        w = data[[weight]]/sum(data[[weight]])
        
        data_U[[names(contrast)[1]]] <- contrast[[1]][1]
        data_L[[names(contrast)[1]]] <- contrast[[1]][2]
        
        terms <- delete.response(reg$terms)
        
        X_U <- model.matrix(terms, 
                            data = data_U, 
                            na.action = na.omit, 
                            xlev = reg$xlevels)
        
        X_L <- model.matrix(terms, 
                            data = data_L, 
                            na.action = na.omit, 
                            xlev = reg$xlevels)
        
        
        beta_list = simulate_beta(reg = reg, nsim = nsim)
        ref_cat_name = reg$lab[1]
        
        #i = 1
        sim = function(i){
                beta_matrix_i = get_sim_betaMatrix_i(beta_list, i)
                
                P_U = get_multinom_pred(X = X_U,
                                        beta_matrix  = beta_matrix_i,
                                        ref_cat_name = ref_cat_name)
                
                P_L = get_multinom_pred(X = X_L,
                                        beta_matrix  = beta_matrix_i,
                                        ref_cat_name = ref_cat_name)
                
                tibble(cat      = colnames(P_U),
                       avg.Pu   = colSums(P_U*w),
                       avg.Pl   = colSums(P_L*w),
                       avg.diff = colSums(w*(P_U - P_L)),
                       sim      = i)
        }
        
        #nsim = 4
        
        if(parallel == F){
                sims = lapply(1:nsim, \(i) sim(i))
                
                sims_df <- do.call(bind_rows, sims)
                
        }else{
                library(foreach)
                library(doParallel)
                doParallel::registerDoParallel(cl)
                sims_df = foreach::foreach(i= 1:nsim, .combine=rbind, .verbose = T, .inorder = F,
                                           .packages = c("dplyr", "tidyr", "purrr", "tibble", "stringr"), 
                                           .export = c("get_sim_betaMatrix_i", "get_multinom_pred")) %dopar% {
                                                   
                                                   sim(i) 
                                                   
                                           }
                doParallel::stopImplicitCluster()
                
                
        }
        
        sims_df
        
}