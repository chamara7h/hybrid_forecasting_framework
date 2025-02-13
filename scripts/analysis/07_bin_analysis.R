## stock_out analysis


library(tidyverse)
library(tsibble)
library(ggridges)


# read data

point_pred <- read_rds('data/predictions/bs_predictions_all_point.rds') %>% 
  
  drop_na()


stock_tidy <- read_rds('data/tidy_data/stock_tidy_so.rds') %>% 
  
  as_tsibble(index = yearmonth, key = id)


point_accuracy <- read_rds('data/results/fc_accuray.rds') %>% 
  
  filter(!is.na(mase)) %>% 
  
  filter(model != 'ensemble_ml_bs' , model != 'ensemble_demo_bs' , model != 'mlr_all' , model != 'lgbm_all' , model != 'xgb_all' , model != 'rf_all' ,
         model != 'mlr_filtered' , model != 'lgbm_filtered' , model != 'xgb_filtered' , model != 'rf_filtered' ,
         model != 'mlr_demo' , model != 'lgbm_demo' , model != 'lgbm_demo' , model != 'rf_demo' , model != 'xgb_demo',
         model != 'hybrid_pred_bs' , model != 'hybrid_optimise_bs' , model != 'hybrid_pred_sk' ,
         model != 'lgbm_bs' , model != 'xgb_bs' , model != 'rf_bs' , model != 'mlr_bs', model != 'demographic_prob',
         model != 'hybrid_scaled', model != 'lgbm_bottom_up', model != 'lgbm_min_trace_method_ols', model != 'lgbm_bottom_up_all',
         model != 'lgbm_min_trace_method_ols_all', model != 'lgbm_min_trace_method_mint_shrink_all',
         model != 'ensemble_uni') %>% 
  
  mutate(model = case_when(model == 'ensemble_ml_sk' ~ 'ensemble_ml',
                           model == 'hybrid_optimise_sk' ~ 'hybrid_optimise', 
                           model == 'lgbm_sk' ~ 'lgbm', 
                           model == 'xgb_sk' ~ 'xgb', 
                           model == 'rf_sk' ~ 'rf', 
                           model == 'mlr_sk' ~ 'mlr',
                           model == 'demographic_point' ~ 'demographic',
                           .default = model)) %>%  
  
  mutate(model = factor(model, levels = c("snaive", "moving_average", "ets", "arima", "croston", "demographic",
                                          'cs_1', 'cs_2', 'cs_3', 'cs_4', 'cs_5', 'cs_6', 'cs_7', 'cs_8', 'cs_9', 'cs_10',
                                          "mlr", "lgbm", "xgb", "rf", "ensemble_ml", "hybrid_optimise")))


point_accuracy_annual <- read_rds('data/results/point_accuracy_annual.rds')  %>% 
  
  filter(!is.na(mase)) %>% 
  
  filter(model != 'ensemble_ml_bs' , model != 'ensemble_demo_bs' , model != 'mlr_all' , model != 'lgbm_all' , model != 'xgb_all' , model != 'rf_all' ,
         model != 'mlr_filtered' , model != 'lgbm_filtered' , model != 'xgb_filtered' , model != 'rf_filtered' ,
         model != 'mlr_demo' , model != 'lgbm_demo' , model != 'lgbm_demo' , model != 'rf_demo' , model != 'xgb_demo',
         model != 'hybrid_pred_bs' , model != 'hybrid_optimise_bs' , model != 'hybrid_pred_sk' ,
         model != 'lgbm_bs' , model != 'xgb_bs' , model != 'rf_bs' , model != 'mlr_bs', model != 'demographic_prob',
         model != 'hybrid_scaled', model != 'lgbm_bottom_up', model != 'lgbm_min_trace_method_ols', model != 'lgbm_bottom_up_all',
         model != 'lgbm_min_trace_method_ols_all', model != 'lgbm_min_trace_method_mint_shrink_all',
         model != 'ensemble_uni') %>% 
  
  mutate(model = case_when(model == 'ensemble_ml_sk' ~ 'ensemble_ml',
                           model == 'hybrid_optimise_sk' ~ 'hybrid_optimise', 
                           model == 'lgbm_sk' ~ 'lgbm', 
                           model == 'xgb_sk' ~ 'xgb', 
                           model == 'rf_sk' ~ 'rf', 
                           model == 'mlr_sk' ~ 'mlr',
                           model == 'demographic_point' ~ 'demographic',
                           .default = model)) %>%  
  
  mutate(model = factor(model, levels = c("snaive", "moving_average", "ets", "arima", "croston", "demographic",
                                          'cs_1', 'cs_2', 'cs_3', 'cs_4', 'cs_5', 'cs_6', 'cs_7', 'cs_8', 'cs_9', 'cs_10',
                                          "mlr", "lgbm", "xgb", "rf", "ensemble_ml", "hybrid_optimise")))


crps <- read_rds('data/results/bs_crps.rds') %>% 
  
  filter(model != 'ensemble_ml_bs' , model != 'ensemble_demo_bs' , model != 'hybrid_bs' , 
         model != 'lgbm_bs' , model != 'xgb_bs' , model != 'rf_bs' , model != 'mlr_bs' , model != 'demographic_prob',
         model != 'ensemble_uni') %>% 
  
  mutate(model = case_when(model == 'ensemble_ml_sk' ~ 'ensemble_ml',
                           model == 'hybrid_sk' ~ 'hybrid_optimise', 
                           model == 'lgbm_sk' ~ 'lgbm', 
                           model == 'xgb_sk' ~ 'xgb', 
                           model == 'rf_sk' ~ 'rf', 
                           model == 'mlr_sk' ~ 'mlr',
                           .default = model)) %>% 
  
  mutate(model = factor(model, levels = c("snaive", "moving_average", "ets", "arima",
                                          "mlr", "lgbm", "xgb", "rf", "ensemble_ml", "hybrid_optimise")))


crps_annual <- read_rds('data/results/prob_crps_annual.rds') %>% 
  
  filter(model != 'ensemble_ml_bs' , model != 'ensemble_demo_bs' , model != 'hybrid_bs' , 
         model != 'lgbm_bs' , model != 'xgb_bs' , model != 'rf_bs' , model != 'mlr_bs' , model != 'demographic_prob',
         model != 'ensemble_uni') %>% 
  
  mutate(model = case_when(model == 'ensemble_ml_sk' ~ 'ensemble_ml',
                           model == 'hybrid_sk' ~ 'hybrid_optimise', 
                           model == 'lgbm_sk' ~ 'lgbm', 
                           model == 'xgb_sk' ~ 'xgb', 
                           model == 'rf_sk' ~ 'rf', 
                           model == 'mlr_sk' ~ 'mlr',
                           .default = model)) %>% 
  
  mutate(model = factor(model, levels = c("snaive", "moving_average", "ets", "arima",
                                          "mlr", "lgbm", "xgb", "rf", "ensemble_ml", "hybrid_optimise")))



## merge two data frames

point_pred_tidy <- point_pred %>% 
  
  left_join(stock_tidy, by = c('id', 'yearmonth')) %>% 
  
  mutate(absolute_error = abs(stock_distributed - prediction),
         
         absolute_error_scaled = (absolute_error - min(absolute_error)) / (max(absolute_error) - min(absolute_error)))
         

## filterout stockout days related predictions

point_pred_tidy_filter <- point_pred_tidy %>% 
  
  filter(cumulative_stockout_days == 0) %>% 
  
  mutate(absolute_error_scaled_bin = cut(absolute_error_scaled, breaks = seq(0, 1, by = 0.2), labels = 1:5, include.lowest = TRUE)) # make bins for error distributions


## count the number of error bins

error_bin_count <- point_pred_tidy_filter %>% 
  
  group_by(model, absolute_error_scaled_bin) %>% 
  
  summarise(bin_count = n(), .groups = 'drop')


# create bins for MASE

mase_bin <- point_accuracy %>% 
  
  mutate(mase_bin = cut(mase,
                        breaks = c(-Inf, 1, 2, 5, 10, Inf), 
                        labels = c("0-1", "1-2", "2-5", "5-10", ">10"),
                        right = FALSE))


mase_bin_count <- mase_bin %>% 
  
  group_by(model, mase_bin) %>%
  
  summarise(mase_bin_count = n(), .groups = 'drop') %>% 
  
  group_by(model) %>% 
  
  mutate(mase_bin_prob = round(mase_bin_count/sum(mase_bin_count), 4))


mase_bin_annual <- point_accuracy_annual %>% 
  
  mutate(mase_bin = cut(mase,
                        breaks = c(-Inf, 1, 2, 5, 10, Inf), 
                        labels = c("0-1", "1-2", "2-5", "5-10", ">10"),
                        right = FALSE))


mase_bin_count_annual <- mase_bin_annual %>% 
  
  group_by(model, mase_bin) %>%
  
  summarise(mase_bin_count = n(), .groups = 'drop') %>% 
  
  group_by(model) %>% 
  
  mutate(mase_bin_prob = round(mase_bin_count/sum(mase_bin_count), 4))


#### plot errors


mase_bin_count %>% 
  
  ggplot(aes(x = model, y = mase_bin_prob)) +
  
  geom_col(aes(fill = mase_bin)) +
  
  labs(x = "Model",
       y = "Probability") +
  
  hrbrthemes::theme_ipsum() +
  # ggthemes::theme_few() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggthemes::scale_color_colorblind() +
  
  scale_fill_discrete(name = "MASE value distribution in bins")


mase_bin_count_annual %>% 
  
  ggplot(aes(x = model, y = mase_bin_prob)) +
  
  geom_col(aes(fill = mase_bin)) +
  
  labs(x = "Model",
       y = "Probability") +
  
  hrbrthemes::theme_ipsum() +
  # ggthemes::theme_few() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggthemes::scale_color_colorblind() +
  
  scale_fill_discrete(name = "MASE value distribution in bins")
  
 



# create bins for CRPS

crps_bin <- crps %>% 
  
  mutate(crps_bin = cut(crps,
                        breaks = c(-Inf, 5, 10, 20, 50, Inf), 
                        labels = c("0-5", "10-20", "20-50", "50-100", ">100"),
                        right = FALSE))


crps_bin_count <- crps_bin %>% 
  
  group_by(model, crps_bin) %>%
  
  summarise(crps_bin_count = n(), .groups = 'drop') %>% 
  
  group_by(model) %>% 
  
  mutate(crps_bin_prob = round(crps_bin_count/sum(crps_bin_count), 4))


crps_bin_annual <- crps_annual %>% 
  
  mutate(crps_bin = cut(crps,
                        breaks = c(-Inf, 5, 10, 20, 50, Inf), 
                        labels = c("0-5", "10-20", "20-50", "50-100", ">100"),
                        right = FALSE))


crps_bin_count_annual <- crps_bin_annual %>% 
  
  group_by(model, crps_bin) %>%
  
  summarise(crps_bin_count = n(), .groups = 'drop') %>% 
  
  group_by(model) %>% 
  
  mutate(crps_bin_prob = round(crps_bin_count/sum(crps_bin_count), 4))


#### plot errors


crps_bin_count %>% 
  
  ggplot(aes(x = model, y = crps_bin_prob)) +
  
  geom_col(aes(fill = crps_bin)) +
  
  labs(x = "Model",
       y = "Probability") +
  
  hrbrthemes::theme_ipsum() +
  # ggthemes::theme_few() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggthemes::scale_color_colorblind() +
  
  scale_fill_discrete(name = "CRPS value distribution in bins")


crps_bin_count_annual %>% 
  
  ggplot(aes(x = model, y = crps_bin_prob)) +
  
  geom_col(aes(fill = crps_bin)) +
  
  labs(x = "Model",
       y = "Probability") +
  
  hrbrthemes::theme_ipsum() +
  # ggthemes::theme_few() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggthemes::scale_color_colorblind() +
  
  scale_fill_discrete(name = "CRPS value distribution in bins")













