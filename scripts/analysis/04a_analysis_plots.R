## Nonparametric multiple comparisons (Nemenyi test)

if(!require('tsutils')) {
  install.packages('tsutils')
  library('tsutils')
}

library(tidyverse)
library(ggridges)

# read data

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


point_accuracy_annual_product_summary <- point_accuracy_annual %>% 
  
  filter(!is.na(mase), !is.na(rmse)) %>% 
  
  group_by(model) %>% 
  
  summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmse = mean(rmse), median_rmse = median(rmse))




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



## box plots of error metrics

# plot error distribution - Point

point_accuracy %>% 
  
  ggplot(aes(as.factor(model), mase))+
  
  geom_boxplot(outliers = FALSE) +
  
  labs(x = "Model", y = "MASE") +
  
  # ggthemes::scale_color_colorblind() +
  
  ggthemes::theme_few() +
  
  theme(axis.text.x = element_text(angle=45, hjust=1))



point_accuracy %>% 
  
  filter(mase < 10) %>% 
  
  
  ggplot(aes(y = model, x = mase, fill = model))+
  
  geom_density_ridges(rel_min_height = 0.005, alpha=0.6, scale = 2.5) +
  theme_ridges() + 
  theme(legend.position = "none")


# plot error distribution - Point - annual

point_accuracy_annual %>% 
  
  ggplot(aes(as.factor(model), mase))+
  
  geom_boxplot(outliers = FALSE) +
  
  labs(x = "Model", y = "MASE") +
  
  # ggthemes::scale_color_colorblind() +
  
  ggthemes::theme_few() +
  
  theme(axis.text.x = element_text(angle=45, hjust=1))


point_accuracy_annual %>% 
  
  filter(mase < 10) %>%
  
  
  ggplot(aes(y = model, x = mase, fill = model))+
  
  geom_density_ridges(rel_min_height = 0.005, alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")


## scatter plot

point_accuracy_annual %>% 
  
  # filter(model %in% c("ensemble_ml_sk", "hybrid_optimise_sk", "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk")) %>%
  
  filter(model %in% c("hybrid_optimise_sk", "rf_sk")) %>%
  
  filter(stock_distributed > 0, prediction > 0) %>% 
  
  filter(mase < 5) %>% 
  
  ggplot(aes(x=stock_distributed, y=mase, shape=model, color=model)) + 
  geom_point() +
  ggthemes::scale_color_colorblind() +
  hrbrthemes::theme_ipsum()



point_accuracy_annual %>% 
  
  # filter(model %in% c("ensemble_ml_sk", "hybrid_optimise_sk", "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk")) %>%
  
  filter(model %in% c("hybrid_optimise_sk", "rf_sk")) %>%
  
  filter(stock_distributed > 0, prediction > 0) %>% 
  
  mutate(across(c(stock_distributed, prediction, mase, rmse), ~ (. - min(.)) / (max(.) - min(.)))) %>% 
  
  filter(stock_distributed < 0.2) %>% 
  
  ggplot(aes(x=stock_distributed, y=mase, shape=model, color=model)) + 
  geom_point() +
  ggthemes::scale_color_colorblind() +
  hrbrthemes::theme_ipsum()


point_accuracy_annual %>% 
  
  # filter(model %in% c("ensemble_ml_sk", "hybrid_optimise_sk", "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk")) %>%
  
  filter(model %in% c("hybrid_optimise_sk", "rf_sk")) %>%
  
  filter(stock_distributed > 0, prediction > 0) %>% 
  
  # mutate(across(c(stock_distributed, prediction, mase, rmse), ~ (. - min(.)) / (max(.) - min(.)))) %>% 
  
  # filter(stock_distributed < 0.2) %>% 
  
  ggplot(aes(x=stock_distributed, y=prediction, color=model)) + 
  geom_point() +
  ggthemes::scale_color_colorblind() +
  hrbrthemes::theme_ipsum()






## prob plots

crps %>% 
  
  ggplot(aes(model, crps))+
  
  geom_boxplot(outliers = FALSE) +
  
  labs(x = "Model", y = "CRPS") +
  
  # ggthemes::scale_color_colorblind() +
  
  ggthemes::theme_few() +
  
  theme(axis.text.x = element_text(angle=45, hjust=1))



crps %>% 
  
  filter(crps < 80) %>%
  
  
  ggplot(aes(y = model, x = crps, fill = model))+
  
  geom_density_ridges(rel_min_height = 0.005, alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")


## prob plots - annual

crps_annual %>% 
  
  ggplot(aes(model, crps))+
  
  geom_boxplot(outliers = FALSE) +
  
  labs(x = "Model", y = "CRPS") +
  
  # ggthemes::scale_color_colorblind() +
  
  ggthemes::theme_few() +
  
  theme(axis.text.x = element_text(angle=45, hjust=1))



crps_annual %>% 
  
  filter(crps < 100) %>%
  
  
  ggplot(aes(y = model, x = crps, fill = model))+
  
  geom_density_ridges(rel_min_height = 0.005, alpha=0.6) +
  theme_ridges() + 
  theme(legend.position = "none")


crps_annual %>% 
  
  # filter(model %in% c("ensemble_ml_sk", "hybrid_optimise_sk", "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk")) %>%
  
  filter(model %in% c("hybrid_sk", "rf_sk")) %>%
  
  mutate(across(c(stock_distributed, crps), ~ (. - min(.)) / (max(.) - min(.)))) %>% 
  
  # filter(stock_distributed < 0.2) %>% 
  
  ggplot(aes(x=stock_distributed, y=crps, shape=model, color=model)) + 
  geom_point() +
  ggthemes::scale_color_colorblind() +
  hrbrthemes::theme_ipsum()



# accuracy summary

point_accuracy <- point_accuracy %>% 
  
  mutate(model = factor(model, levels = c('cs_1', 'cs_2', 'cs_3', 'cs_4', 'cs_5', 'cs_6', 'cs_7', 'cs_8', 'cs_9', 'cs_10',
                                          "snaive", "moving_average", "ets", "arima", "ensemble_uni", "demographic_prob",
                                          "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk", "ensemble_ml_sk", "hybrid_optimise_sk"))) 


fc_accuray_summary <- point_accuracy %>% 
  
  group_by(model) %>% 
  
  summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmse = mean(rmse), median_rmse = median(rmse), .groups = 'drop')


point_accuray_summary_qnt <- point_accuracy_annual %>% 
  
  mutate(model = factor(model, levels = c('cs_1', 'cs_2', 'cs_3', 'cs_4', 'cs_5', 'cs_6', 'cs_7', 'cs_8', 'cs_9', 'cs_10',
                                          "snaive", "moving_average", "ets", "arima", "ensemble_uni", "demographic_point",
                                          "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk", "ensemble_ml_sk", "hybrid_optimise_sk"))) %>% 
  
  group_by(model) %>% 
  
  summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmse = mean(rmse), median_rmse = median(rmse), .groups = 'drop')



crps <- crps %>% 
  
  mutate(model = factor(model, levels = c("snaive", "moving_average", "ets", "arima", "ensemble_uni",
                                                "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk", "ensemble_ml_sk", "hybrid_sk"))) 


prob_accuray_summary <- crps %>% 
  
  group_by(model) %>% 
  
  summarise(mean_crps = mean(crps), median_crps = median(crps), .groups = 'drop')



prob_accuray_ANNUAL_summary <- crps_annual %>% 
  
  mutate(model = factor(model, levels = c("snaive", "moving_average", "ets", "arima", "ensemble_uni",
                                          "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk", "ensemble_ml_sk", "hybrid_sk"))) %>% 
  
  group_by(model) %>% 
  
  summarise(mean_crps = mean(crps), median_crps = median(crps), .groups = 'drop')





# qnt_accuray_summary <- pinball %>% 
#   
#   group_by(model) %>% 
#   
#   summarise(mean_pinball_loss = mean(pinball_loss), median_pin_ball_loss = median(pinball_loss))


## prepare mase matrix

mase_tidy <- point_accuracy %>%  
  
  select(-rmse) %>% 
  
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'mase') %>% 
  
  select(-id) %>% 
  
  drop_na()


# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)

nemenyi(mase_tidy,conf.level=0.95,plottype="vmcb")


## prepare rmse matrix

rmse_tidy <- point_accuracy %>%  
  
  select(-mase) %>% 
  
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'rmse') %>% 
  
  select(-id) %>% 
  
  drop_na()


# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)

nemenyi(rmse_tidy,conf.level=0.95,plottype="vmcb")


## prepare mase matrix

mase_tidy_annual <- point_accuracy_annual %>%  
  
  select(-rmse) %>% 
  
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'mase') %>% 
  
  select(-id) %>% 
  
  drop_na()


# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)

nemenyi(mase_tidy_annual,conf.level=0.95,plottype="vmcb")


## prepare rmse matrix

rmse_tidy_annual <- point_accuracy_annual %>%  
  
  select(-mase) %>% 
  
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'rmse') %>% 
  
  select(-id) %>% 
  
  drop_na()


# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)

nemenyi(rmse_tidy_annual,conf.level=0.95,plottype="vmcb")



## prepare crps matrix

crps_tidy <- crps %>%  
  
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'crps') %>% 
  
  select(-id)

nemenyi(crps_tidy,conf.level=0.95,plottype="vmcb")


## prepare crps matrix

crps_tidy_annual <- crps_annual %>%  
  
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'crps') %>% 
  
  select(-id)

nemenyi(crps_tidy_annual,conf.level=0.95,plottype="vmcb")


## prepare pinball matrix

# pinball_tidy <- pinball %>%  
#   
#   pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'pinball_loss') %>% 
#   
#   select(-id)
# 
# nemenyi(pinball_tidy,conf.level=0.95,plottype="vmcb")


## pair wise test

## point predictions - mase

kruskal.test(mase ~ model, data = point_accuracy)

point_test_mase <- pairwise.wilcox.test(point_accuracy$mase, point_accuracy$model, p.adjust.method = "bonferroni")

point_test_table_mase <- point_test_mase$p.value


## point predictions - rmse

kruskal.test(rmse ~ model, data = point_accuracy)

point_test_rmse <- pairwise.wilcox.test(point_accuracy$rmse, point_accuracy$model, p.adjust.method = "bonferroni")

point_test_table_rmse <- point_test_rmse$p.value


## pair wise test

## prob predictions - CRPS

kruskal.test(crps ~ model, data = crps)

prob_test_crps <- pairwise.wilcox.test(crps$crps, crps$model, p.adjust.method = "bonferroni")

prob_test_table_crps <-prob_test_crps$p.value
