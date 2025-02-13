# accuracy plots and statistical test

library(tidyverse)
library(fpp3)
library(ggridges)
library(tsutils)
library(colorspace)


# read data ---------------------------------------------------------------

# read data

stock <- read.csv('data/tidy_data/stock_tsb_zero_remove.csv') 

# make the tsibble

stock_tsb <- stock %>% 
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  as_tsibble(index = yearmonth, key = c(region, district, site_code, product_type, product_code))


# creating training and test set

stock_tsb_train <- stock_tsb %>% 
  filter_index(. ~ '2019 Sep') 

stock_tsb_test <- stock_tsb %>% 
  filter_index('2019 Aug' ~ .) 


# create tscv train data

f_horizon = 3

stock_train_tscv <- stock_tsb_train %>% 
  stretch_tsibble(.init = (length(unique(stock_tsb_train$yearmonth)) - f_horizon + 1), .step = 1) %>% 
  rename(horizon = .id) |> 
  as_tibble() |> 
  # group_by(id, origin) |> 
  # mutate(horizon = row_number()) |> # creating the horizon
  # ungroup() |>
  mutate(unique_id = paste0(id, '-', horizon))


point_accuracy <- read_rds('data/results/fc_cv_accuray.rds') %>% 
  filter(!is.na(mase)) %>% 
  filter(model != 'hybrid_bias_series') %>% 
  mutate(model = case_when(model == 'hybrid_bias_fh' ~ 'hybrid_bias_adjustment',
                           model == 'hybrid_old_fh' ~ 'hybrid_weighted_averaging',
                           .default = model)) %>% 
  separate(id, into = c('id', 'horizon'), sep = '-')


prob_accuracy <- read_rds('data/results/bs_cv_crps.rds') %>% 
  filter(!is.na(crps)) %>% 
  filter(model != 'hybrid_bias_series') %>% 
  mutate(model = case_when(model == 'hybrid_bias_fh' ~ 'hybrid_bias_adjustment',
                           model == 'hybrid_old_fh' ~ 'hybrid_weighted_averaging',
                           .default = model)) %>% 
  separate(id, into = c('id', 'horizon'), sep = '-')


# accuracy summaries ------------------------------------------------------


point_acc_summary <- point_accuracy %>% 
  group_by(model, horizon) %>% 
  summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmse = mean(rmse), median_rmse = median(rmse), .groups = 'drop')


prob_acc_summary <- prob_accuracy %>% 
  group_by(model, horizon) %>% 
  summarise(mean_crps = mean(crps), median_crps = median(crps), .groups = 'drop')


# nemenyi statistical test ------------------------------------------------

# significant test for mase - Horizon 1

mase_tidy_h1 <- point_accuracy %>% 
  filter(horizon == 1) %>% 
  select(-rmse, -horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'mase') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(mase_tidy_h1,conf.level=0.95,plottype="vmcb")


# significant test for mase - Horizon 2

mase_tidy_h2 <- point_accuracy %>% 
  filter(horizon == 2) %>% 
  select(-rmse, -horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'mase') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(mase_tidy_h2,conf.level=0.95,plottype="vmcb")


# significant test for mase - Horizon 3

mase_tidy_h3 <- point_accuracy %>% 
  filter(horizon == 3) %>% 
  select(-rmse, -horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'mase') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(mase_tidy_h3,conf.level=0.95,plottype="vmcb")


# significant test for rmse - Horizon 1

rmse_tidy_h1 <- point_accuracy %>% 
  filter(horizon == 1) %>% 
  select(-mase, -horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'rmse') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(rmse_tidy_h1,conf.level=0.95,plottype="vmcb")


# significant test for rmse - Horizon 2

rmse_tidy_h2 <- point_accuracy %>% 
  filter(horizon == 2) %>% 
  select(-mase, -horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'rmse') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(rmse_tidy_h2,conf.level=0.95,plottype="vmcb")


# significant test for rmse - Horizon 3

rmse_tidy_h3 <- point_accuracy %>% 
  filter(horizon == 3) %>% 
  select(-mase, -horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'rmse') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(rmse_tidy_h3,conf.level=0.95,plottype="vmcb")


# significant test for crps - Horizon 1

crps_tidy_h1 <- prob_accuracy %>% 
  filter(horizon == 1) %>% 
  select(-horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'crps') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(crps_tidy_h1,conf.level=0.95,plottype="vmcb")


# significant test for rmse - Horizon 2

crps_tidy_h2 <- prob_accuracy %>% 
  filter(horizon == 2) %>% 
  select(-horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'crps') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(crps_tidy_h2,conf.level=0.95,plottype="vmcb")


# significant test for rmse - Horizon 3

crps_tidy_h3 <- prob_accuracy %>% 
  filter(horizon == 3) %>% 
  select(-horizon) %>%
  pivot_wider(id_cols = 'id', names_from = 'model', values_from = 'crps') %>% 
  select(-id)
# Adjust plot margins
par(mar = c(5, 5, 3, 1))  # Set the margins (bottom, left, top, right)
nemenyi(crps_tidy_h3,conf.level=0.95,plottype="vmcb")


# density plots for errors ------------------------------------------------

# define horizon level as factors

point_accuracy <- point_accuracy %>% 
  mutate(horizon = factor(horizon, levels = sort(unique(horizon))))


# List of specific models to highlight

filtered_models <- c('demographic', 'hybrid_weighted_averaging', 'hybrid_bias_adjustment', 'ml_ensemble', 'lgbm', 'rf', 'xgb')

# mase distribution over the horizon

# point_accuracy %>% 
#   filter(mase < 25) %>%
#   ggplot(aes(y = model, x = mase, fill = model))+
#   geom_density_ridges(rel_min_height = 0.05) +
#   scale_y_discrete(expand = c(0.01, 0)) +
#   scale_x_continuous(expand = c(0.01, 0)) +
#   theme_ridges() + 
#   theme(legend.position = "none")


# Background density plot for all models in grey

background_plot_mase <- point_accuracy %>% 
  filter(!model %in% filtered_models, mase < 5) %>%  # No filtering of models, include all
  rename(Model = model) %>% 
  ggplot(aes(x = mase, group = Model)) +  # Use group to plot each model separately
  geom_density(colour = 'azure3', linewidth = 0.5)


# Main plot with specific models highlighted

main_plot_mase <- point_accuracy %>% 
  filter(mase < 5) %>% 
  mutate(model = ifelse(model %in% filtered_models, model, 'other_models'),
         model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'demographic' ~ 'Demographic',
                           model == 'other_models' ~ 'Other models'),
         model = factor(model, levels = c('LGBM', 'RF', 'XGB', 'ML ensemble', 'Hybrid weighted averaging', 'Hybrid bias adjustment', 'Demographic', 'Other models'))) %>%
  rename(Model = model) %>% 
  ggplot(aes(x = mase, colour = Model)) +
  geom_density()


# Combine the background and main plots

final_plot_mase <- background_plot_mase +
  geom_density(data = main_plot_mase$data, aes(x = mase, colour = Model, linewidth = Model)) +
  facet_grid(rows = vars(horizon), labeller = label_both) +  # Add "Forecast Horizon" label
  scale_color_manual(
    values = c('Demographic' =  '#d62728',
               'Hybrid bias adjustment' = '#D55E00',            
               'Hybrid weighted averaging' = '#0072B2',
               'LGBM' = '#E69F00',
               'RF' = '#009E73', 
               'XGB' = '#CC79A7',
               'ML ensemble' = '#F0E442',
               'Other models' = 'azure3')
  ) +
  scale_linewidth_manual(
    values = c('Demographic' = 1,
               'Hybrid bias adjustment' = 0.75,
               'Hybrid weighted averaging' = 1.25,
               'LGBM' = 0.75,
               'RF' = 1,
               'XGB' = 0.75,
               'ML ensemble' = 0.75,
               'Other models' = 0.5)
  ) +
  theme_minimal() +
  labs(
    # title = "MASE Density by Forecast Horizon and Model",
    x = "MASE",
    y = "Density",
    # color = "Model"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.key = element_blank(),  # Remove legend key background
    legend.key.size = unit(1.2, "lines")  # Adjust size for better visibility
  )

# Display the final plot
print(final_plot_mase)


# rmse distribution over the horizon

point_accuracy %>%
  filter(rmse < 25) %>%
  ggplot(aes(y = model, x = rmse, fill = model))+
  geom_density_ridges(rel_min_height = 0.05) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges() +
  theme(legend.position = "none")


# Background density plot for all models in grey

background_plot_rmse <- point_accuracy %>% 
  filter(!model %in% filtered_models, rmse < 50) %>%  # No filtering of models, include all
  rename(Model = model) %>% 
  ggplot(aes(x = rmse, group = Model)) +  # Use group to plot each model separately
  geom_density(colour = 'azure3', linewidth = 0.5)


# Main plot with specific models highlighted

main_plot_rmse <- point_accuracy %>% 
  filter(rmse < 50) %>% 
  mutate(model = ifelse(model %in% filtered_models, model, 'other_models'),
         model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'demographic' ~ 'Demographic',
                           model == 'other_models' ~ 'Other models'),
         model = factor(model, levels = c('LGBM', 'RF', 'XGB', 'ML ensemble', 'Hybrid weighted averaging', 'Hybrid bias adjustment', 'Demographic', 'Other models'))) %>%
  rename(Model = model) %>% 
  ggplot(aes(x = rmse, colour = Model)) +
  geom_density()


# Combine the background and main plots

final_plot_rmse <- background_plot_rmse +
  geom_density(data = main_plot_rmse$data, aes(x = rmse, colour = Model, linewidth = Model)) +
  facet_grid(rows = vars(horizon), labeller = label_both) +  # Add "Forecast Horizon" label
  scale_color_manual(
    values = c('Demographic' =  '#d62728',
               'Hybrid bias adjustment' = '#D55E00',            
               'Hybrid weighted averaging' = '#0072B2',
               'LGBM' = '#E69F00',
               'RF' = '#009E73', 
               'XGB' = '#CC79A7',
               'ML ensemble' = '#F0E442',
               'Other models' = 'azure3')
  ) + 
  scale_linewidth_manual(
    values = c('Demographic' = 1,
               'Hybrid bias adjustment' = 0.75,
               'Hybrid weighted averaging' = 1.25,
               'LGBM' = 0.75,
               'RF' = 1,
               'XGB' = 0.75,
               'ML ensemble' = 0.75,
               'Other models' = 0.5)
  ) +
  theme_minimal() +
  labs(
    # title = "rmse Density by Forecast Horizon and Model",
    x = "RMSE",
    y = "Density",
    # color = "Model"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.key = element_blank(),  # Remove legend key background
    legend.key.size = unit(1.2, "lines")  # Adjust size for better visibility
  )

# Display the final plot
print(final_plot_rmse)


# plot CRPS distribution

# define horizon level as factors

prob_accuracy <- prob_accuracy %>% 
  mutate(horizon = factor(horizon, levels = sort(unique(horizon))))


# List of specific models to highlight

filtered_models <- c('hybrid_weighted_averaging', 'hybrid_bias_adjustment', 'ml_ensemble', 'lgbm', 'rf', 'xgb')

# CRPS distribution over the horizon

prob_accuracy %>%
  filter(crps < 100) %>%
  ggplot(aes(y = model, x = crps, fill = model))+
  geom_density_ridges(rel_min_height = 0.05) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_ridges() +
  theme(legend.position = "none")


# Background density plot for all models in grey

background_plot_crps <- prob_accuracy %>% 
  filter(!model %in% filtered_models, crps < 25) %>%  # No filtering of models, include all
  rename(Model = model) %>% 
  ggplot(aes(x = crps, group = Model)) +  # Use group to plot each model separately
  geom_density(colour = 'azure3', linewidth = 0.5)


# Main plot with specific models highlighted

main_plot_crps <- prob_accuracy %>% 
  filter(crps < 25) %>% 
  mutate(model = ifelse(model %in% filtered_models, model, 'other_models'),
         model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'other_models' ~ 'Other models'),
         model = factor(model, levels = c('LGBM', 'RF', 'XGB', 'ML ensemble', 'Hybrid weighted averaging', 'Hybrid bias adjustment', 'Other models'))) %>%
  rename(Model = model) %>% 
  ggplot(aes(x = crps, colour = Model)) +
  geom_density()


# Combine the background and main plots

final_plot_crps <- background_plot_crps +
  geom_density(data = main_plot_crps$data, aes(x = crps, colour = Model, linewidth = Model)) +
  facet_grid(rows = vars(horizon), labeller = label_both) +  # Add "Forecast Horizon" label
  scale_color_manual(
    values = c('Hybrid bias adjustment' = '#D55E00',            
               'Hybrid weighted averaging' = '#0072B2',
               'LGBM' = '#E69F00',
               'RF' = '#009E73', 
               'XGB' = '#CC79A7',
               'ML ensemble' = '#F0E442',
               'Other models' = 'azure3')
  ) +
  scale_linewidth_manual(
    values = c('Hybrid bias adjustment' = 0.75,
               'Hybrid weighted averaging' = 1.25,
               'LGBM' = 0.75,
               'RF' = 1,
               'XGB' = 0.75,
               'ML ensemble' = 0.75,
               'Other models' = 0.5)
  ) +
  theme_minimal() +
  labs(
    # title = "CRPS Density by Forecast Horizon and Model",
    x = "CRPS",
    y = "Density",
    # color = "Model"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.key = element_blank(),  # Remove legend key background
    legend.key.size = unit(1.2, "lines")  # Adjust size for better visibility
  )

# Display the final plot
print(final_plot_crps)


# Error distributions using box plots -------------------------------------

# mase

boxplot_mase_h1 <- point_accuracy %>% 
  filter(horizon == 1) %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BSTS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           model == 'demographic' ~ 'Demographic',
                           .default = model)) %>%
  ggplot(aes(y = reorder(model, -mase, FUN = mean), mase))+
  geom_boxplot(outliers = FALSE) +
  # geom_violin(trim = TRUE) +
  labs(x = "Model", y = "MASE") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust=1))


box_plot_mase_h2 <- point_accuracy %>% 
  filter(horizon == 2) %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           model == 'demographic' ~ 'Demographic',
                           .default = model)) %>%
  ggplot(aes(y = reorder(model, -mase, FUN = mean), mase))+
  geom_boxplot(outliers = FALSE) +
  # geom_violin(trim = TRUE) +
  labs(x = "Model", y = "MASE") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust=1))


boxplot_mase_h3 <- point_accuracy %>% 
  filter(horizon == 3) %>%
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           model == 'demographic' ~ 'Demographic',
                           .default = model)) %>%
  ggplot(aes(y = reorder(model, -mase, FUN = mean), x = mase))+
  geom_boxplot(outliers = FALSE) +
  # geom_violin(trim = TRUE) +
  # facet_grid(rows = vars(horizon), labeller = label_both) +
  labs(x = "Model", y = "MASE") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust=1))


# rmse

boxplot_rmse_h1 <- point_accuracy %>% 
  filter(horizon == 1) %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           model == 'demographic' ~ 'Demographic',
                           .default = model)) %>%
  ggplot(aes(y = reorder(model, -rmse, FUN = mean), rmse))+
  geom_boxplot(outliers = FALSE) +
  # geom_violin(trim = TRUE) +
  labs(x = "Model", y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust=1))


box_plot_rmse_h2 <- point_accuracy %>% 
  filter(horizon == 2) %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           model == 'demographic' ~ 'Demographic',
                           .default = model)) %>%
  ggplot(aes(y = reorder(model, -rmse, FUN = mean), rmse))+
  geom_boxplot(outliers = FALSE) +
  # geom_violin(trim = TRUE) +
  labs(x = "Model", y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust=1))


boxplot_rmse_h3 <- point_accuracy %>% 
  filter(horizon == 3) %>%
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           model == 'demographic' ~ 'Demographic',
                           .default = model)) %>%
  ggplot(aes(y = reorder(model, -rmse, FUN = mean), x = rmse))+
  geom_boxplot(outliers = FALSE) +
  # geom_violin(trim = TRUE) +
  # facet_grid(rows = vars(horizon), labeller = label_both) +
  labs(x = "Model", y = "RMSE") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust=1))


# crps

boxplot_crps <- prob_accuracy %>% 
  # filter(horizon == 1) %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           .default = model)) %>%
  ggplot(aes(reorder(model, -crps, FUN = mean), crps))+
  geom_boxplot(outliers = FALSE) +
  # geom_violin(trim = TRUE) +
  facet_grid(rows = vars(horizon), labeller = label_both) +
  labs(x = "Model", y = "CRPS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))


# line plot for fc horizon ------------------------------------------------

# mase

# Define a custom dark contrast color palette
dark_colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", 
                 "#66a61e", "#e6ab02", "#a6761d", "#666666", 
                 "#1f78b4", "#6a3d9a", "#b2df8a", "#33a02c", 
                 "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", 
                 "#cab2d6", "#8a2be2", "#ff4500", "#daa520")

point_acc_summary %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           # model == 'demographic' ~ 'Demographic',
                           # model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           .default = 'other')) %>%
  filter(model != 'other') %>%
  group_by(model) %>%
  mutate(mean_mase_overall = mean(mean_mase, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(model = fct_reorder(model, mean_mase_overall, .desc = TRUE)) %>%
  ggplot(aes(x = horizon, y = mean_mase, group = model, colour = model)) +
  geom_line(lwd = 0.75) +
  geom_point() +
  scale_color_manual(values = dark_colors) +
  labs(x = "Horizon", y = "MASE", colour = 'Model') +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(hjust = 1))


# rmse

lineplot_rmse <- point_acc_summary %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'croston' ~ 'Croston',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           # model == 'demographic' ~ 'Demographic',
                           # model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           .default = 'other')) %>%
  filter(model != 'other') %>%
  group_by(model) %>%
  mutate(mean_rmse_overall = mean(mean_rmse, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(model = fct_reorder(model, mean_rmse_overall, .desc = TRUE)) %>%
  ggplot(aes(x = horizon, y = mean_rmse, group = model, colour = model)) +
  geom_line(lwd = 0.75) +
  geom_point() +
  scale_color_manual(values = qualitative_hcl(20, palette = "Dark 3")) +
  labs(x = "Horizon", y = "RMSE", colour = 'Model') +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


# CRPS

lineplot_crps <- prob_acc_summary %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           # model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           .default = 'other')) %>%
  filter(model != 'other') %>%
  group_by(model) %>%
  mutate(mean_crps_overall = mean(mean_crps, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(model = fct_reorder(model, mean_crps_overall, .desc = TRUE)) %>%
  ggplot(aes(x = horizon, y = mean_crps, group = model, colour = model)) +
  geom_line(lwd = 0.75) +
  geom_point() +
  scale_color_manual(values = qualitative_hcl(20, palette = "Dark 3")) +
  labs(x = "Horizon", y = "CRPS", colour = 'Model') +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))


# evaluate series features ------------------------------------------------

# heat map for rank distribution

# MASE

# calculate ranking based on mase

ranked_point_accuracy_mase <- point_accuracy %>% 
  filter(mase >0) %>% 
  mutate(unique_id = paste0(id,'-',horizon)) %>% 
  group_by(unique_id) %>% 
  mutate(rank = rank(mase, ties.method = "first")) %>%   # Rank based on mase, lower is better
  group_by(model) %>%
  mutate(mean_rank = mean(rank)) %>%
  ungroup() %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                                     model == 'rf' ~ 'RF', 
                                     model == 'xgb' ~ 'XGB', 
                                     model == 'ml_ensemble' ~ 'ML ensemble', 
                                     model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                                     model == 'arima' ~ 'ARIMA',
                                     model == 'ets' ~ 'ETS',
                                     model == 'bsts' ~ 'BATS',
                                     model == 'bsts_demo' ~ 'BSTS demo',
                                     model == 'chronos' ~ 'Chronos',
                                     model == 'lag_llama' ~ 'Lag llama',
                                     model == 'mlr' ~ 'MLR',
                                     model == 'moving_average' ~ 'Moving average',
                                     model == 'snaive' ~ 'sNaive',
                                     model == 'timegpt' ~ 'TimeGPT',
                                     model == 'timegpt_reg' ~ 'TimeGPT reg',
                                     model == 'uni_ensemble' ~ 'Uni ensemble',
                                     model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                                     model == 'demographic' ~ 'Demographic',
                                     .default = model))


# Summarize count data

ranked_summary_mase <- ranked_point_accuracy_mase %>%
  group_by(model, rank) %>% 
  summarize(count = n(), mean_rank = mean(mean_rank), .groups = 'drop') %>% 
  group_by(model) %>% 
  reframe(rank = rank,
            count_per = count/sum(count)*100, 
            mean_rank = mean_rank,
            count = count) 

# Plot with reordered models and custom color gradient

ggplot(ranked_summary_mase, aes(x = factor(rank), y = reorder(model, -mean_rank), fill = count_per)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("red", "orange", "yellow", "lightgreen", "green")) +
  labs(#title = "Heatmap of percentage across Rank and Model",
       x = "Rank",
       y = "Model (Ordered by mean rank)",
       fill = 'Percentage') +
  theme_minimal()


# RMSE

# calculate ranking based on rmse

ranked_point_accuracy_rmse <- point_accuracy %>% 
  filter(rmse >0) %>% 
  mutate(unique_id = paste0(id,'-',horizon)) %>% 
  group_by(unique_id) %>% 
  mutate(rank = rank(rmse, ties.method = "first")) %>%   # Rank based on rmse, lower is better
  group_by(model) %>%
  mutate(mean_rank = mean(rank)) %>%
  ungroup() %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           model == 'demographic' ~ 'Demographic',
                           .default = model))


# Summarize count data

ranked_summary_rmse <- ranked_point_accuracy_rmse %>%
  group_by(model, rank) %>% 
  summarize(count = n(), mean_rank = mean(mean_rank), .groups = 'drop') %>% 
  group_by(model) %>% 
  reframe(rank = rank,
          count_per = count/sum(count)*100, 
          mean_rank = mean_rank,
          count = count)


# Plot with reordered models and custom color gradient

ggplot(ranked_summary_rmse, aes(x = factor(rank), y = reorder(model, -mean_rank), fill = count_per)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("red", "orange", "yellow", "lightgreen", "green")) +
  labs(#title = "Heatmap of percentage across Rank and Model",
    x = "Rank",
    y = "Model (Ordered by mean rank)",
    fill = 'Percentage') +
  theme_minimal()


# CRPS

# calculate ranking based on rmse

ranked_prob_accuracy_crps <- prob_accuracy %>% 
  mutate(unique_id = paste0(id,'-',horizon)) %>% 
  group_by(unique_id) %>% 
  mutate(rank = rank(crps, ties.method = "first")) %>%   # Rank based on crps, lower is better
  group_by(model) %>%
  mutate(mean_rank = mean(rank)) %>%
  ungroup() %>% 
  mutate(model = case_when(model == 'lgbm' ~ 'LGBM',
                           model == 'rf' ~ 'RF', 
                           model == 'xgb' ~ 'XGB', 
                           model == 'ml_ensemble' ~ 'ML ensemble', 
                           model == 'hybrid_weighted_averaging' ~ 'Hybrid weighted averaging', 
                           model == 'arima' ~ 'ARIMA',
                           model == 'ets' ~ 'ETS',
                           model == 'bsts' ~ 'BATS',
                           model == 'bsts_demo' ~ 'BSTS demo',
                           model == 'chronos' ~ 'Chronos',
                           model == 'lag_llama' ~ 'Lag llama',
                           model == 'mlr' ~ 'MLR',
                           model == 'moving_average' ~ 'Moving average',
                           model == 'snaive' ~ 'sNaive',
                           model == 'timegpt' ~ 'TimeGPT',
                           model == 'timegpt_reg' ~ 'TimeGPT reg',
                           model == 'uni_ensemble' ~ 'Uni ensemble',
                           model == 'hybrid_bias_adjustment' ~ 'Hybrid bias adjustment',
                           .default = model))


# Summarize count data

ranked_summary_crps <- ranked_prob_accuracy_crps %>%
  group_by(model, rank) %>% 
  summarize(count = n(), mean_rank = mean(mean_rank), .groups = 'drop') %>% 
  group_by(model) %>% 
  reframe(rank = rank,
          count_per = count/sum(count)*100, 
          mean_rank = mean_rank,
          count = count)


# Plot with reordered models and custom color gradient

ggplot(ranked_summary_crps, aes(x = factor(rank), y = reorder(model, -mean_rank), fill = count_per)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("red", "orange", "yellow", "lightgreen", "green")) +
  labs(#title = "Heatmap of percentage across Rank and Model",
    x = "Rank",
    y = "Model (Ordered by mean rank)",
    fill = 'Percentage') +
  theme_minimal()


# we are going to see whether time series features have a relationship with model performance

# calculate ts features

stock_feat <- stock_train_tscv %>% 
  features(stock_distributed, feat_stl) %>% 
  select(unique_id, trend_strength, seasonal_strength_year)


# calculate summary statistics for stock_tsb

stock_train_tscv_summary <- stock_train_tscv %>% 
  as_tibble() %>% 
  group_by(unique_id) %>% 
  summarise(zero_per = mean(zero_per),
            rolling_max_4 = mean(rolling_max_4),
            rolling_zero_per_4 = mean(rolling_zero_per_4),
            mean_demand = mean(stock_distributed),
            sd_demand = sd(stock_distributed),
            variance = var(stock_distributed),
            .groups = 'drop') %>% 
  left_join(stock_feat, by = 'unique_id') %>% 
  left_join(ranked_point_accuracy_mase %>% 
              select(unique_id, rank, model) %>% 
              filter(rank == 1) %>% 
              rename(mase_rank = rank,
                     mase_model = model),
            by = 'unique_id') %>% 
  left_join(ranked_point_accuracy_rmse %>% 
              select(unique_id, rank, model) %>% 
              filter(rank == 1) %>% 
              rename(rmse_rank = rank,
                     rmse_model = model),
            by = 'unique_id') %>% 
  left_join(ranked_prob_accuracy_crps %>% 
              select(unique_id, rank, model) %>% 
              filter(rank == 1) %>% 
              rename(crps_rank = rank,
                     crps_model = model),
            by = 'unique_id')


# feature importance ------------------------------------------------------

lgbm_feat_cv <- read.csv('data/results/lgbm_feat_imp_skt_cv1.csv') %>% 
  mutate(horizon = 1, model = 'LGBM') %>% 
  bind_rows(read.csv('data/results/lgbm_feat_imp_skt_cv2.csv') %>% 
              mutate(horizon = 2, model = 'LGBM'),
            read.csv('data/results/lgbm_feat_imp_skt_cv3.csv') %>% 
              mutate(horizon = 3, model = 'LGBM'))


xgb_feat_cv <- read.csv('data/results/xgb_feat_imp_skt_cv1.csv') %>% 
  mutate(horizon = 1, model = 'XGB') %>% 
  bind_rows(read.csv('data/results/xgb_feat_imp_skt_cv2.csv') %>% 
              mutate(horizon = 2, model = 'XGB'),
            read.csv('data/results/xgb_feat_imp_skt_cv3.csv') %>% 
              mutate(horizon = 3, model = 'XGB'))


rf_feat_cv <- read.csv('data/results/rf_feat_imp_skt_cv1.csv') %>% 
  mutate(horizon = 1, model = 'RF') %>% 
  bind_rows(read.csv('data/results/rf_feat_imp_skt_cv2.csv') %>% 
              mutate(horizon = 2, model = 'RF'),
            read.csv('data/results/rf_feat_imp_skt_cv3.csv') %>% 
              mutate(horizon = 3, model = 'RF'))


lr_feat_cv <- read.csv('data/results/lr_feat_imp_skt_cv1.csv') %>% 
  mutate(horizon = 1, model = 'MLR') %>% 
  bind_rows(read.csv('data/results/lr_feat_imp_skt_cv2.csv') %>% 
              mutate(horizon = 2, model = 'MLR'),
            read.csv('data/results/lr_feat_imp_skt_cv3.csv') %>% 
              mutate(horizon = 3, model = 'MLR'))


feat_cv <- lgbm_feat_cv %>% 
  bind_rows(xgb_feat_cv, rf_feat_cv, lr_feat_cv)


# making the line plot over horizons

barplot_feat_cv <- feat_cv %>% 
  ggplot(aes(x = reorder(feature, importance), y = importance, 
             group = factor(horizon, levels = c(3, 2, 1)), 
             fill = factor(horizon, levels = c(3, 2, 1)))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_fill_manual(
    values = c('1' = '#d62728',            
               '2' = '#0072B2',
               '3' = '#E69F00')
  ) +
  facet_wrap(vars(model), scales = "free_x") +
  labs(x = "Feature importance", y = "Feature", fill = 'Forecast horizon') +
  ggthemes::theme_few() +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.25))



