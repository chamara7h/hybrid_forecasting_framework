## Create single df of predictions

library(tidyverse)
library(tsibble)



# reading actuals ---------------------------------------------------------

actuals <- read.csv('data/tidy_data/stock_tsb.csv') %>% 
  select(-X) %>% 
  mutate(yearmonth = yearmonth(yearmonth),
         id = paste0(site_code, product_code))


# univariate_models -------------------------------------------------------

bs_pred_uni <- read_rds('data/predictions/uni_forecasts_bs.rds') %>% 
  mutate(id = paste0(site_code, product_code), X0 = X1000) %>% 
  select(!c('site_code', 'product_code', '80%_lower', '80%_upper', '95%_lower', '95%_upper', 'X1000')) %>% 
  rename(model = .model, prediction = .mean)

point_pred_uni <- read_rds('data/predictions/uni_point_pred.rds')


# make univariate ensemble model

bs_pred_uni_ensemble <- bs_pred_uni %>% 
  select(-model) %>% 
  group_by(id, yearmonth) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  mutate(model = 'ensemble_uni')



# ml_models ---------------------------------------------------------------

# ml models - point predictions

# ml for all timeseries

ml_all <- read.csv('data/predictions/ml_forecasts_all.csv') %>%
  mutate(yearmonth = yearmonth(yearmonth)) %>%
  select(yearmonth, id, stock_distributed, lm_pred, lgbm_pred_tune, xgb_pred, rf_pred) %>%
  rename(mlr_all = lm_pred, lgbm_all = lgbm_pred_tune, xgb_all = xgb_pred, rf_all = rf_pred) %>%
  pivot_longer(-c(yearmonth, id, stock_distributed), names_to = 'model', values_to = 'prediction')


# ml for filtered time series (remove ts with high number of zeros)

ml_filtered <- read.csv('data/predictions/ml_forecasts_filtered.csv') %>%
  mutate(yearmonth = yearmonth(yearmonth)) %>%
  select(yearmonth, id, stock_distributed, lm_pred, lgbm_pred_tune, xgb_pred, rf_pred) %>%
  rename(mlr_filtered = lm_pred, lgbm_filtered = lgbm_pred_tune, xgb_filtered = xgb_pred, rf_filtered = rf_pred) %>%
  pivot_longer(-c(yearmonth, id, stock_distributed), names_to = 'model', values_to = 'prediction')


# make ml point ensemble model

ml_filtered_ensemble <- ml_filtered %>% 
  filter(model != 'mlr_filtered') %>% 
  select(-model) %>% 
  group_by(id, yearmonth) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  mutate(model = 'ensemble_ml_filtered')


# ml filtered model with demo factors

ml_demo <- read.csv('data/predictions/ml_forecasts_with_demo.csv') %>%
  mutate(yearmonth = yearmonth(yearmonth)) %>%
  select(yearmonth, id, stock_distributed, lm_pred, lgbm_pred_tune, xgb_pred, rf_pred) %>%
  rename(mlr_demo = lm_pred, lgbm_demo = lgbm_pred_tune, xgb_demo = xgb_pred, rf_demo = rf_pred) %>%
  pivot_longer(-c(yearmonth, id, stock_distributed), names_to = 'model', values_to = 'prediction')


# ml bootsrap models

bs_pred_lgbm <- read_rds('data/predictions/boots_pred_lgbm_tidy.rds') %>%
  mutate(id = paste0(site_code, product_code), model = 'lgbm_bs') %>%
  select(!c(unique_id, site_code, product_code, stock_distributed,
            median_prediction, lower_bound, upper_bound)) %>% 
  rename(prediction = mean_prediction)


bs_pred_lm <- read_rds('data/predictions/boots_pred_lm_tidy.rds') %>% 
  mutate(id = paste0(site_code, product_code), model = 'mlr_bs') %>% 
  select(!c(unique_id, site_code, product_code, stock_distributed,
            median_prediction, lower_bound, upper_bound)) %>% 
  rename(prediction = mean_prediction)


bs_pred_xgb <- read_rds('data/predictions/boots_pred_xgb_tidy.rds') %>% 
  mutate(id = paste0(site_code, product_code), model = 'xgb_bs') %>% 
  select(!c(unique_id, site_code, product_code, stock_distributed,
            median_prediction, lower_bound, upper_bound)) %>% 
  rename(prediction = mean_prediction)


bs_pred_rf <- read_rds('data/predictions/boots_pred_rf_tidy.rds') %>% 
  mutate(id = paste0(site_code, product_code), model = 'rf_bs') %>% 
  select(!c(unique_id, site_code, product_code, stock_distributed,
            median_prediction, lower_bound, upper_bound)) %>% 
  rename(prediction = mean_prediction)


# ml bootstrap ensemble model

bs_pred_ensemble <- bs_pred_lgbm %>% 
  bind_rows(bs_pred_lgbm, bs_pred_xgb) %>% 
  select(-model) %>% 
  group_by(id, yearmonth) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  mutate(model = 'ensemble_ml_bs')


# sk time ml models

sk_pred_lgbm <- read_rds("data/predictions/sk_pred_lgbm_tidy.rds")

sk_pred_xgb <- read_rds("data/predictions/sk_pred_xgb_tidy.rds")

sk_pred_rf <- read_rds("data/predictions/sk_pred_rf_tidy.rds")

sk_pred_lm <- read_rds("data/predictions/sk_pred_lm_tidy.rds")


# sk_time ensemble model

sk_pred_ensemble <- sk_pred_lgbm %>% 
  bind_rows(sk_pred_xgb, bs_pred_xgb) %>% 
  select(-model) %>% 
  group_by(id, yearmonth) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  mutate(model = 'ensemble_ml_sk')


# bayesian_models ---------------------------------------------------------


# Bayesian structural timeseries model - with normal predictors

bsts_pred <- read_rds('data/predictions/bsts_prob_pred.rds') %>% 
  mutate(yearmonth = yearmonth(yearmonth),
         prediction = rowMeans(across(starts_with("X")))) %>% 
  select(yearmonth, id, model, prediction, starts_with("X") & num_range("X", 0:999))


# Bayesian structural timeseries model - with demo predictors

bsts_demo_pred <- read_rds('data/predictions/bsts_demo_prob_pred.rds') %>% 
  mutate(yearmonth = yearmonth(yearmonth),
         prediction = rowMeans(across(starts_with("X")))) %>% 
  select(yearmonth, id, model, prediction, starts_with("X") & num_range("X", 0:999))



# Foundational timeseries models ------------------------------------------

# lag llama

lag_llama_pred <- read.csv('data/predictions/lag_llama_pred.csv') %>% 
  mutate(yearmonth = yearmonth(yearmonth),
         prediction = rowMeans(across(starts_with("X"))))


chronos_pred <- read.csv('data/predictions/chronos_pred.csv') %>% 
  mutate(fc_step = row_number(),
         id2 = paste0(id, fc_step)) %>% 
  
  left_join(lag_llama_pred %>%
              mutate(fc_step = row_number(),
                     id2 = paste0(id, fc_step)) %>% 
            select(id2, yearmonth), by = 'id2') %>% 
  
  select(-id2, -fc_step) %>% 
  mutate(prediction = rowMeans(across(starts_with("X"))))
  
  
# demographic_models ------------------------------------------------------

# demographic probabilistic forecasting model

bs_pred_demo <- read_rds('data/predictions/demo_prob_predictions.rds') %>% 
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  as_tsibble(index = yearmonth, key = c(site_code, product_code, group_id)) %>% 
  filter_index('2019 Oct' ~ .)

new_names <- paste0("X", 0:999)

bs_pred_demo_wide <- bs_pred_demo %>% 
  as_tibble() %>% 
  pivot_wider(names_from = 'group_id', values_from = 'demographic_forecast') %>% 
  mutate(id = paste0(site_code, product_code), model = 'demographic_prob') %>% 
  select(-site_code, -product_type, -product_code, -stock_distributed) %>% 
  rename_with(~ new_names, all_of(2:1001)) %>% 
  mutate(prediction = rowMeans(across(starts_with("X"))))


# demographic forecasting ensemble model

bs_pred_demo_ensemble <- bs_pred_demo_wide %>% 
  bind_rows(bs_pred_ensemble) %>% 
  select(-model) %>% 
  group_by(id, yearmonth) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  mutate(model = 'ensemble_demo_bs')


# demographic point forecasting model

pred_demo <- read.csv('data/predictions/demo_point_forecast.csv') %>% 
  mutate(yearmonth = yearmonth(yearmonth), id = paste0(site_code, product_code), model = 'demographic_point') %>% 
  rename(prediction = demographic_forecast) %>% 
  select(yearmonth, id, model, stock_distributed, prediction) %>% 
  as_tsibble(index = yearmonth, key = id) %>% 
  filter_index('2019 Oct' ~ .) %>% 
  as_tibble()



# hybrid_method -----------------------------------------------------------

# ml and demo quantile regression averaging forecasts (bs and sk are two different python packages)

bs_pred_hybrid <- read_rds('data/predictions/bs_pred_hybrid_quantile.rds') %>%  #point
  as_tibble() %>%
  mutate(id = paste0(site_code, product_code)) %>%
  janitor::remove_empty(which = "cols") %>% 
  mutate(hybrid_optimise_bs = rowMeans(across(starts_with("X")))) %>% 
  select(yearmonth, id, stock_distributed, hybrid_pred_bs, hybrid_optimise_bs) %>%
  pivot_longer(-c(yearmonth, id, stock_distributed), names_to = 'model', values_to = 'prediction')


bs_pred_hybrid_q <- read_rds('data/predictions/bs_pred_hybrid_quantile.rds') %>%  #probabilistic
  as_tibble() %>%
  mutate(id = paste0(site_code, product_code)) %>%
  select(!c(product_type, lgbm_forecast, lgbm_forecast, rf_forecast, xgb_forecast, hybrid_pred_bs, demographic_forecast))


sk_pred_hybrid <- read_rds('data/predictions/sk_pred_hybrid_quantile.rds') %>%
  as_tibble() %>%
  mutate(id = paste0(site_code, product_code)) %>%
  janitor::remove_empty(which = "cols") %>% 
  mutate(hybrid_optimise_sk = rowMeans(across(starts_with("X")))) %>% 
  select(yearmonth, id, stock_distributed, hybrid_pred_sk, hybrid_optimise_sk) %>%
  pivot_longer(-c(yearmonth, id, stock_distributed), names_to = 'model', values_to = 'prediction')


sk_pred_hybrid_q <- read_rds('data/predictions/sk_pred_hybrid_quantile.rds') %>%
  as_tibble() %>%
  mutate(id = paste0(site_code, product_code)) %>%  
  select(!c(product_type, lgbm_forecast, lgbm_forecast, rf_forecast, xgb_forecast, hybrid_pred_sk, demographic_forecast, prediction)) %>% 
  select(-starts_with("Q"))


# hybrid scaled (sacled predictors)

hybrid_adjusted <- read_rds('data/predictions/hybrid_adjusted.rds') %>% 
  mutate(prediction = rowMeans(across(starts_with("X"))))


## hierarchical reconciliation approach for hybrid forecast combination

hybrid_hts <- read.csv('data/predictions/hts_forecasts_pt.csv') %>%  #product type aggregation
  janitor::clean_names() %>% 
  separate(unique_id, into = c("product_type", "product_code", "region", "district", "site_code"), sep = "/") %>% 
  mutate(yearmonth = yearmonth(ds), id = paste0(site_code, product_code)) %>% 
  select(yearmonth, id, lgbm_bottom_up, lgbm_min_trace_method_ols) %>% 
  pivot_longer(!c(yearmonth, id), names_to = 'model', values_to = 'prediction')


hybrid_hts_all <- read.csv('data/predictions/hts_forecasts.csv') %>% # national level aggregation
  janitor::clean_names() %>% 
  separate(unique_id, into = c("country", 'region', 'district', 'site_code', "product_type", "product_code"), sep = "/") %>% 
  mutate(yearmonth = yearmonth(ds), id = paste0(site_code, product_code)) %>% 
  select(yearmonth, id, lgbm_bottom_up, lgbm_min_trace_method_ols, lgbm_min_trace_method_mint_shrink) %>% 
  rename(lgbm_bottom_up_all = lgbm_bottom_up, lgbm_min_trace_method_ols_all = lgbm_min_trace_method_ols, 
         lgbm_min_trace_method_mint_shrink_all = lgbm_min_trace_method_mint_shrink) %>% 
  pivot_longer(!c(yearmonth, id), names_to = 'model', values_to = 'prediction')



# competition_submissions -------------------------------------------------

my_read_csv <- function(x) {
  data <- read_csv(x)
  filename <- tools::file_path_sans_ext(basename(x))  # Extract filename without extension
  data$model <- filename      # Add it as a new column
  data
}


filenames <- list.files(path = 'data/submissions/', pattern = "\\.csv$", full.names = TRUE)


competition <- lapply(filenames, my_read_csv) %>%
  bind_rows()


competition_pred <- competition %>% 
  mutate(yearmonth = with(., sprintf("%d-%02d", year, month)), 
         yearmonth = yearmonth(yearmonth),
         id = paste0(site_code, product_code)) %>% 
  select(yearmonth, id, model, predicted_value) %>% 
  rename(prediction = predicted_value) %>% 
  left_join(actuals, by = c('yearmonth', 'id')) %>% 
  replace(is.na(.), 0)



# point_prediction_master_df ----------------------------------------------

# Helper function to select relevant columns

select_columns <- function(df) {
  df %>% select(yearmonth, id, model, prediction)
}


# Combining predictions

bs_pred_point <- point_pred_uni %>% 
  select_columns() %>% 
  bind_rows(
    select_columns(bs_pred_uni_ensemble),
    select_columns(bs_pred_lm),
    select_columns(bs_pred_lgbm),
    select_columns(bs_pred_xgb),
    select_columns(bs_pred_rf),
    select_columns(bs_pred_demo_wide),
    select_columns(bs_pred_ensemble),
    select_columns(bs_pred_demo_ensemble),
    # select_columns(blr_pred),
    select_columns(sk_pred_ensemble),
    select_columns(hybrid_adjusted),
    select_columns(lag_llama_pred),
    select_columns(chronos_pred),
    select_columns(bsts_pred),
    select_columns(bsts_demo_pred),
    select_columns(sk_pred_lgbm), 
    select_columns(sk_pred_xgb), 
    select_columns(sk_pred_rf), 
    select_columns(sk_pred_lm),
    select_columns(competition_pred),
    hybrid_hts,
    hybrid_hts_all
  ) %>%  
  left_join(actuals, by = c("id", "yearmonth")) %>% 
  select(yearmonth, id, model, stock_distributed, prediction) %>% 
  bind_rows(
    ml_all,
    ml_filtered,
    ml_demo,
    ml_filtered_ensemble,
    pred_demo,
    sk_pred_hybrid
  ) %>% 
  mutate(prediction = ifelse(prediction < 0, 0, prediction))

# options(scipen = 999)

write_rds(bs_pred_point, 'data/predictions/bs_predictions_all_point.rds')


# probabilistic_prediction_master_df --------------------------------------


# Helper function to select relevant columns
select_columns_except_prediction <- function(df) {
  df %>% select(-prediction)
}

# Helper function to select columns except prediction
select_columns_except_prediction <- function(df) {
  df %>% select(-prediction)
}

# Helper function to select columns except prediction and stock_distributed
select_columns_except_prediction_stock <- function(df) {
  df %>% select(-prediction, -stock_distributed)
}

# Helper function for special hybrid case
select_hybrid_columns <- function(df, model_name) {
  df %>% select(-site_code, -product_code, -stock_distributed) %>% mutate(model = model_name)
}

# Combining predictions
bs_pred_prob <- bs_pred_uni %>% 
  select_columns_except_prediction() %>% 
  bind_rows(
    select_columns_except_prediction(bs_pred_uni_ensemble),
    select_columns_except_prediction(bs_pred_lm),
    select_columns_except_prediction(bs_pred_lgbm),
    select_columns_except_prediction(bs_pred_xgb),
    select_columns_except_prediction(bs_pred_rf),
    select_columns_except_prediction(bs_pred_demo_wide),
    select_columns_except_prediction(bs_pred_ensemble),
    select_columns_except_prediction(bs_pred_demo_ensemble),
    # select_columns_except_prediction(blr_pred),
    select_columns_except_prediction_stock(sk_pred_lgbm),
    select_columns_except_prediction_stock(sk_pred_xgb),
    select_columns_except_prediction_stock(sk_pred_rf),
    select_columns_except_prediction_stock(sk_pred_lm),
    select_columns_except_prediction(sk_pred_ensemble),
    select_columns_except_prediction(lag_llama_pred),
    select_columns_except_prediction(chronos_pred),
    select_columns_except_prediction(bsts_pred),
    select_columns_except_prediction(bsts_demo_pred),
    select_hybrid_columns(sk_pred_hybrid_q, "hybrid_sk"),
    select_hybrid_columns(bs_pred_hybrid_q, "hybrid_bs")
  ) %>%  
  left_join(actuals, by = c("id", "yearmonth")) %>% 
  mutate(across(starts_with("X"), ~ if_else(. < 0, 0, .))) %>% 
  select_if(~ !any(is.na(.)))

write_rds(bs_pred_prob, "data/predictions/bs_predictions_all_prob.rds")


# quantile_prediction_master ----------------------------------------------

# create quantiles

bs_pred_q <- bs_pred_prob %>% 
  mutate(across(starts_with("X"), ~ if_else(. < 0, 0, .))) %>% 
  select(starts_with("X")) %>% 
  bind_cols(as.data.frame(t(apply(., 1, quantile, seq(0.01, 0.99, by = 0.01))))) 


new_names <- paste0("Q", 1:99)

old_names <- names(bs_pred_q)[(ncol(bs_pred_q) - 98):ncol(bs_pred_q)] 


## create total quantile data

bs_pred_q_full <- bs_pred_prob %>%
  select(yearmonth, id, model, stock_distributed) %>% 
  bind_cols(bs_pred_q %>% 
              rename_with(~ new_names, all_of(old_names)) %>%
              select(starts_with("Q"))) %>% 
  bind_rows(bs_pred_hybrid_q %>% 
              mutate(model = 'hybrid_optimise',
                     id = paste0(site_code, product_code)) %>% 
              select(-site_code, -product_code, -prediction))


# create ensmble model using simple averaging for quantiles

bs_pred_q_ensemble <- bs_pred_q_full %>% 
  filter(model == 'lgbm_bs' | model == 'xgb_bs' | model == 'rf_bs' | model == 'demographic_prob') %>% 
  select(-model) %>% 
  group_by(id, yearmonth) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  mutate(model = 'ensemble_demo_qt')


bs_pred_q_full <- bs_pred_q_full %>% 
  bind_rows(bs_pred_q_ensemble)

write_rds(bs_pred_q_full, 'data/predictions/bs_predictions_all_q.rds')
