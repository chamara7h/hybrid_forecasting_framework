# prepare master prediction file - TSCV

# load libraries

library(tidyverse)
library(fable)
library(tsibble)


# data preperation --------------------------------------------------------

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


# create tscv test data

stock_test_tscv <- stock_tsb_test %>% 
  slide_tsibble(.size = 3, .step = 1) %>% 
  mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id) %>% 
  select(-id, -stock_distributed) %>% 
  rename(ds = yearmonth) %>% 
  as_tibble()


# Univariate models -------------------------------------------------------

snaive <- read_rds('data/predictions/snaive_prob_cv.rds') %>% 
  mutate(model = 'snaive',
         across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))),
         id = paste0(site_code, product_code)) %>% 
  select(-site_code, -product_code) %>% 
  rename(X0 = X1000)


ma <- read_rds('data/predictions/ma_prob_cv.rds') %>% 
  mutate(model = 'moving_average',
         across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))),
         id = paste0(site_code, product_code)) %>% 
  select(-site_code, -product_code) %>% 
  rename(X0 = X1000)


ets <- read_rds('data/predictions/ets_prob_cv.rds') %>% 
  mutate(model = 'ets',
         across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))),
         id = paste0(site_code, product_code)) %>% 
  select(-site_code, -product_code) %>% 
  rename(X0 = X1000)


arima <- read_rds('data/predictions/arima_prob_cv.rds') %>% 
  mutate(model = 'arima',
         across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))),
         id = paste0(site_code, product_code)) %>% 
  select(-site_code, -product_code) %>% 
  rename(X0 = X1000)


croston <- read_rds('data/predictions/croston_pred_cv.rds') %>% 
  mutate(model = 'croston',
         id = paste0(site_code, product_code)) %>% 
  select(-site_code, -product_code)


uni_ensemble <- snaive %>% 
  bind_rows(ma, ets, arima) %>% 
  select(-model) %>%  
  group_by(yearmonth, id, horizon) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  mutate(model = 'uni_ensemble')


# ML methods --------------------------------------------------------------

lgbm <- read.csv('data/predictions/lgbm_forecasts_skt_cv.csv') %>% 
  mutate(yearmonth = yearmonth(dmy(yearmonth)),
         across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))))


xgb <- read.csv('data/predictions/xgb_forecasts_skt_cv.csv') %>% 
  mutate(yearmonth = yearmonth(dmy(yearmonth)),
         across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))))


rf <- read.csv('data/predictions/rf_forecasts_skt_cv.csv') %>% 
  mutate(yearmonth = yearmonth(dmy(yearmonth)),
         across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))))


lr <- read.csv('data/predictions/lr_forecasts_skt_cv.csv') %>% 
  mutate(yearmonth = yearmonth(dmy(yearmonth)),
         across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))))


ml_ensemble <- lgbm %>% 
  bind_rows(xgb, rf, lr) %>% 
  select(-model) %>% 
  group_by(yearmonth, id, horizon) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  mutate(model = 'ml_ensemble') 


# Bayesian models ---------------------------------------------------------

bsts <- read_rds('data/predictions/bsts_prob_pred_cv.rds') %>% 
  mutate(across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X")))) %>% 
  select(-stock_distributed)
  

bsts_demo <- read_rds('data/predictions/bsts_demo_prob_pred_cv.rds') %>% 
  mutate(across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X")))) %>% 
  select(-stock_distributed)


# Demographic model -------------------------------------------------------

demo <- read_rds('data/predictions/demo_pred_cv.rds') %>% 
  mutate(model = 'demographic',
         id = paste0(site_code, product_code)) %>% 
  rename(prediction = demographic_forecast) %>% 
  as_tibble() %>% 
  select(yearmonth, id, model, horizon, prediction)


# Hybrid method -----------------------------------------------------------

hybrid_new <- read_rds('data/predictions/hybrid_prob_pred_cv.rds') %>% 
  select(!c(site_code, product_type, product_code, stock_distributed)) %>% 
  mutate(prediction = rowMeans(across(starts_with("X"))),
         model = 'hybrid_bias_series')


hybrid_new_fh <- read_rds('data/predictions/hybrid_prob_pred_cv_new_fh.rds') %>% 
  select(!c(site_code, product_type, product_code, stock_distributed)) %>% 
  mutate(prediction = rowMeans(across(starts_with("X"))),
         model = 'hybrid_bias_fh')


hybrid_old_fh <- read_rds('data/predictions/hybrid_prob_pred_no_bias_cv.rds') %>% 
  select(!c(site_code, product_type, product_code, stock_distributed)) %>% 
  mutate(prediction = rowMeans(across(starts_with("X"))),
         model = 'hybrid_old_fh')


# Foundational models -----------------------------------------------------

# lag llama

lag_llama <- read.csv('data/predictions/lag_llama_pred_cv.csv') %>% 
  mutate(yearmonth = yearmonth(yearmonth),
         prediction = rowMeans(across(starts_with("X")))) %>% 
  separate(id, into = c("id", "horizon"), sep = "-") %>% 
  mutate(horizon = as.integer(horizon)) %>% 
  as_tibble()


# Chronos

chronos <- read.csv('data/predictions/chronos_pred_cv.csv') %>% 
  separate(id, into = c("id", "horizon"), sep = "-") %>% 
  mutate(prediction = rowMeans(across(starts_with("X"))),
         fc_step = row_number(),
         id2 = paste0(id, fc_step)) %>% 
  left_join(lag_llama %>% 
              mutate(fc_step = row_number(),
                     id2 = paste0(id, fc_step)) %>% 
              select(yearmonth, id2),
            by = 'id2') %>% 
  select(-id2, -fc_step) %>% 
  mutate(horizon = as.integer(horizon)) %>% 
  as_tibble()
  

# timegpt as a univariate model
  
timegpt <- read.csv('data/predictions/timegpt_quantile_uni_cv.csv') %>% 
  mutate(across(starts_with("T"), ~ if_else(. < 0, 0, .)))
  

# Function to interpolate 1000 points from quantiles (timegpt only generates quantiles)

interpolate_points <- function(row_quantiles, quantiles, n_points = 1022) {
  approx(x = quantiles, y = row_quantiles, xout = seq(0, 1, length.out = n_points))$y
}


# Define quantile levels

quantiles <- quantiles <- seq(0.01, 0.99, by = 0.01)
  

# Apply the interpolation function row-wise

distribution_points <- t(apply(timegpt %>% select(-unique_id, -ds, -TimeGPT), 1, interpolate_points, quantiles = quantiles))


# Create column names for the distribution points

colnames(distribution_points) <- paste0('X', seq_len(ncol(distribution_points)))


# Bind the distribution points to the original data

timegpt_updated <- timegpt %>%
  bind_cols(as.data.frame(distribution_points)) %>% 
  select(where(function(x) !all(is.na(x)))) %>% 
  select(-contains("Time")) %>% 
  mutate(prediction = rowMeans(across(starts_with("X")))) %>% 
  rename_with(~ paste0("X", as.integer(str_extract(., "\\d+")) - 12), starts_with("X")) %>% 
  separate(unique_id, into = c("id", "horizon"), sep = "-") %>%
  rename(yearmonth = ds) %>% 
  mutate(horizon = as.integer(horizon),
         yearmonth = yearmonth(yearmonth),
         model = 'timegpt') %>% 
  as_tibble()
  
  
# timegpt with regressors 

timegpt_reg <- read.csv('data/predictions/timegpt_quantile_cv.csv') %>% 
  mutate(across(starts_with("T"), ~ if_else(. < 0, 0, .)))


# Function to interpolate 1000 points from quantiles (timegpt only generates quantiles)

interpolate_points <- function(row_quantiles, quantiles, n_points = 1022) {
  approx(x = quantiles, y = row_quantiles, xout = seq(0, 1, length.out = n_points))$y
}


# Define quantile levels

quantiles <- quantiles <- seq(0.01, 0.99, by = 0.01)


# Apply the interpolation function row-wise

distribution_points <- t(apply(timegpt_reg %>% select(-unique_id, -ds, -TimeGPT), 1, interpolate_points, quantiles = quantiles))


# Create column names for the distribution points

colnames(distribution_points) <- paste0('X', seq_len(ncol(distribution_points)))


# Bind the distribution points to the original data

timegpt_reg_updated <- timegpt_reg %>%
  bind_cols(as.data.frame(distribution_points)) %>% 
  select(where(function(x) !all(is.na(x)))) %>% 
  select(-contains("T")) %>% 
  mutate(prediction = rowMeans(across(starts_with("X"))),
         model = 'timegpt_reg') %>% 
  rename_with(~ paste0("X", as.integer(str_extract(., "\\d+")) - 12), starts_with("X")) %>% 
  separate(unique_id, into = c("id", "horizon"), sep = "-") %>%
  rename(yearmonth = ds) %>% 
  mutate(horizon = as.integer(horizon),
         yearmonth = yearmonth(yearmonth)) %>% 
  as_tibble()


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
         id = paste0(site_code, product_code),
         horizon = 3) %>% 
  select(yearmonth, id, model, horizon, predicted_value) %>% 
  rename(prediction = predicted_value)  %>% 
  replace(is.na(.), 0)



# Create prob and point masters -------------------------------------------

prob_pred_cv <- snaive %>% 
  bind_rows(ma, ets, arima,
            lr, lgbm, xgb, rf,
            bsts, bsts_demo,
            hybrid_new, hybrid_new_fh, hybrid_old_fh,
            uni_ensemble, ml_ensemble,
            chronos, lag_llama, timegpt_updated, timegpt_reg_updated)

write_rds(prob_pred_cv, 'data/predictions/prob_pred_cv.rds')

point_pred_cv <- prob_pred_cv %>% 
  select(yearmonth, id, model, horizon, prediction) %>% 
  bind_rows(croston, 
            demo,
            competition_pred)

write_rds(point_pred_cv, 'data/predictions/point_pred_cv.rds')
  
