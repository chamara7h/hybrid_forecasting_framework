# hybrid model - Cross validation

library(tidyverse)
library(tsibble)
library(quantreg)
library(doParallel)
library(foreach)


# data preparation for demographic forecast model -------------------------

# read data

stock_df <- read.csv('data/tidy_data/stock_tsb_zero_remove.csv') %>% 
  select(yearmonth, district, site_code, product_type, product_code, stock_distributed) %>% 
  mutate(yearmonth = yearmonth(yearmonth), year = year(yearmonth))


new_predictors <- read.csv('data/tidy_data/new_predictors.csv') %>% 
  mutate(yearmonth = yearmonth(yearmonth))


pop_df <- read_rds('data/tidy_data/pop_tidy.rds')


method_mix <- read.csv('data/fp_indicators/contraceptive_method_mix.csv')


cyp_factor <- read.csv('data/fp_indicators/cyp_factor.csv')


market_share <- read.csv('data/fp_indicators/market_share.csv')


mcpr_rate <- read.csv('data/fp_indicators/percent_of_women_with_demand_modern_methods.csv')


# calculate brand preference

stock_demo <- stock_df %>% 
  group_by(year, site_code, product_code) %>% 
  mutate(total_product = sum(stock_distributed)) %>% 
  ungroup() %>% 
  group_by(year, site_code, product_type) %>% 
  mutate(total_type = sum(stock_distributed)) %>% 
  ungroup() %>%
  mutate(brand_mix = total_product/total_type) %>% 
  mutate_all(~replace(., is.nan(.), 0)) %>% 
  group_by(yearmonth, product_code) %>% ## to calculate product stock distribution in months
  mutate(total_product_month = sum(stock_distributed)) %>% 
  ungroup() %>% 
  group_by(year, product_code) %>% 
  mutate(total_product_year = sum(stock_distributed)) %>% 
  ungroup() %>% 
  mutate(weight_dis = total_product_month/total_product_year)


# join other demo factors

demo_df <- stock_demo %>% 
  left_join(method_mix, by = c('year', 'product_type')) %>% 
  left_join(cyp_factor, by = c('year', 'product_code')) %>% 
  left_join(market_share %>% filter(donor == 'USAID'), by = c('year')) %>% 
  left_join(mcpr_rate %>% select(year, age_group, percent_of_women_with_demand) %>% 
              pivot_wider(names_from = age_group, values_from = percent_of_women_with_demand) %>% 
              rename(mcpr_20 = `20`,
                     mcpr_25 = `25`,
                     mcpr_30 = `30`,
                     mcpr_35 = `35`,
                     mcpr_40 = `40`,
                     mcpr_45 = `45`,
                     mcpr_50 = `50`), by = 'year') %>% 
  left_join(pop_df %>% select(!c(site_type, site_region, site_district, site_latitude, site_longitude)), by = c('year', 'site_code')) %>% 
  rename(market_share_per = contribution_to_contraceptive_commodities)


# Demographic forecasting model -------------------------------------------

demo_fc <- demo_df %>% 
  mutate(demographic_forecast = cyp_per_unit * brand_mix * market_share_per * (method_mix/100) *
           (age_20 * mcpr_20/100 +
              age_25 * mcpr_25/100 +
              age_30 * mcpr_30/100 +
              age_35 * mcpr_35/100 +
              age_40 * mcpr_40/100 +
              age_45 * mcpr_45/100 +
              age_50 * mcpr_50/100) * weight_dis) %>% 
  select(yearmonth, site_code, product_type, product_code, stock_distributed, demographic_forecast) %>% 
  mutate(id = paste0(site_code, product_code))


# creating test set

demo_tsb_test <- demo_fc %>% 
  as_tsibble(index = yearmonth, key = c(site_code, product_code)) %>% 
  filter_index('2019 Aug' ~ .) 


# create tscv test data

f_horizon = 3

demo_tsb_test_tscv <- demo_tsb_test %>% 
  slide_tsibble(.size = 3, .step = 1) %>% 
  rename(horizon = .id)

write_rds(demo_tsb_test_tscv, 'data/predictions/demo_pred_cv.rds')


# Ensemble ML model -------------------------------------------------------

lgbm_pred <- read.csv('data/predictions/lgbm_forecasts_skt_cv.csv') %>% 
  mutate(yearmonth = yearmonth(dmy(yearmonth)),
         # across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))))


xgb_pred <- read.csv('data/predictions/xgb_forecasts_skt_cv.csv') %>% 
  mutate(yearmonth = yearmonth(dmy(yearmonth)),
         # across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))))


rf_pred <- read.csv('data/predictions/rf_forecasts_skt_cv.csv') %>% 
  mutate(yearmonth = yearmonth(dmy(yearmonth)),
         # across(starts_with("X"), ~ if_else(. < 0, 0, .)),
         prediction = rowMeans(across(starts_with("X"))))


ml_ensemble_pred <- lgbm_pred %>% 
  bind_rows(xgb_pred, rf_pred) %>% 
  select(-model) %>% 
  group_by(yearmonth, id, horizon) %>% 
  summarise(across(where(is.double), ~ mean(.)), .groups = 'drop') %>% 
  as_tibble() %>% 
  arrange(id, horizon)


demo_ensemble_point_pred <- demo_tsb_test_tscv %>% 
  left_join(lgbm_pred %>% 
              select(yearmonth, id, horizon, prediction) %>% 
              rename(lgbm = prediction), by = c('yearmonth', 'id', 'horizon')) %>% 
  left_join(xgb_pred %>% 
              select(yearmonth, id, horizon, prediction) %>% 
              rename(xgb = prediction), by = c('yearmonth', 'id', 'horizon')) %>% 
  left_join(rf_pred %>% 
              select(yearmonth, id, horizon, prediction) %>% 
              rename(rf = prediction), by = c('yearmonth', 'id', 'horizon')) %>% 
  mutate(mean_pred = (demographic_forecast + lgbm + xgb + rf)/4) %>% 
  as_tibble() %>% 
  arrange(id, horizon)


# Custom Quantile Regression Averaging (CQRA) -----------------------------

# Define quantile levels

quantiles <- quantiles <- seq(0.01, 0.99, by = 0.01)


# Updated CQRA loss function with bias term

cqra_loss <- function(params, point_forecast, prob_forecast, quantiles) {
  # Separate weights and bias from params
  weights <- params[-length(params)]
  bias <- params[length(params)]
  
  # Calculate the weighted forecast for each quantile
  weighted_forecasts <- matrix(NA, nrow(prob_forecast), length(quantiles))
  for (i in seq_along(quantiles)) {
    weighted_forecasts[, i] <- prob_forecast[, i] * weights[i] + bias
  }
  
  # Calculate the pinball loss for each quantile
  pinball_losses <- abs(point_forecast - rowMeans(weighted_forecasts))
  
  for (i in seq_along(quantiles)) {
    pinball_losses <- pinball_losses + pmax(0, (point_forecast - weighted_forecasts[, i]) * 
                                              (quantiles[i] - (point_forecast < weighted_forecasts[, i])))
  }
  
  # Return the sum of pinball losses
  return(sum(pinball_losses))
}


# filter ids

id_list <- ml_ensemble_pred %>% 
  select(horizon) %>% 
  unique() %>% 
  pull()


# Register outer parallel backend with 6 cores

cl_outer <- makeCluster(6)
registerDoParallel(cl_outer)

# I am introducing a bias factor and allow weights to be more than 1 to give priority to expert method

system.time(hybrid_prob_pred <- foreach(i = id_list,
                                        .combine = 'rbind',
                                        .packages=c("doParallel", "foreach", "tidyverse", "quantreg", "tsibble")) %dopar% {
                                          
                                          #i = id_list[2]
                                          
                                          point_id <- demo_ensemble_point_pred %>% filter(horizon == i)
                                          prob_id <- ml_ensemble_pred %>% filter(horizon == i) %>% select(-c(yearmonth, id, horizon, prediction))
                                          
                                          
                                          # Extract point forecasts and probabilistic forecasts
                                          
                                          point_forecast <- point_id$mean_pred
                                          prob_forecast <- as.matrix(prob_id)
                                          
                                          
                                          # Initial weights and bias (bias starts at 0)
                                          
                                          initial_weights <- rep(1 / length(quantiles), length(quantiles))
                                          initial_bias <- 0
                                          initial_params <- c(initial_weights, initial_bias)
                                          
                                          
                                          # Optimize the CQRA loss function
                                          
                                          result <- optim(initial_params, cqra_loss, method = "L-BFGS-B", 
                                                          point_forecast = point_forecast, 
                                                          prob_forecast = prob_forecast, 
                                                          quantiles = quantiles, 
                                                          lower = c(rep(0, length(quantiles)), -Inf), # Allow bias to be negative if needed
                                                          upper = c(rep(2, length(quantiles)), Inf)) # Allow weights to exceed 1
                                          
                                          
                                          # Extract final weights and bias
                                          
                                          final_weights <- result$par[-length(result$par)]
                                          final_bias <- result$par[length(result$par)]
                                          
                                          
                                          # Calculate the weighted probabilistic forecasts for each quantile with the optimized bias
                                          
                                          weighted_forecasts <- matrix(NA, nrow(prob_forecast), length(quantiles))
                                          for (i in seq_along(quantiles)) {
                                            weighted_forecasts[, i] <- prob_forecast[, i] * final_weights[i] + final_bias
                                          }
                                          
                                          
                                          # Align the mean of the final forecast distribution with the point forecast
                                          
                                          final_means <- rowMeans(weighted_forecasts)
                                          adjustment_factor <- point_forecast / final_means
                                          weighted_forecasts <- sweep(weighted_forecasts, 1, adjustment_factor, FUN = "*")
                                          
                                          
                                          # Add column names
                                          
                                          colnames(weighted_forecasts) <- paste0('Q', seq_len(ncol(weighted_forecasts)))
                                          
                                          
                                          # create the tibble
                                          
                                          pred_hybrid_q <- point_id %>%
                                            select(-c(demographic_forecast, lgbm, xgb, rf, mean_pred)) %>%
                                            bind_cols(weighted_forecasts)
                                          
                                          
                                          # create the forecast distributions
                                          
                                          # Interpolation function to generate the final forecast distribution
                                          
                                          interpolate_points <- function(row_quantiles, quantiles, n_points = 1022) {
                                            approx(x = quantiles, y = row_quantiles, xout = seq(0, 1, length.out = n_points))$y
                                          }
                                          
                                          
                                          # Apply the interpolation function row-wise
                                          
                                          distribution_points <- t(apply(weighted_forecasts, 1, interpolate_points, quantiles = quantiles))
                                          
                                          
                                          # Create column names for the distribution points
                                          
                                          colnames(distribution_points) <- paste0('X', seq_len(ncol(distribution_points)))
                                          
                                          
                                          # Bind the distribution points to the original data
                                          
                                          pred_hybrid <- pred_hybrid_q %>%
                                            bind_cols(as.data.frame(distribution_points)) %>%
                                            select(where(function(x) !all(is.na(x)))) %>%
                                            select(-contains("Q")) %>%
                                            mutate(prediction = rowMeans(across(starts_with("X")))) %>%
                                            rename_with(~ paste0("X", as.integer(str_extract(., "\\d+")) - 12), starts_with("X"))
                                          
                                          pred_hybrid
                                          
                                        })


# Stop outer cluster

stopCluster(cl_outer)

write_rds(hybrid_prob_pred, 'data/predictions/hybrid_prob_pred_cv_new_fh.rds')



# old logic ---------------------------------------------------------------

# old method - where i did not use a bias factor to adjust the distribution

# Define quantile levels

quantiles <- quantiles <- seq(0.01, 0.99, by = 0.01)


# CQRA function

cqra_loss <- function(weights, point_forecast, prob_forecast, quantiles) {
  # Initialize an empty matrix to store weighted forecasts for each quantile
  weighted_forecasts <- matrix(NA, nrow(prob_forecast), length(quantiles))

  # Calculate the weighted forecast for each quantile
  for (i in seq_along(quantiles)) {
    weighted_forecasts[, i] <- prob_forecast[, i] * weights[i]
  }

  # Calculate the pinball loss for each quantile
  pinball_losses <- abs(point_forecast - rowMeans(weighted_forecasts))

  for (i in seq_along(quantiles)) {
    pinball_losses <- pinball_losses + pmax(0, (point_forecast - weighted_forecasts[, i]) * (quantiles[i] - (point_forecast < weighted_forecasts[, i])))
  }

  # Sum of pinball losses
  return(sum(pinball_losses))
}


# filter ids

id_list <- ml_ensemble_pred %>%
  select(horizon) %>%
  unique() %>%
  pull()


# Register outer parallel backend with 6 cores

cl_outer <- makeCluster(6)
registerDoParallel(cl_outer)

system.time(hybrid_prob_pred_no_bias <- foreach(i = id_list,
                                           .combine = 'rbind',
                                           .packages=c("doParallel", "foreach", "tidyverse", "quantreg", "tsibble")) %dopar% {

                                             #i = id_list[4]

                                             point_id <- demo_ensemble_point_pred %>% filter(horizon == i)
                                             prob_id <- ml_ensemble_pred %>% filter(horizon == i) %>% select(-c(yearmonth, id, horizon, prediction))


                                             # Extract point forecasts and probabilistic forecasts

                                             point_forecast <- point_id$mean_pred
                                             prob_forecast <- as.matrix(prob_id)


                                             # Initial weights (equal weights for each quantile)

                                             initial_weights <- rep(1 / length(quantiles), length(quantiles))


                                             # Minimize the CQRA loss function

                                             result <- optim(initial_weights, cqra_loss, method = "L-BFGS-B", point_forecast = point_forecast,
                                                                         prob_forecast = prob_forecast, quantiles = quantiles, lower = rep(0, length(quantiles)), upper = rep(1, length(quantiles)))

                                             # Final weights

                                             final_weights <- result$par

                                             # Calculate the weighted probabilistic forecasts for each quantile

                                             weighted_forecasts <- matrix(NA, nrow(prob_forecast), length(quantiles))

                                             # Calculate the weighted forecast for each quantile

                                             for (i in seq_along(quantiles)) {
                                               weighted_forecasts[, i] <- prob_forecast[, i] * final_weights[i]
                                             }

                                             #  add col names

                                             colnames(weighted_forecasts) <- paste0('Q', seq_len(ncol(weighted_forecasts)))

                                             # create the tibble

                                             pred_hybrid_q <- point_id %>%
                                               select(-c(demographic_forecast, lgbm, xgb, rf, mean_pred)) %>%
                                               bind_cols(weighted_forecasts)

                                             # create the forecast distributions

                                             # Function to interpolate 1000 points from quantiles

                                             interpolate_points <- function(row_quantiles, quantiles, n_points = 1022) {
                                               approx(x = quantiles, y = row_quantiles, xout = seq(0, 1, length.out = n_points))$y
                                             }


                                             # Apply the interpolation function row-wise

                                             distribution_points <- t(apply(weighted_forecasts, 1, interpolate_points, quantiles = quantiles))


                                             # Create column names for the distribution points

                                             colnames(distribution_points) <- paste0('X', seq_len(ncol(distribution_points)))


                                             # Bind the distribution points to the original data

                                             pred_hybrid <- pred_hybrid_q %>%
                                               bind_cols(as.data.frame(distribution_points)) %>%
                                               select(where(function(x) !all(is.na(x)))) %>%
                                               select(-contains("Q")) %>%
                                               mutate(prediction = rowMeans(across(starts_with("X")))) %>%
                                               rename_with(~ paste0("X", as.integer(str_extract(., "\\d+")) - 12), starts_with("X"))

                                             pred_hybrid

                                           })



# Stop outer cluster

stopCluster(cl_outer)

write_rds(hybrid_prob_pred_no_bias, 'data/predictions/hybrid_prob_pred_no_bias_cv.rds')
