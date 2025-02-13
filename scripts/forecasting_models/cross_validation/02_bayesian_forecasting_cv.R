# Bayesian probabilistic modelling

library(tidyverse)
library(brms)
library(bsts)
library(tsibble)
library(doParallel)
library(foreach)


# Data_preparation --------------------------------------------------------

# read data

stock_tsb <- read.csv('data/tidy_data/new_predictors.csv') %>% 
  select(-unique_id) %>% 
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  as_tsibble(index = yearmonth, key = c(region, district, site_code, site_type, product_type, product_code))


demo_tsb <- read.csv('data/tidy_data/demo_predictors.csv') %>% 
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  select(!c(unique_id, zero_per, lag_1, lag_2, lag_3, lag_4, lag_rolling_mean_1234, rolling_max_4, rolling_zero_per_4)) %>% 
  as_tsibble(index = yearmonth, key = c(region, district, site_code, site_type, product_type, product_code)) %>% 
  mutate(method_mix_pct = method_mix / 100,
         mcpr_20_pct = mcpr_20 / 100,
         mcpr_25_pct = mcpr_25 / 100,
         mcpr_30_pct = mcpr_30 / 100,
         mcpr_35_pct = mcpr_35 / 100,
         mcpr_40_pct = mcpr_40 / 100,
         mcpr_45_pct = mcpr_45 / 100,
         mcpr_50_pct = mcpr_50 / 100,
         interaction_20 = cyp_per_unit * brand_mix * market_share_per * method_mix_pct * weight_dis * age_20 * mcpr_20_pct,
         interaction_25 = cyp_per_unit * brand_mix * market_share_per * method_mix_pct * weight_dis * age_25 * mcpr_25_pct,
         interaction_30 = cyp_per_unit * brand_mix * market_share_per * method_mix_pct * weight_dis * age_30 * mcpr_30_pct,
         interaction_35 = cyp_per_unit * brand_mix * market_share_per * method_mix_pct * weight_dis * age_35 * mcpr_35_pct,
         interaction_40 = cyp_per_unit * brand_mix * market_share_per * method_mix_pct * weight_dis * age_40 * mcpr_40_pct,
         interaction_45 = cyp_per_unit * brand_mix * market_share_per * method_mix_pct * weight_dis * age_45 * mcpr_45_pct,
         interaction_50 = cyp_per_unit * brand_mix * market_share_per * method_mix_pct * weight_dis * age_50 * mcpr_50_pct) %>% 
  select(id, stock_distributed, year, month,interaction_20, interaction_25, interaction_30, interaction_35, interaction_40, interaction_45, interaction_50)
  
  

# create train and test data

train_tsb <- stock_tsb %>% 
  filter_index(. ~ '2019 Sep')

test_tsb <- stock_tsb %>% 
  filter_index('2019 Aug' ~ .)

demo_train_tsb <- demo_tsb %>% 
  filter_index(. ~ '2019 Sep')

demo_test_tsb <- demo_tsb %>% 
  filter_index('2019 Aug' ~ .)


# create tscv train data

f_horizon = 3

train_tsb_tscv <- train_tsb %>% 
  stretch_tsibble(.init = (length(unique(train_tsb$yearmonth)) - f_horizon + 1), .step = 1) %>% 
  mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id)

test_tsb_tscv <- test_tsb %>% 
  slide_tsibble(.size = 3, .step = 1) %>% 
  mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id)

demo_train_tsb_tscv <- demo_train_tsb %>% 
  stretch_tsibble(.init = (length(unique(train_tsb$yearmonth)) - f_horizon + 1), .step = 1) %>% 
  mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id)

demo_test_tsb_tscv <- demo_test_tsb %>% 
  slide_tsibble(.size = 3, .step = 1) %>% 
  mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id)


# Bayesian_demographic_linear_regression ----------------------------------------------

# id list

id_list <- demo_tsb %>% pull(id) %>% unique()


# Register outer parallel backend with 5 cores

cl_outer <- makeCluster(5)
registerDoParallel(cl_outer)


# parallel loop

blr_demo_prob_pred <- foreach(i = id_list,
                          .combine = 'rbind',
                          .packages=c("doParallel", "foreach", "tidyverse", "brms", "bsts")) %dopar% {
                            
                            # train and test sets
                            
                            train_data <- demo_train_tsb %>% 
                              filter(id == i) %>% 
                              as_tibble()
                            
                            test_data <- demo_test_tsb %>% 
                              filter(id == i) %>% 
                              as_tibble()
                            
                            # Define the model formula including lagged variables, rolling stats, year, and month
                            
                            formula <- bf(
                              stock_distributed ~ 
                                interaction_20 + interaction_25 + interaction_30 + 
                                interaction_35 + interaction_40 + interaction_45 + 
                                interaction_50 +
                                year + month)
                            # +    (1 | region/district/site_code/site_type/product_code))
                            
                            
                            # Define priors
                            
                            priors <- c(
                              prior(normal(0, 1), class = "b")  # Weakly informative for fixed effects
                              # prior(normal(0, 1), class = "Intercept"),  # Weakly informative for intercept
                              # prior(student_t(3, 0, 10), class = "sd")  # Weakly informative for random effects
                            )
                            
                            
                            # Fit the Bayesian model
                            
                            blr_demo_fit <- brm(
                              formula = formula,
                              data = train_data,
                              family = negbinomial(),
                              prior = priors,
                              iter = 2000, 
                              warmup = 1000, 
                              chains = 4, 
                              cores = 4,
                              control = list(adapt_delta = 0.99, max_treedepth = 20),
                              seed = 123
                            )
                            
                            
                            # Predict on the test set
                            
                            blr_demo_predictions <- predict(blr_demo_fit, newdata = test_data %>% 
                                                              select(-stock_distributed),
                                                            summary = FALSE, ndraws = 1000)
                            
                            
                            # Convert predictions to a data frame with columns X0 to X999
                            
                            pred_df <- t(as.data.frame(blr_demo_predictions))
                            colnames(pred_df) <- paste0("X", 0:999)
                            
                            
                            # Combine predictions with actual data
                            
                            blr_demo_prob <- test_data %>% 
                              as_tibble() %>% 
                              select(yearmonth, id, stock_distributed) %>% 
                              mutate(model = 'blr') %>% 
                              bind_cols(as_tibble(pred_df))
                            
                            
                            blr_demo_prob
                            
                          }

# Stop outer cluster

stopCluster(cl_outer)


write_rds(blr_demo_prob_pred, 'data/predictions/blr_demo_prob_pred.rds')

# Bayesian_state_space_model (with demographic regressors)----------------------------------

id_list <- demo_train_tsb_tscv %>% pull(unique_id) %>% unique()


# Register outer parallel backend with 6 cores

cl_outer <- makeCluster(6)
registerDoParallel(cl_outer)


# parallel loop

system.time(bsts_demo_prob_pred <- foreach(i = id_list,
                                           .combine = 'rbind',
                                           .packages=c("doParallel", "foreach", "tidyverse", "brms", "bsts")) %dopar% {
                                             
                                             # train and test sets
                                             
                                             # i <- id_list[1]
                                             
                                             train_data <- demo_train_tsb_tscv %>% 
                                               filter(unique_id == i) %>% 
                                               as_tibble()
                                             
                                             test_data <- demo_test_tsb_tscv %>% 
                                               filter(unique_id == i) %>% 
                                               select(-stock_distributed) %>% 
                                               as_tibble()
                                             
                                             
                                             # Define the state-space model with a local linear trend and seasonal component
                                             
                                             ss <- AddLocalLinearTrend(list(), train_data$stock_distributed)
                                             ss <- AddSeasonal(ss, train_data$stock_distributed, nseasons = 12)
                                             
                                             
                                             # Fit the model with 1000 iterations
                                             
                                             bsts_model <- bsts(stock_distributed ~ interaction_20 + interaction_25 + interaction_30 + 
                                                                  interaction_35 + interaction_40 + interaction_45 + interaction_50 +
                                                                  year + month, 
                                                                state.specification = ss, 
                                                                niter = 2000, 
                                                                data = train_data,
                                                                seed = 123)
                                             
                                             
                                             # Generate forecasts for the next 3 months
                                             
                                             bsts_pred <- predict(bsts_model, horizon = 3, burn = 1000, newdata = (test_data %>% 
                                                                                                                     select(interaction_20, interaction_25, interaction_30,
                                                                                                                            interaction_35, interaction_40, interaction_45, interaction_50,
                                                                                                                            year, month)))
                                             
                                             # Extract 1000 sample points for each forecasted date and convert to dataframe
                                             
                                             bsts_prob <- as.data.frame(t(bsts_pred$distribution))
                                             colnames(bsts_prob) <- paste0("X", 0:999)
                                             
                                             # Combine the forecast samples with the test dates
                                             
                                             bsts_prob_pred_loop <- demo_test_tsb_tscv %>% 
                                               filter(unique_id == i) %>% 
                                               as_tibble() %>% 
                                               select(yearmonth, id, horizon, stock_distributed) %>% 
                                               mutate(model = 'bsts_demo') %>% 
                                               bind_cols(bsts_prob)
                                             
                                             bsts_prob_pred_loop
                                             
                                           })

# Stop outer cluster

stopCluster(cl_outer)


write_rds(bsts_demo_prob_pred, 'data/predictions/bsts_demo_prob_pred_cv.rds')


# Bayesian_state_space_model (with regressors)----------------------------------

id_list <- train_tsb_tscv %>% pull(unique_id) %>% unique()


# Register outer parallel backend with 6 cores

cl_outer <- makeCluster(6)
registerDoParallel(cl_outer)


# parallel loop

system.time(bsts_prob_pred <- foreach(i = id_list,
                                      .combine = 'rbind',
                                      .packages=c("doParallel", "foreach", "tidyverse", "brms", "bsts")) %dopar% {
                                        
                                        # train and test sets
                                        
                                        train_data <- train_tsb_tscv %>% 
                                          filter(unique_id == i) %>% 
                                          as_tibble()
                                        
                                        test_data <- test_tsb_tscv %>% 
                                          filter(unique_id == i) %>% 
                                          select(-stock_distributed) %>% 
                                          as_tibble()
                                        
                                        
                                        # Define the state-space model with a local linear trend and seasonal component
                                        
                                        ss <- AddLocalLinearTrend(list(), train_data$stock_distributed)
                                        ss <- AddSeasonal(ss, train_data$stock_distributed, nseasons = 12)
                                        
                                        
                                        # Fit the model with 1000 iterations
                                        
                                        bsts_model <- bsts(stock_distributed ~ lag_1 + lag_2 + lag_3 + lag_4 + 
                                                             lag_rolling_mean_1234 + rolling_max_4 + rolling_zero_per_4 +
                                                             year + month, 
                                                           state.specification = ss, 
                                                           niter = 2000, 
                                                           data = train_data,
                                                           seed = 123)
                                        
                                        
                                        # Generate forecasts for the next 3 months
                                        
                                        bsts_pred <- predict(bsts_model, horizon = 3, burn = 1000, newdata = (test_data %>% 
                                                                                                                select(lag_1, lag_2, lag_3, lag_4, 
                                                                                                                       lag_rolling_mean_1234, rolling_max_4, rolling_zero_per_4,
                                                                                                                       year, month)))
                                        
                                        # Extract 1000 sample points for each forecasted date and convert to dataframe
                                        
                                        bsts_prob <- as.data.frame(t(bsts_pred$distribution))
                                        colnames(bsts_prob) <- paste0("X", 0:999)
                                        
                                        # Combine the forecast samples with the test dates
                                        
                                        bsts_prob_pred_loop <- test_tsb_tscv %>% 
                                          filter(unique_id == i) %>% 
                                          as_tibble() %>% 
                                          select(yearmonth, id, horizon, stock_distributed) %>% 
                                          mutate(model = 'bsts') %>% 
                                          bind_cols(bsts_prob)
                                        
                                        bsts_prob_pred_loop
                                        
                                      })

# Stop outer cluster

stopCluster(cl_outer)


write_rds(bsts_prob_pred, 'data/predictions/bsts_prob_pred_cv.rds')



