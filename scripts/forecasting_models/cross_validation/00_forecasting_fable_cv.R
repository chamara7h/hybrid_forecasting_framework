# forecasting using FPP3

library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(doParallel)
library(foreach)
library(dygraphs)
library(tsbox)


# data preparation --------------------------------------------------------

# read data

stock <- read.csv('data/tidy_data/stock_tsb_zero_remove.csv') %>% 
  select(-X)


# make the tsibble

stock_tsb <- stock %>% 
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  as_tsibble(index = yearmonth, key = c(region, district, site_code, product_type, product_code))


# creating training set

stock_tsb_train <- stock_tsb %>% 
    filter_index(. ~ '2019 Sep') 


# create tscv train data

f_horizon = 3

stock_tsb_train_tscv <- stock_tsb_train %>% 
  stretch_tsibble(.init = (length(unique(stock_tsb_train$yearmonth)) - f_horizon + 1), .step = 1) %>% 
  # mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id)


# sNaive ------------------------------------------------------------------

# id list

id_list <- stock_tsb_train_tscv %>% pull(id) %>% unique()


# Register outer parallel backend with 5 cores

cl_outer <- makeCluster(5)
registerDoParallel(cl_outer)


# parallel loop

system.time(snaive_prob_pred <- foreach(i = id_list,
                              .combine = 'rbind',
                              .packages=c("doParallel", "foreach", "tidyverse", "fpp3")) %dopar% {
                                
                                # i <- id_list[1]
                                
                                # train sets
                                
                                train_data <- stock_tsb_train_tscv %>% 
                                  filter(id == i)

                                
                                # model fitting
                                
                                snaive_fit <- train_data %>% 
                                  model(snaive = SNAIVE(stock_distributed))
                                
                                
                                # forecasting
                                
                                fc_horizon <- 3
                                
                                boot <- 1000
                                
                                snaive_fc <- snaive_fit %>%
                                  fabletools::forecast(h = fc_horizon, bootstrap = TRUE, times = boot)
                                
                                snaive_bs <- data.frame(matrix(nrow = nrow(snaive_fc), ncol = boot))
                                
                                for (n in 1:nrow(snaive_fc)) {
                                  
                                  snaive_bs[n, ] <-  snaive_fc %>% pull(stock_distributed) %>% nth(n) %>% purrr::pluck("x")
                                  
                                }
                                
                                # preparing snaive bootstrap 
                                
                                snaive_pred_bs <- snaive_fc %>%
                                  as_tibble() %>% 
                                  select(yearmonth, site_code, product_code, horizon) %>% 
                                  bind_cols(snaive_bs)
                                
                                snaive_pred_bs
                                
                              })

# Stop outer cluster

stopCluster(cl_outer)

write_rds(snaive_prob_pred, 'data/predictions/snaive_prob_cv.rds')


# Moving Average ----------------------------------------------------------


# id list

id_list <- stock_tsb_train_tscv %>% pull(id) %>% unique()


# Register outer parallel backend with 5 cores

cl_outer <- makeCluster(5)
registerDoParallel(cl_outer)


# parallel loop

system.time(ma_prob_pred <- foreach(i = id_list,
                                        .combine = 'rbind',
                                        .packages=c("doParallel", "foreach", "tidyverse", "fpp3")) %dopar% {
                                          
                                          # i <- id_list[1]
                                          
                                          # train sets
                                          
                                          train_data <- stock_tsb_train_tscv %>% 
                                            filter(id == i)
                                          
                                          
                                          # model fitting
                                          
                                          ma_fit <- train_data %>% 
                                            model(moving_average = MEAN(stock_distributed ~ window(size = 3)))
                                          
                                          
                                          # forecasting
                                          
                                          fc_horizon <- 3
                                          
                                          boot <- 1000
                                          
                                          ma_fc <- ma_fit %>%
                                            fabletools::forecast(h = fc_horizon, bootstrap = TRUE, times = boot)
                                          
                                          ma_bs <- data.frame(matrix(nrow = nrow(ma_fc), ncol = boot))
                                          
                                          for (n in 1:nrow(ma_fc)) {
                                            
                                            ma_bs[n, ] <-  ma_fc %>% pull(stock_distributed) %>% nth(n) %>% purrr::pluck("x")
                                            
                                          }
                                          
                                          # preparing ma bootstrap 
                                          
                                          ma_pred_bs <- ma_fc %>%
                                            as_tibble() %>% 
                                            select(yearmonth, site_code, product_code, horizon) %>% 
                                            bind_cols(ma_bs)
                                          
                                          ma_pred_bs
                                          
                                        })

# Stop outer cluster

stopCluster(cl_outer)

write_rds(ma_prob_pred, 'data/predictions/ma_prob_cv.rds')



# ARIMA -------------------------------------------------------------------

# id list

id_list <- stock_tsb_train_tscv %>% pull(id) %>% unique()


# Register outer parallel backend with 5 cores

cl_outer <- makeCluster(5)
registerDoParallel(cl_outer)


# parallel loop

system.time(arima_prob_pred <- foreach(i = id_list,
                                    .combine = 'rbind',
                                    .packages=c("doParallel", "foreach", "tidyverse", "fpp3")) %dopar% {
                                      
                                      # i <- id_list[1]
                                      
                                      # train sets
                                      
                                      train_data <- stock_tsb_train_tscv %>% 
                                        filter(id == i)
                                      
                                      
                                      # model fitting
                                      
                                      arima_fit <- train_data %>% 
                                        model(arima = ARIMA(stock_distributed))
                                      
                                      
                                      # forecasting
                                      
                                      fc_horizon <- 3
                                      
                                      boot <- 1000
                                      
                                      arima_fc <- arima_fit %>%
                                        fabletools::forecast(h = fc_horizon, bootstrap = TRUE, times = boot)
                                      
                                      arima_bs <- data.frame(matrix(nrow = nrow(arima_fc), ncol = boot))
                                      
                                      for (n in 1:nrow(arima_fc)) {
                                        
                                        arima_bs[n, ] <-  arima_fc %>% pull(stock_distributed) %>% nth(n) %>% purrr::pluck("x")
                                        
                                      }
                                      
                                      # preparing arima bootstrap 
                                      
                                      arima_pred_bs <- arima_fc %>%
                                        as_tibble() %>% 
                                        select(yearmonth, site_code, product_code, horizon) %>% 
                                        bind_cols(arima_bs)
                                      
                                      arima_pred_bs
                                      
                                    })

# Stop outer cluster

stopCluster(cl_outer)

write_rds(arima_prob_pred, 'data/predictions/arima_prob_cv.rds')


# ETS ---------------------------------------------------------------------

# id list

id_list <- stock_tsb_train_tscv %>% pull(id) %>% unique()


# Register outer parallel backend with 5 cores

cl_outer <- makeCluster(5)
registerDoParallel(cl_outer)


# parallel loop

system.time(
  ets_prob_pred <- foreach(i = id_list,
                           .combine = 'rbind',
                           .packages=c("doParallel", "foreach", "tidyverse", "fpp3")) %dopar% {
                             
                             # i <- id_list[1]
                             
                             # train sets
                             
                             train_data <- stock_tsb_train_tscv %>% 
                               filter(id == i)
                             
                             
                             # model fitting
                             
                             ets_fit <- train_data %>% 
                               model(ets = ETS(stock_distributed))
                             
                             
                             # forecasting
                             
                             fc_horizon <- 3
                             
                             boot <- 1000
                             
                             ets_fc <- ets_fit %>%
                               fabletools::forecast(h = fc_horizon, bootstrap = TRUE, times = boot)
                             
                             ets_bs <- data.frame(matrix(nrow = nrow(ets_fc), ncol = boot))
                             
                             for (n in 1:nrow(ets_fc)) {
                               
                               ets_bs[n, ] <-  ets_fc %>% pull(stock_distributed) %>% nth(n) %>% purrr::pluck("x")
                               
                             }
                             
                             # preparing ets bootstrap 
                             
                             ets_pred_bs <- ets_fc %>%
                               as_tibble() %>% 
                               select(yearmonth, site_code, product_code, horizon) %>% 
                               bind_cols(ets_bs)
                             
                             ets_pred_bs
                           }
)

# Stop outer cluster

stopCluster(cl_outer)

write_rds(ets_prob_pred, 'data/predictions/ets_prob_cv.rds')
  

# CROSTON -----------------------------------------------------------------

# id list

id_list <- stock_tsb_train_tscv %>% pull(id) %>% unique()


# Register outer parallel backend with 5 cores

cl_outer <- makeCluster(5)
registerDoParallel(cl_outer)


# parallel loop

system.time(
  croston_pred <- foreach(i = id_list,
                               .combine = 'rbind',
                               .packages=c("doParallel", "foreach", "tidyverse", "fpp3")) %dopar% {
                                 
                                 # i <- id_list[1]
                                 
                                 # train set
                                 
                                 train_data <- stock_tsb_train_tscv %>% 
                                   filter(id == i)
                                 
                                 
                                 # model fitting
                                 
                                 croston_fit <- train_data %>% 
                                   model(croston = CROSTON(stock_distributed, type = 'sba'))


                                # forecasting
                                
                                fc_horizon <- 3
                                
                                croston_fc <- croston_fit %>%
                                  fabletools::forecast(h = fc_horizon)
                                
                                croston_predict <- croston_fc %>% 
                                  as_tibble() %>% 
                                  rename(prediction = .mean) %>% 
                                  select(yearmonth, site_code, product_code, horizon, prediction)
                                
                                croston_predict}
)

# Stop outer cluster

stopCluster(cl_outer)

write_rds(croston_pred, 'data/predictions/croston_pred_cv.rds')


