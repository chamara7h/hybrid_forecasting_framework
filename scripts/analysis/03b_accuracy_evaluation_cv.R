# cross validation

library(tidyverse)
library(tsibble)
library(Metrics)
library(SpecsVerification)


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
  
# create tscv test data

stock_test_tscv <- stock_tsb_test %>% 
  slide_tsibble(.size = 3, .step = 1) %>% 
  rename(origin = .id) |> 
  as_tibble() |> 
  group_by(id, origin) |> 
  mutate(horizon = row_number()) |> # creating the horizon
  ungroup() |>
  mutate(unique_id = paste0(id, '-', horizon))


# Point forecast evaluation -----------------------------------------------

point_pred_cv <- read_rds('data/predictions/point_pred_cv.rds') %>% 
  rename(origin = horizon) |> 
  as_tibble() |> 
  group_by(model, id, origin) |> 
  mutate(horizon = row_number()) |> # creating the horizon
  ungroup() |>
  mutate(unique_id = paste0(id, '-', horizon))


# create id list and model list

# id <- unique(point_pred_cv %>%
#                filter(!str_starts(model, "cs_")) %>% 
#                pull(unique_id))

id <- unique(stock_test_tscv |> 
               pull(unique_id))

model <- unique(point_pred_cv$model)


## MASE calculation

computeMASE <- function(forecast,train,test,period){
  
  # forecast - forecasted values
  # train - data used for forecasting .. used to find scaling factor
  # test - actual data used for finding MASE. same length as forecast
  # period - in case of seasonal data.. if not, use 1
  
  forecast <- as.vector(forecast)
  train <- as.vector(train)
  test <- as.vector(test)
  
  n <- length(train)
  scalingFactor <- sum(abs(train[(period+1):n] - train[1:(n-period)])) / (n-period)
  
  et <- abs(test-forecast)
  qt <- et/scalingFactor
  meanMASE <- mean(qt)
  #medianMASE <- median(qt)
  return(meanMASE)
}


fc_accuray <- tibble(
  id = c('test'),
  model = c('test'),
  mase = c(0),
  rmse = c(0)
)

y = 1

for (i in id) {
  for (m in model) {
    forecast = point_pred_cv %>% filter(unique_id == i, model == m) %>% pull(prediction)
    train = stock_train_tscv %>% filter(unique_id == i) %>% pull(stock_distributed)
    test = stock_test_tscv %>% filter(unique_id == i) %>% pull(stock_distributed)
    mase = computeMASE(forecast, train, test, 12)
    rmse = rmse(test, forecast)
    
    fc <- tibble(
      id = i,
      model = m,
      mase = mase,
      rmse = rmse)
    
    fc_accuray <- bind_rows(fc_accuray, fc)
    
    print(y)
    
    y <- y + 1
    
  }
  
}


# remove the first row

fc_accuray = fc_accuray[-1,]


# calculate the mase summary

fc_accuray_summary <- fc_accuray %>% 
  filter(!is.na(mase), !is.na(rmse)) %>% 
  separate(id, into = c("id", "horizon"), sep = "-") %>%
  group_by(model) %>% 
  summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmse = mean(rmse), median_rmse = median(rmse))

write_rds(fc_accuray, 'data/results/fc_cv_accuray.rds')


# fc_accuray %>% 
#   filter(mase != 0) %>% 
#   rename(unique_id = id) %>% 
#   group_by(unique_id) %>%
#   mutate(rank = dense_rank(desc(mase))) %>% 
#   filter(rank == 1) %>% 
#   ungroup() %>% 
#   group_by(model) %>% 
#   summarise(observation_count = n())


# Prob prediction evaluation ----------------------------------------------


prob_pred_cv <- read_rds('data/predictions/prob_pred_cv.rds') %>% 
  rename(origin = horizon) |> 
  as_tibble() |> 
  group_by(model, id, origin) |> 
  mutate(horizon = row_number()) |> # creating the horizon
  ungroup() |>
  mutate(unique_id = paste0(id, '-', horizon))


# create id list and model list

# id <- unique(prob_pred_cv$unique_id)
model <- unique(prob_pred_cv$model)


bs_crps <- tibble(
  id = c('test'),
  model = c('test'),
  crps = c(0)
)

y <- 1

for (m in model) {
  for (i in id) {
    
    # m <- model[1]
    # id <- id[1]
    
    forecast = prob_pred_cv %>% filter(unique_id == i, model == m) %>% 
      select(-yearmonth, -id, -model, -prediction, -unique_id, -horizon)
    
    actual = stock_test_tscv %>% filter(unique_id == i) %>% pull(stock_distributed)
    
    mean = forecast %>% rowwise() %>% mutate(mean = mean(c_across(starts_with("X")), na.rm = TRUE)) %>% pull(mean)
    
    sd = forecast %>% rowwise() %>% mutate(sd = sd(c_across(starts_with("X")), na.rm = TRUE)) %>% pull(sd)
    
    crps = mean(GaussCrps(mean, sd, actual))
    
    fc <- tibble(
      id = i,
      model = m,
      crps = crps)
    
    bs_crps = bind_rows(bs_crps, fc)
    
    print(y)
    
    y <- y+1
    
  }
  
}

# remove the first row

bs_crps = bs_crps[-1,]


# calculate the mase summary

bs_crps_summary <- bs_crps %>% 
  group_by(model) %>% 
  summarise(mean_crps = mean(crps), median_crps = median(crps))


write_rds(bs_crps, 'data/results/bs_cv_crps.rds')




