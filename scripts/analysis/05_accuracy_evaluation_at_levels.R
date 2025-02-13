## accuracy measures at aggregrate levels

library(tidyverse)
library(tsibble)
library(ggdist)
library(Metrics)
library(SpecsVerification)


# read data

product_master <- read.csv('data/product.csv')


pred_point <- read_rds('data/predictions/bs_predictions_all_point.rds') %>% 
  
  filter(model != 'ensemble_ml_bs' , model != 'ensemble_demo_bs' , model != 'mlr_all' , model != 'lgbm_all' , model != 'xgb_all' , model != 'rf_all' ,
         model != 'mlr_filtered' , model != 'lgbm_filtered' , model != 'xgb_filtered' , model != 'rf_filtered' ,
         model != 'mlr_demo' , model != 'lgbm_demo' , model != 'lgbm_demo' , model != 'rf_demo' , model != 'xgb_demo',
         model != 'hybrid_pred_bs' , model != 'hybrid_optimise_bs' , model != 'hybrid_pred_sk' ,
         model != 'lgbm_bs' , model != 'xgb_bs' , model != 'rf_bs' , model != 'mlr_bs', model != 'demographic_prob',
         model != 'hybrid_scaled', model != 'lgbm_bottom_up', model != 'lgbm_min_trace_method_ols', model != 'lgbm_bottom_up_all',
         model != 'lgbm_min_trace_method_ols_all', model != 'lgbm_min_trace_method_mint_shrink_all') %>% 
  
  mutate(site_code = substr(id, 1, 5), product_code = substr(id, 6, nchar(id))) %>% 
  
  drop_na() %>% 
  
  left_join(product_master, by = 'product_code')


pred_prob <- read_rds('data/predictions/bs_predictions_all_prob.rds') %>% 
  
  filter(model != 'ensemble_ml_bs' , model != 'ensemble_demo_bs' , model != 'hybrid_bs' , 
         model != 'lgbm_bs' , model != 'xgb_bs' , model != 'rf_bs' , model != 'mlr_bs' , model != 'demographic_prob') %>% 
  
  mutate(site_code = substr(id, 1, 5), product_code = substr(id, 6, nchar(id))) %>% 
  
  drop_na() %>% 
  
  left_join(product_master, by = 'product_code')



# training data


fitted_data <- read.csv('data/predictions/ml_fitted_filtered.csv')


train_df <- fitted_data %>% 
  
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  
  select(yearmonth, id, stock_distributed) %>% 
  
  as_tsibble(index = yearmonth, key = id) %>% 
  
  mutate(site_code = substr(id, 1, 5), product_code = substr(id, 6, nchar(id))) %>% 
  
  left_join(product_master, by = 'product_code')



## point predictions evaluation

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



# quarterly product code

id_list <- train_df %>% as_tibble() %>% select(id) %>% unique() %>% pull()

model_list <- pred_point %>% select(model) %>% unique() %>% pull()


pred_point_qtr_product <- pred_point %>% # create product aggregrate at annual level - predictions
  
  mutate(quarter = yearquarter(yearmonth)) %>% 
  
  filter(id %in% id_list) %>% 
  
  group_by(quarter, id, model) %>% 
  
  summarise(prediction = sum(prediction), stock_distributed = sum(stock_distributed), .groups = 'drop') 


train_qtr_product  <- train_df %>% # create product aggregrate at annual level - train data
  
  mutate(quarter = yearquarter(yearmonth)) %>% 
  
  group_by(quarter, id) %>% 
  
  summarise(stock_distributed = sum(stock_distributed), .groups = 'drop') 



point_accuracy_annual_product <- tibble( # create a blank df
  id = c('test'),
  model = c('test'),
  stock_distributed = c(0),
  prediction = c(0),
  mase = c(0),
  rmse = c(0)
)

y = 1

for (i in id_list) {
  
  for (m in model_list) {
    
    forecast = pred_point_qtr_product %>% filter(id == i, model == m) %>% pull(prediction)
    
    train = train_qtr_product %>% filter(id == i) %>% pull(stock_distributed)
    
    test = pred_point_qtr_product %>% filter(id == i, model == m) %>% pull(stock_distributed)
    
    mase = computeMASE(forecast, train, test, 3)
    
    rmse = rmse(test, forecast)
    
    fc <- tibble(
      id = i,
      model = m,
      stock_distributed = test,
      prediction = forecast,
      mase = mase,
      rmse = rmse)
    
    point_accuracy_annual_product <- bind_rows(point_accuracy_annual_product, fc)
    
    print(y)
    
    y <- y + 1
    
  }
  
}


# remove the first row

point_accuracy_annual_product = point_accuracy_annual_product[-1,]


# calculate the mase summary

point_accuracy_annual_product_summary <- point_accuracy_annual_product %>% 
  
  filter(!is.na(mase), !is.na(rmse)) %>% 
  
  group_by(model) %>% 
  
  summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmse = mean(rmse), median_rmse = median(rmse))


write_rds(point_accuracy_annual_product, 'data/results/point_accuracy_annual.rds')  



## prob predictions evaluation  
  
pred_prob_annual_product <- pred_prob %>% # create product aggregrate at annual level - predictions
  
  mutate(quarter = yearquarter(yearmonth)) %>% 
  
  filter(id %in% id_list) %>% 
  
  group_by(quarter, id, model) %>% 
  
  summarise(across(where(is.double), ~ sum(.)), stock_distributed = sum(stock_distributed), .groups = 'drop')


bs_crps <- tibble(
  id = c('test'),
  model = c('test'),
  stock_distributed = c(0),
  crps = c(0)
)

model_list <- unique(pred_prob_annual_product$model)

y <- 1

for (m in model_list) {
  
  for (i in id_list) {
    
    forecast = pred_prob_annual_product %>% filter(id == i, model == m) %>% 
      select(-yearmonth, -id, -model)
    
    actual = pred_prob_annual_product %>% 
      filter(id == i, model == m) %>% 
      pull(stock_distributed)
    
    mean = forecast %>% rowwise() %>% mutate(mean = mean(c_across(starts_with("X")), na.rm = TRUE)) %>% pull(mean)
    
    sd = forecast %>% rowwise() %>% mutate(sd = sd(c_across(starts_with("X")), na.rm = TRUE)) %>% pull(sd)
    
    crps = mean(GaussCrps(mean, sd, actual))
    
    fc <- tibble(
      id = i,
      model = m,
      stock_distributed = actual,
      crps = crps)
    
    bs_crps = bind_rows(bs_crps, fc)
    
    print(y)
    
    y <- y+1
    
  }
  
}

# remove the first row

bs_crps = bs_crps[-1,]


# calculate the mase summary

prob_crps_summary_annual <- bs_crps %>% 
  
  group_by(model) %>% 
  
  summarise(mean_crps = mean(crps), median_crps = median(crps))


write_rds(bs_crps, 'data/results/prob_crps_annual.rds')



# quarterly product type

product_type_list <- train_df %>% as_tibble() %>% select(product_type) %>% unique() %>% pull()

model_list <- pred_point %>% select(model) %>% unique() %>% pull()


pred_point_qtr_product_type <- pred_point %>% # create product aggregrate at annual level - predictions
  
  # mutate(quarter = yearquarter(yearmonth)) %>% 
  
  filter(product_type %in% product_type_list) %>% 
  
  group_by(yearmonth, product_type, model) %>% 
  
  summarise(prediction = sum(prediction), stock_distributed = sum(stock_distributed), .groups = 'drop') 


train_qtr_product  <- train_df %>% # create product aggregrate at annual level - train data
  
  # mutate(quarter = yearquarter(yearmonth)) %>% 
  
  as_tibble() %>% 
  
  group_by(yearmonth, product_type) %>% 
  
  summarise(stock_distributed = sum(stock_distributed), .groups = 'drop') 



point_accuracy_qtr_product_type <- tibble( # create a blank df
  product_type = c('test'),
  model = c('test'),
  stock_distributed = c(0),
  prediction = c(0),
  mase = c(0),
  rmse = c(0)
)

y = 1

for (i in product_type_list) {
  
  for (m in model_list) {
    
    forecast = pred_point_qtr_product_type %>% filter(product_type == i, model == m) %>% pull(prediction)
    
    train = train_qtr_product %>% filter(product_type == i) %>% pull(stock_distributed)
    
    test = pred_point_qtr_product_type %>% filter(product_type == i, model == m) %>% pull(stock_distributed)
    
    mase = computeMASE(forecast, train, test, 3)
    
    rmse = rmse(test, forecast)
    
    fc <- tibble(
      product_type = i,
      model = m,
      stock_distributed = test,
      prediction = forecast,
      mase = mase,
      rmse = rmse)
    
    point_accuracy_qtr_product_type <- bind_rows(point_accuracy_qtr_product_type, fc)
    
    print(y)
    
    y <- y + 1
    
  }
  
}


# remove the first row

point_accuracy_qtr_product_type <- point_accuracy_qtr_product_type[-1,]


# calculate the mase summary

point_accuracy_qtr_product_type_summary <- point_accuracy_qtr_product_type %>% 
  
  filter(!is.na(mase), !is.na(rmse)) %>% 
  
  group_by(model) %>% 
  
  summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmse = mean(rmse), median_rmse = median(rmse))


write_rds(point_accuracy_annual_product, 'data/results/point_accuracy_annual.rds')  








