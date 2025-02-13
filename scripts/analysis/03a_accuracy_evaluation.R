## accuracy evaluation

library(tidyverse)
library(tsibble)
library(Metrics)
library(SpecsVerification)



# read_data ---------------------------------------------------------------

bs_pred_point <- read_rds('data/predictions/bs_predictions_all_point.rds')

bs_pred_prob <- read_rds("data/predictions/bs_predictions_all_prob.rds")

bs_pred_q_full <- read_rds('data/predictions/bs_predictions_all_q.rds')



# accuracy_evaluation_point forecasts -------------------------------------

# training data

fitted_data <- read.csv('data/predictions/ml_fitted_filtered.csv')


train_df <- fitted_data %>% 
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  select(yearmonth, id, stock_distributed) %>% 
  as_tsibble(index = yearmonth, key = id)


# create id list and model list

id <- unique(train_df$id)
model <- unique(bs_pred_point$model)


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
        forecast = bs_pred_point %>% filter(id == i, model == m) %>% pull(prediction)
        train = train_df %>% filter(id == i) %>% pull(stock_distributed)
        test = bs_pred_point %>% filter(id == i, model == m) %>% pull(stock_distributed)
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
    group_by(model) %>% 
    summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmse = mean(rmse), median_rmse = median(rmse))

write_rds(fc_accuray, 'data/results/fc_accuray.rds')



# accuracy_evaluation_probabilistic_forecast ------------------------------


bs_crps <- tibble(
  id = c('test'),
  model = c('test'),
  crps = c(0)
)

model <- unique(bs_pred_prob$model)


for (m in model) {
    for (i in id) {
        forecast = bs_pred_prob %>% filter(id == i, model == m) %>% 
          select(-yearmonth, -id, -model, stock_distributed)
    
        actual = bs_pred_prob %>% 
            filter(id == i, model == m) %>% 
            pull(stock_distributed)
          
        mean = forecast %>% rowwise() %>% mutate(mean = mean(c_across(starts_with("X")), na.rm = TRUE)) %>% pull(mean)
          
        sd = forecast %>% rowwise() %>% mutate(sd = sd(c_across(starts_with("X")), na.rm = TRUE)) %>% pull(sd)
          
        crps = mean(GaussCrps(mean, sd, actual))
          
        fc <- tibble(
            id = i,
            model = m,
            crps = crps)
          
        bs_crps = bind_rows(bs_crps, fc)
    
  }
  
}

# remove the first row

bs_crps = bs_crps[-1,]


# calculate the mase summary

bs_crps_summary <- bs_crps %>% 
    group_by(model) %>% 
    summarise(mean_crps = mean(crps), median_crps = median(crps))


write_rds(bs_crps, 'data/results/bs_crps.rds')



# accuracy_evaluation_quantile_forecast -----------------------------------

# pin ball loss evaluation

# Calculate pinball loss for each quantile

pinball_loss <- function(y, qa, a) {
  if (y < qa) {
    return (1 - a/100) * (qa - y)
  } else {
    return (a/100) * (y - qa)
  }
}


# create id list and model list

#id <- unique(bs_pred_q_full$id)
model <- unique(bs_pred_q_full$model)


qnt_accuray <- tibble(
  id = c('test'),
  model = c('test'),
  pinball_loss = c(0)
)


for (i in id) {
  for (m in model) {
    data <- bs_pred_q_full %>% filter(id == i, model == m)
    
    # Calculate average pinball loss
    average_pinball_loss <- function(data) {
      total_loss <- 0
      num_periods <- length(unique(data$yearmonth))
      num_quantiles <- length(grep("^Q", colnames(data), value = TRUE))
      
      for (t in 1:num_periods) {
        for (a in seq(0.01, 0.99, by = 0.01)) {
          col_name <- c(paste0("Q", as.character(a * 100)))
          total_loss <- total_loss + sum(pinball_loss(data$stock_distributed[t], data[[col_name]][t], a))
        }
      }
      
      return(total_loss / (num_periods * num_quantiles))
    }
    
    # Calculate average pinball loss for your data frame
    average_loss <- average_pinball_loss(data)
    
    fc <- tibble(
      id = i,
      model = m,
      pinball_loss = average_loss)
    
    qnt_accuray <- bind_rows(qnt_accuray, fc)
    
  }
  
}


# remove the first row

qnt_accuray = qnt_accuray[-1,]


# calculate the mase summary

qnt_accuray_summary <- qnt_accuray %>% 
  group_by(model) %>% 
  summarise(mean_pinball_loss = mean(pinball_loss), median_pin_ball_loss = median(pinball_loss))

write_rds(qnt_accuray, 'data/results/qnt_accuray.rds')








