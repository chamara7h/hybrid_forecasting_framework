# cross validation data preperation for LLMs

library(tidyverse)
library(fable)
library(tsibble)


# read data

stock <- read.csv('data/tidy_data/new_predictors.csv') 

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

stock_tsb_train_tscv <- stock_tsb_train %>% 
  stretch_tsibble(.init = (length(unique(stock_tsb_train$yearmonth)) - f_horizon + 1), .step = 1) %>% 
  mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id)

write.csv(stock_tsb_train_tscv %>% 
            as_tibble() %>% 
            select(-id), 
          'data/tidy_data/train_tscv.csv', row.names = FALSE)

# create tscv test data

stock_tsb_test_tscv <- stock_tsb_test %>% 
  slide_tsibble(.size = 3, .step = 1) %>% 
  mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id)

write.csv(stock_tsb_test_tscv %>% 
            as_tibble() %>% 
            select(-id, -stock_distributed), 
          'data/tidy_data/test_tscv.csv', row.names = FALSE)


# create tscv for lag llama

f_horizon <- 3

stock_tsb_tscv <- stock_tsb %>% 
  stretch_tsibble(.init = (length(unique(stock_tsb$yearmonth)) - f_horizon + 1), .step = 1) %>% 
  mutate(unique_id = paste0(id, '-', .id)) %>% 
  rename(horizon = .id)

write.csv(stock_tsb_tscv %>% 
            as_tibble() %>% 
            select(yearmonth, id, unique_id, horizon, stock_distributed), 
          'data/tidy_data/stock_tscv.csv', row.names = FALSE)
