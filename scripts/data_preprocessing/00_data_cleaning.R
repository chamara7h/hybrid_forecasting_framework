## Data cleaning

library(tsibble)
library(tidyverse)


# Read data

#stock_df <- read.csv('data/contraceptive_logistics_data.csv')

stock_df <- read.csv('data/stock_pnmse.csv')

site_df <- read.csv('data/service_delivery_site_data.csv') %>% 
    select(site_code, site_type, site_latitude, site_longitude)

#product_df <- read.csv('data/product.csv')


# Merge data

stock_tidy <- left_join(stock_df, site_df, by = 'site_code')
#stock_tidy <- left_join(stock_tidy, product_df, by = 'product_code')


# create date column

stock_tidy <- stock_tidy %>%
  mutate(yearmonth = paste0(year,"-", month)) %>% ##to create date column
  mutate(yearmonth = yearmonth(yearmonth)) %>%
  arrange(yearmonth) 

stock_tidy <- stock_tidy %>%
    rename(product_type = product_group) %>% 
    select(yearmonth, region, district, site_code, site_type, site_latitude, site_longitude, product_code, product_name, product_type,
         stock_initial, stock_received, stock_distributed,stock_adjustment, stock_end, average_monthly_consumption, stock_stockout_days, stock_ordered)


write_rds(stock_tidy, "data/tidy_data/stock_tidy.rds")


