## evaluate stockout situations

library(tidyverse)


# read data

stock_tidy <- read_rds('data/tidy_data/stock_tidy.rds') %>% 
    mutate(id = paste0(site_code, product_code))


# calculate cumulative stockout days with reset

stock_tidy <- stock_tidy %>%
    group_by(id) %>%
    mutate(
    group = cumsum(stock_stockout_days == 0), # Create a group for each segment of non-zero stockout days
    cumulative_stockout_days = cumsum(if_else(stock_stockout_days > 0, stock_stockout_days, 0) * (cumsum(stock_stockout_days == 0) == 0)) # Cumulative sum within each group
  ) %>%
    group_by(id, group) %>%
    mutate(cumulative_stockout_days = if_else(stock_stockout_days == 0, 0, cumsum(stock_stockout_days))) %>%
    ungroup() %>%
    select(-group) # Remove the temporary group column


stock_tidy_so <-stock_tidy %>% 
    select(yearmonth, id, cumulative_stockout_days) %>% 
    as_tsibble(index = yearmonth, key = id)

write_rds(stock_tidy_so, 'data/tidy_data/stock_tidy_so.rds')









