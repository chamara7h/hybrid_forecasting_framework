# data wrangaling

library(tidyverse)
library(tsibble)


# read data

stock_df <- read_rds('data/tidy_data/stock_tidy.rds') |> 
  select(yearmonth, region, district, site_code, site_type, product_type, product_code, stock_distributed, stock_stockout_days) |>
  drop_na(site_type) |> ## remove sites without site types
  filter(stock_stockout_days == 0) |>
  select(-stock_stockout_days)


site_df <- read.csv("data/service_delivery_site_data.csv") |> 
  select(site_code, site_type)


# take a quick look

glimpse(stock_df)

summary(stock_df |> select(-yearmonth)) # stock_ordered has 839 missing values


# count number of unique values

stock_df |>
  select(region, district, site_code, product_type, product_code) |>
  summarise(across(everything(), ~ n_distinct(.))) 


# plot product count in each site

stock_df |>
  ggplot(aes(product_code)) +
  geom_bar() +
  geom_text(stat = "count" , aes(label=after_stat(count)), vjust=-1)+
  labs(x = "Product code", y = "Count") +
  theme_minimal()

# make a tsibble

stock_tsb <- stock_df |> 
  as_tsibble(index = yearmonth, key = c(region, district, site_code, product_type, product_code)) 


# check for gaps

gaps <- count_gaps(stock_tsb)

stock_tsb <- stock_tsb |> fill_gaps(.full = TRUE) |> 
  mutate_if(is.integer, ~replace_na(., 0)) |> 
  select(-site_type) |> 
  left_join(site_df, by = 'site_code') |> 
  ungroup() |> 
  mutate(unique_id = paste0(yearmonth, site_code, product_code),
         id = paste0(site_code, product_code))


# calculate 0 percentage in each series and count the zeros 

stock_tsb_zero_remove <- stock_tsb |>
  mutate(id = paste0(site_code, product_code)) |>
  group_by(id) |>
  mutate(zero_per = sum(stock_distributed == 0)/n(), count = n() - sum(stock_distributed == 0)) |> 
  filter(zero_per < 0.8) |>  # filter ts which contains more than 10 data points
  #select(-zero_per, -count, -site_type) |> 
  select(-count, -site_type) |> 
  left_join(site_df, by = 'site_code') |> 
  ungroup()


#save file

write.csv(stock_tsb_zero_remove, 'data/tidy_data/stock_tsb_zero_remove.csv')

write.csv(stock_tsb, 'data/tidy_data/stock_tsb.csv')
