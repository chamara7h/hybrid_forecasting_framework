## Data quality check

library(tidyverse)
library(tsibble)


## read data

stock_df <- read.csv('data/contraceptive_logistics_data.csv')


## take a quick look

glimpse(stock_df)

summary(stock_df) # stock_ordered has 770 missing values


## count number of unique values

stock_df %>%
  select(region, district, site_code, product_code) %>%
  summarise(across(everything(), ~ n_distinct(.))) 


## plot product count in each site

stock_df %>%
  ggplot(aes(product_code)) +
  geom_bar() +
  geom_text(stat = "count" , aes(label=after_stat(count)), vjust=-1)+
  labs(x = "Product code", y = "Count") +
  theme_minimal()


## Distribution of stock values

#stock_initial

stock_df %>%
  group_by(product_code) %>%
  mutate(max = max(stock_initial)) %>%
  ungroup %>%
  ggplot(aes(x=stock_initial))+
  geom_histogram()+
  geom_vline(aes(xintercept = max), color="red")+
  facet_wrap(~product_code)  

#stock_received

stock_df %>%
  group_by(product_code) %>%
  mutate(max = max(stock_received)) %>%
  ungroup %>%
  ggplot(aes(x=stock_received))+
  geom_histogram()+
  geom_vline(aes(xintercept = max), color="red")+
  facet_wrap(~product_code)  
  
#stock_distributed

stock_df %>%
  group_by(product_code) %>%
  mutate(max = max(stock_distributed)) %>%
  ungroup %>%
  ggplot(aes(x=stock_distributed))+
  geom_histogram()+
  geom_vline(aes(xintercept = max), color="red")+
  facet_wrap(~product_code)

#stock_end

stock_df %>%
  group_by(product_code) %>%
  mutate(max = max(stock_end)) %>%
  ungroup %>%
  ggplot(aes(x=stock_end))+
  geom_histogram()+
  geom_vline(aes(xintercept = max), color="red")+
  facet_wrap(~product_code)
  
#stock_adjustment

stock_df %>%
  group_by(product_code) %>%
  mutate(max = max(stock_adjustment)) %>%
  ungroup %>%
  ggplot(aes(x=stock_adjustment))+
  geom_histogram()+
  geom_vline(aes(xintercept = max), color="red")+
  facet_wrap(~product_code)

#stock_ordered

stock_df %>%
  filter(!is.na(stock_ordered)) %>%
  group_by(product_code) %>%
  mutate(max = max(stock_ordered)) %>%
  ungroup %>%
  ggplot(aes(x=stock_ordered))+
  geom_histogram()+
  geom_vline(aes(xintercept = max), color="red")+
  facet_wrap(~product_code)


## relationship between stock variables

stock_df %>%
  ggplot(aes(x=stock_initial, y=stock_end))+
  geom_point() +
  facet_wrap(~product_code)
  
stock_df %>%
  ggplot(aes(x=stock_received, y=stock_distributed))+
  geom_point() +
  facet_wrap(~product_code) 
  
stock_df %>%
  filter(!is.na(stock_ordered)) %>%
  ggplot(aes(x=stock_received, y=stock_ordered))+
  geom_point() +
  facet_wrap(~product_code)  
  
stock_df %>%
  filter(!is.na(stock_ordered)) %>%
  ggplot(aes(x=stock_distributed, y=stock_ordered))+
  geom_point() +
  facet_wrap(~product_code)   


## stock data validation

stock_df <- stock_df %>%
  mutate(validation = stock_initial + stock_received - stock_distributed)

# plot validation vs stock_end

stock_df %>%
  ggplot(aes(x=stock_end, y=validation))+
  geom_point() +
  facet_wrap(~product_code)

# add stock adjustments to the validation equation

stock_df <- stock_df %>%
  mutate(validation = stock_initial + stock_received - stock_distributed + stock_adjustment)

# plot validation vs stock_end

stock_df %>%
  ggplot(aes(x=stock_end, y=validation))+
  geom_point() +
  facet_wrap(~product_code)

# check lag value of stock end against stock initial

stock_df <- stock_df %>%
  mutate(yearmonth = paste0(year,"-", month)) %>% ##to create date column
  mutate(yearmonth = yearmonth(yearmonth)) %>%
  arrange(yearmonth) 


stock_df <- stock_df %>%
  group_by(site_code, product_code) %>%
  mutate(stock_end_lag1 = lag(stock_end, n = 1, default = 0))


# plot stock_initial vs stock_end_lag1

stock_df %>%
  ggplot(aes(x=stock_initial, y=stock_end_lag1))+
  geom_point() +
  facet_wrap(~product_code)


## Plot stock related variables over time

stock_df %>% 
  ungroup() %>%
  filter(site_code %in% sample(stock_df$site_code,5),
         product_code %in% sample(stock_df$product_code,5)) %>%
  ggplot(aes(x=yearmonth)) +
  geom_line(aes(y=stock_initial, colour = "stock_initial")) +
  geom_line(aes(y=stock_end, colour = "stock_end")) +
  geom_line(aes(y=stock_distributed, colour = "stock_distributed")) +
  facet_grid(product_code ~ site_code)


stock_df %>% 
  ungroup() %>%
  filter(site_code %in% sample(stock_df$site_code,5),
         product_code %in% sample(stock_df$product_code,5),
         !is.na(stock_ordered)) %>%
  ggplot(aes(x=yearmonth)) +
  geom_line(aes(y=stock_received, colour = "stock_received")) +
  geom_line(aes(y=stock_distributed, colour = "stock_distributed")) +
  geom_line(aes(y=stock_ordered, colour = "stock_ordered")) +
  facet_grid(product_code ~ site_code)



