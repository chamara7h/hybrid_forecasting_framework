## ts visualisation

library(tidyverse)
library(tsibble)
library(tsbox)
library(dygraphs)
library(fpp3)


# read data

stock_tsb_zero_remove <- read.csv('data/tidy_data/stock_tsb_zero_remove.csv') %>% 
  select(-X) %>% 
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  as_tsibble(index = yearmonth, key = c(region, district, site_code, product_type, product_code))


## ts graphs

# dy graphs for all

ts_xts(stock_tsb_zero_remove) |> 
    dygraph() 


# dy graph for each product

stock_tsb_zero_remove |> group_by(product_code) |> 
    summarise(stock_distributed = sum(stock_distributed)) |> 
    ts_xts() |> 
    dygraph() 


## ts plots

no_x_axis <- theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)


# total

stock_tsb_zero_remove %>% filter_index(. ~ "2019 Sep") %>%
    index_by(yearmonth) %>% 
    summarise(stock_distributed = sum(stock_distributed)) %>%
    autoplot(stock_distributed) +
    labs(x = "Month", y = "Stock Distributed") +
    ggthemes::theme_few() +
    no_x_axis


# region

region_code = sample(stock_tsb_zero_remove$region, 5)


stock_tsb_zero_remove %>% filter_index(. ~ "2019 Sep") %>%
    filter(region %in% region_code) %>% 
    group_by(region) %>% 
    summarise(stock_distributed = sum(stock_distributed)) %>%
    autoplot(stock_distributed) +
    labs(x = "Month", y = "Stock Distributed") +
    ggthemes::theme_few() +
    ggthemes::scale_color_colorblind() +
    no_x_axis


# district

district_code = sample(stock_tsb_zero_remove$district, 5)


stock_tsb_zero_remove %>% filter_index(. ~ "2019 Jun") %>%
    filter(district %in% district_code) %>% 
    group_by(district) %>% 
    summarise(stock_distributed = sum(stock_distributed)) %>%
    autoplot(stock_distributed) +
    labs(x = "Month", y = "Stock Distributed") +
    ggthemes::theme_few() +
    ggthemes::scale_color_colorblind() +
    no_x_axis


# site code


site = sample(stock_tsb_zero_remove$site_code, 5)


stock_tsb_zero_remove %>% filter_index(. ~ "2019 Jun") %>%
    filter(site_code %in% site) %>% 
    group_by(site_code) %>% 
    summarise(stock_distributed = sum(stock_distributed)) %>%
    autoplot(stock_distributed) +
    labs(x = "Month", y = "Stock Distributed") +
    ggthemes::theme_few() +
    ggthemes::scale_color_colorblind() +
    no_x_axis


# product code

product = c("AS27138" , "AS27000", "AS27137", "AS27133", "AS27132", "AS27138")


stock_tsb_zero_remove %>% filter_index(. ~ "2019 Jun") %>%
    filter(product_code %in% product) %>% 
    group_by(product_code) %>% 
    summarise(stock_distributed = sum(stock_distributed)) %>%
    autoplot(stock_distributed) +
    labs(x = "Month", y = "Stock Distributed") +
    ggthemes::theme_few() +
    ggthemes::scale_color_colorblind() +
    no_x_axis


stock_tsb_zero_remove %>% filter_index(. ~ "2019 Jun") %>%
    group_by(region, product_code) %>% 
    summarise(stock_distributed = sum(stock_distributed)) %>%
    ggplot(aes(x = yearmonth, y = stock_distributed, color = product_code)) +
    geom_line() +
    labs(x = "Month", y = "Stock Distributed") +
    ggthemes::theme_few() +
    facet_wrap(vars(region), scales = "free_y") +
    no_x_axis


## plot site level

id_code <- c("C2062AS27000", "C1073AS27133", "C4026AS27133", "C4061AS27138", "C1101AS27137", 'C2070AS46000')

stock_tsb_zero_remove %>% filter_index(. ~ "2019 Jun") %>%
    filter(id %in% id_code) %>%
    group_by(site_code, product_code) %>% 
    ggplot(aes(x = yearmonth, y = stock_distributed, color = product_code)) +
    geom_line() +
    labs(x = "Month", y = "Stock Distributed") +
    ggthemes::theme_few() +
    # hrbrthemes::theme_ipsum() +
    # ggthemes::scale_color_colorblind() +
    facet_wrap(vars(site_code), scales = "free_y") +
    theme(axis.text.x = element_text(angle=45, hjust=1))


# seasonal plots

stock_tsb_zero_remove |>  
    group_by(product_code) |> 
    summarise(stock_distributed = sum(stock_distributed)) |>
    gg_season(stock_distributed)


stock_tsb_zero_remove |>  
    group_by(product_code) |> 
    summarise(stock_distributed = sum(stock_distributed)) |>
    gg_subseries(stock_distributed)


# autocorrelation plots

stock_tsb_zero_remove |>  
    #group_by(product_code) |> 
    summarise(stock_distributed = sum(stock_distributed)) |>
    gg_lag(stock_distributed, lags = 1:12, geom = "point") 


# time series features

stock_feat <- stock_tsb_zero_remove |>
    features(stock_distributed, feat_stl)

stock_feat |>
    ggplot(mapping = aes(x= trend_strength, y = seasonal_strength_year, color = product_code)) +
    geom_point() + 
    # facet_wrap(vars(region)) +
    #ggthemes::scale_color_colorblind() +
    labs(x = "Strength of trend", y = "Strength of yearly seasonality", color = "Product code") +
    hrbrthemes::theme_ipsum()





