### post forecasting plots

library(tidyverse)
library(tsibble)
library(ggdist)


## read data

stock_tsb <- read.csv('data/tidy_data/stock_tsb_zero_remove.csv') %>% # actual data
  
  select(-X) %>% 
  
  mutate(yearmonth = yearmonth(yearmonth)) %>% 
  
  as_tsibble(index = yearmonth, key = c(region, district, site_code, product_type, product_code))


prob_prediction <- read_rds('data/predictions/bs_predictions_all_prob.rds') # prob prediction

point_prediction <- read_rds('data/predictions/bs_predictions_all_point.rds') # prob prediction


### plot distributions

#C5060AS27138 C1086AS27134


actuals <- stock_tsb %>% 
  
  filter(id == 'C2005AS27000')


fcst <- prob_prediction %>% 
  
  filter(model == 'hybrid_sk', id == 'C2005AS27000') %>% 
  
  select(-model, -stock_distributed) %>% 
  
  pivot_longer(-c(yearmonth, id), values_to = '.sim', names_to = '.rep') %>% 
  
  as_tsibble(index = yearmonth, key = c(id, .rep))



fcst_q <- fcst %>% 
  
  as_tibble() %>% 
  
  group_by(yearmonth) %>%
  
  summarise(quantile = scales::percent(c(0.80, 0.95)),
            value = quantile(.sim, c(0.80, 0.95)), .groups = 'drop')



fcst_point <- fcst %>%  index_by(yearmonth) %>%  summarise(.mean = mean(.sim))


ggplot(data = fcst, mapping = aes(x = yearmonth, y = .sim)) + 
  
  stat_halfeye() +
  
  geom_point(data=fcst_point, aes(y=.mean, shape ="Point Forecast")) +
  
  geom_line(data=fcst_point, aes(y=.mean)) +
  
  geom_line(aes(y = stock_distributed),data = filter_index(actuals, "2019 Jan" ~ .)) +
  
  geom_point(aes(y = stock_distributed, shape ="Actual"), data = filter_index(actuals, "2019 Jan" ~ .)) +
  
  # geom_line(data = bs_pred_hybrid %>% 
  #             filter(model == 'hybrid', id == 'C2062AS27000'), aes(y = prediction)) +
  # 
  # geom_point(data = bs_pred_hybrid %>% 
  #              filter(model == 'hybrid', id == 'C2062AS27000'), aes(y = prediction, shape ="hybrid")) +
  
  scale_shape_manual(name=NULL,
                     breaks=c('Actual','Point Forecast'),
                     values=c('Actual'=15, 'Point Forecast'=16)) +
  labs(x= "Month", y="Stock Distributed")+
  ggthemes::theme_few() +
  theme(legend.position = "top")


## plot with other models

# model_list <- c("snaive", "moving_average", "ets", "arima", "ensemble_uni",
#            "ensemble_ml_sk", "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk")


model_list <- c("ensemble_ml_sk", "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk")



fig <- ggplot(data = fcst, mapping = aes(x = yearmonth, y = .sim)) + 
  
  stat_halfeye() +
  
  geom_point(data = fcst_point, aes(y = .mean, color = "Mean Forecast"), size = 3) +
  
  geom_line(data = fcst_point, aes(y = .mean, color = "Mean Forecast")) +

  geom_line(aes(y = stock_distributed, color = "StockDistributed (Actual)"), data = filter_index(actuals, "2019 Oct" ~ .)) +
  
  geom_point(aes(y = stock_distributed, color = "StockDistributed (Actual)"), data = filter_index(actuals, "2019 Oct" ~ .), size = 3) +
  
  geom_line(aes(y = stock_ordered, color = "Stock Ordered"), data = filter_index(actuals, "2019 Oct" ~ .)) +
  
  geom_point(aes(y = stock_ordered, color = "Stock Ordered"), data = filter_index(actuals, "2019 Oct" ~ .), size = 3) +
  
  ggthemes::theme_few() +
  
  ggthemes::scale_color_colorblind()
  
  # scale_color_manual(values = c("Point Forecast" = "blue", "Actual" = "green", "Stock Ordered" = "red")) 
  
  # scale_shape_manual(values = c("Point Forecast" = 16, "Actual" = 17, "Stock Ordered" = 18))

ggsave(filename=paste("manuscript/figs/dist_fit.png"), plot = fig, width = 9, height = 6, dpi = 400)


############


actuals <- stock_tsb %>% 
  
  filter(id == 'C2049AS27000')


fcst <- prob_prediction %>% 
  
  filter(model == 'hybrid_sk', id == 'C2049AS27000') %>% 
  
  select(-model, -stock_distributed) %>% 
  
  pivot_longer(-c(yearmonth, id), values_to = '.sim', names_to = '.rep') %>% 
  
  as_tsibble(index = yearmonth, key = c(id, .rep))



fcst_q <- fcst %>% 
  
  as_tibble() %>% 
  
  group_by(yearmonth) %>%
  
  summarise(quantile = scales::percent(c(0.80, 0.95)),
            value = quantile(.sim, c(0.80, 0.95)), .groups = 'drop')



fcst_point <- fcst %>%  index_by(yearmonth) %>%  summarise(.mean = mean(.sim))



model_list <- c("ensemble_ml_sk", "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk")



fig <- ggplot(data = fcst, mapping = aes(x = yearmonth, y = .sim)) + 
  
  stat_halfeye() +
  
  geom_point(data = fcst_point, aes(y = .mean, color = "Mean Forecast"), size = 3) +
  
  geom_line(data = fcst_point, aes(y = .mean, color = "Mean Forecast")) +
  
  geom_line(aes(y = stock_distributed, color = "StockDistributed (Actual)"), data = filter_index(actuals, "2019 Oct" ~ .)) +
  
  geom_point(aes(y = stock_distributed, color = "StockDistributed (Actual)"), data = filter_index(actuals, "2019 Oct" ~ .), size = 3) +
  
  geom_line(aes(y = stock_ordered, color = "Stock Ordered"), data = filter_index(actuals, "2019 Oct" ~ .)) +
  
  geom_point(aes(y = stock_ordered, color = "Stock Ordered"), data = filter_index(actuals, "2019 Oct" ~ .), size = 3) +
  
  ggthemes::theme_few() +
  
  ggthemes::scale_color_colorblind()

# scale_color_manual(values = c("Point Forecast" = "blue", "Actual" = "green", "Stock Ordered" = "red")) 

# scale_shape_manual(values = c("Point Forecast" = 16, "Actual" = 17, "Stock Ordered" = 18))

ggsave(filename=paste("manuscript/figs/dist_fit.png"), plot = fig, width = 9, height = 6, dpi = 400)





############


actuals <- stock_tsb %>% 
  
  filter(product_code == 'AS27000') %>% 
  
  


fcst <- prob_prediction %>% 
  
  filter(model == 'hybrid_sk', id == 'C2049AS27000') %>% 
  
  select(-model, -stock_distributed) %>% 
  
  pivot_longer(-c(yearmonth, id), values_to = '.sim', names_to = '.rep') %>% 
  
  as_tsibble(index = yearmonth, key = c(id, .rep))



fcst_q <- fcst %>% 
  
  as_tibble() %>% 
  
  group_by(yearmonth) %>%
  
  summarise(quantile = scales::percent(c(0.80, 0.95)),
            value = quantile(.sim, c(0.80, 0.95)), .groups = 'drop')



fcst_point <- fcst %>%  index_by(yearmonth) %>%  summarise(.mean = mean(.sim))



model_list <- c("ensemble_ml_sk", "lgbm_sk", "xgb_sk", "rf_sk", "mlr_sk")



fig <- ggplot(data = fcst, mapping = aes(x = yearmonth, y = .sim)) + 
  
  stat_halfeye() +
  
  geom_point(data = fcst_point, aes(y = .mean, color = "Mean Forecast"), size = 3) +
  
  geom_line(data = fcst_point, aes(y = .mean, color = "Mean Forecast")) +
  
  geom_line(aes(y = stock_distributed, color = "Actual"), data = filter_index(actuals, "2019 Oct" ~ .)) +
  
  geom_point(aes(y = stock_distributed, color = "Actual"), data = filter_index(actuals, "2019 Oct" ~ .), size = 3) +
  
  geom_line(aes(y = stock_ordered, color = "Stock Ordered"), data = filter_index(actuals, "2019 Oct" ~ .)) +
  
  geom_point(aes(y = stock_ordered, color = "Stock Ordered"), data = filter_index(actuals, "2019 Oct" ~ .), size = 3) +
  
  ggthemes::theme_few() +
  
  ggthemes::scale_color_colorblind()

# scale_color_manual(values = c("Point Forecast" = "blue", "Actual" = "green", "Stock Ordered" = "red")) 

# scale_shape_manual(values = c("Point Forecast" = 16, "Actual" = 17, "Stock Ordered" = 18))

ggsave(filename=paste("manuscript/figs/dist_fit.png"), plot = fig, width = 9, height = 6, dpi = 400)
