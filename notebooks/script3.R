#clear R memory
rm(list=ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(fpp2)
library(astsa)
library(plotly)
library(forecast)


## Loading the data
bitcoin = read.csv("data/btc-usd-max.csv")
colnames(bitcoin) = c("Date", "Price", "market_cap", "total_volume")

## The tibble
bit_df = bitcoin %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date > as.Date('2022-01-01')) %>%
  filter(Date < as.Date('2024-01-01')) %>%
  arrange(Date)

bit_ts = bit_df %>%
  filter(Date > as.Date('2022-01-01')) %>%
  filter(Date < as.Date('2024-01-01')) %>%
  arrange(Date) %>%
  select(Price) %>%
  as.matrix() %>%
  ts()

## Original plot theme
my_theme = theme(panel.grid = element_line(color = '#e6e6e6'),
                 panel.background = element_rect(fill = 'white'),
                 plot.title = element_text(hjust = .5, size = 28, colour = 'black'),
                 text = element_text(family = 'Arial'),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 18, family = 'Arial', face = 'bold'),
                 axis.line = element_line(colour = '#737373', linewidth = 1),
                 strip.background = element_rect(colour = "black", fill = "white"),
                 strip.text = element_text(face = 'bold'))



ggplot(bit_df, aes(Date, Price)) + geom_line(col = 'black') +
  scale_y_continuous(breaks = c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000),
                     labels = c('$0', '$10000', '$20000', '$30000', '$40000', '$50000', '$60000', '$70000')) +
  labs(title = 'BTC price over time', x = '') +
  my_theme


## The ts
gglagplot(bit_ts, do.lines = F) + my_theme +
  scale_color_continuous(low = "#b37400",
                         high = "#ffc04d",
                         breaks = c(2, 365+1, 365+365+1, 365+365+365+1),
                         labels = c('2020', '2021', '2022', '2023')) +
  scale_y_continuous(breaks = c(0, 25000, 50000, 75000),
                     labels = c('$0', '$25000', '$50000', '$75000')) +
  scale_x_continuous(breaks = c(0, 25000, 50000, 75000),
                     labels = c('$0', '$25000', '$50000', '$75000'))


## ACF and PACF
acf_plot <- ggAcf(bit_ts, lag.max = 200) +
  my_theme +
  labs(title = 'ACF', y = 'Correlation')

# PACF Plot
pacf_plot <- ggPacf(bit_ts, lag.max = 200) +
  my_theme +
  labs(title = 'PACF', y = '')

grid.arrange(acf_plot, pacf_plot, ncol = 2)


## ACF and PACF diff
acf_plot_diff <- ggAcf(diff(bit_ts), lag.max = 200) +
  my_theme +
  labs(title = 'ACF', y = 'Correlation')

# PACF Plot
pacf_plot_diff <- ggPacf(diff(bit_ts), lag.max = 200) +
  my_theme +
  labs(title = 'PACF', y = '')

grid.arrange(acf_plot_diff, pacf_plot_diff, ncol = 2)


## The First Difference
bit_df[-1,] %>%
  mutate(Price = diff(bit_df$Price)) %>%
  ggplot(aes(Date, Price)) + geom_line() + my_theme +
  labs(x = '', title = 'Bitcoin Differenced By One', y = 'Difference')


## Log transformation
bit_df %>%
  mutate(Price_log = log(bit_df$Price)) %>%
  ggplot(aes(Date, Price_log)) + geom_line() +
  labs(title = 'BTC Price (Log Transformed)', x = '', y = 'Price') + my_theme



## Original Price
orig_price_diff <- bit_df[-1,] %>%
  mutate(Price = diff(bit_df$Price)) %>%
  ggplot(aes(Date, Price)) + geom_line() + my_theme +
  labs(x = '', title = 'Original Price', y = 'Difference')

## Transformed Price
log_price_diff <- bit_df[-1,] %>%
  mutate(Price_log = diff(log(bit_df$Price))) %>%
  ggplot(aes(Date, Price_log)) + geom_line() + my_theme +
  labs(x = '', title = 'Log Transformed Price', y = '')

grid.arrange(orig_price_diff, log_price_diff, ncol = 2)


# Log transformed ts
## ACF and PACF diff
bit_ts_tran = log(bit_ts)
acf_plot_diff <- ggAcf(diff(bit_ts_tran), lag.max = 200) +
  my_theme +
  labs(title = 'ACF', y = 'Correlation')

# PACF Plot
pacf_plot_diff <- ggPacf(diff(bit_ts_tran), lag.max = 200) +
  my_theme +
  labs(title = 'PACF', y = '')

grid.arrange(acf_plot_diff, pacf_plot_diff, ncol = 2)
auto.arima(bit_ts_tran)
checkresiduals(auto.arima(bit_ts_tran))


# Box-Cox Transformation
bit_ts_box = BoxCox(bit_ts, lambda = BoxCox.lambda(bit_ts))
acf_plot_diff_box <- ggAcf(diff(bit_ts_box), lag.max = 200) +
  my_theme +
  labs(title = 'ACF', y = 'Correlation')

# PACF Plot
pacf_plot_diff_box <- ggPacf(diff(bit_ts_box), lag.max = 200) +
  my_theme +
  labs(title = 'PACF', y = '')

grid.arrange(acf_plot_diff_box, pacf_plot_diff_box, ncol = 2)
auto.arima(bit_ts_box)
checkresiduals(auto.arima(bit_ts_box))


bit_ts_tran %>%
  Arima(order = c(0,1,0), include.drift = T) %>%
  checkresiduals()