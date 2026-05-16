library(tidyverse)
library(forecast)

monthly_premiums <- read_csv("L5_monthly_premiums.csv") %>%
  mutate(month = as.Date(month))

premium_ts <- ts(
  monthly_premiums$total_premium,
  start     = c(2019, 1),
  frequency = 12
)

train_ts <- window(premium_ts, end   = c(2022, 12))
test_ts  <- window(premium_ts, start = c(2023,  1))

model_train <- auto.arima(
  train_ts,
  stepwise      = FALSE,
  approximation = FALSE
)

fc <- forecast(model_train, h = 12, level = c(80, 95))

autoplot(fc) +
  autolayer(test_ts, series = "Actual", color = "#d7191c") +
  theme_minimal()


accuracy(fc, test_ts)

model_naive <- snaive(train_ts, h = 12)
accuracy(fc,          test_ts)
accuracy(model_naive, test_ts)
