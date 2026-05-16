library(tidyverse)
library(forecast)

monthly_premiums <- read_csv("L5_monthly_premiums.csv") %>%
  mutate(month = as.Date(month))

premium_ts <- ts(
  monthly_premiums$total_premium,
  start     = c(2019, 1),
  frequency = 12
)

ndiffs(premium_ts)
nsdiffs(premium_ts)

model_auto <- auto.arima(
  premium_ts,
  stepwise      = FALSE,
  approximation = FALSE
)

summary(model_auto)


model_manual <- Arima(
  premium_ts,
  order    = c(1, 1, 0),
  seasonal = c(1, 1, 0)
)

checkresiduals(model_auto)



