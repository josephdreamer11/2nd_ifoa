library(tidyverse)
library(forecast)

monthly_premiums <- read_csv("L5_monthly_premiums.csv") %>%
  mutate(month = as.Date(month))

premium_ts <- ts(
  monthly_premiums$total_premium,
  start     = c(2019, 1),
  frequency = 12
)

ggAcf(premium_ts, lag.max = 36) +
  labs(title = "ACF — Monthly Premium Income") +
  theme_minimal()

ggPacf(premium_ts, lag.max = 36) +
  labs(title = "PACF — Monthly Premium Income") +
  theme_minimal()

decomp <- decompose(premium_ts, type = "additive")
autoplot(decomp) +
  labs(title = "Additive Decomposition") +
  theme_minimal()

decomp$trend
decomp$seasonal
decomp$random

decomp$seasonal[1:12]

