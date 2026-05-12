library(tidyverse)
# install.packages('xts')
library(xts)
library(forecast)

monthly_premiums <- read_csv("L5_monthly_premiums.csv") %>%
  mutate(month = as.Date(month))


premium_ts <- ts(
  monthly_premiums$total_premium,
  start     = c(2019, 1),
  frequency = 12
)

start(premium_ts)
end(premium_ts)
frequency(premium_ts)

window(premium_ts, start = c(2021, 1), end = c(2021, 12))

premium_xts <- xts(
  monthly_premiums$total_premium,
  order.by = monthly_premiums$month
)

# All of 2021
premium_xts["2021"]

# First half of 2021
premium_xts["2021-01/2021-06"]

ep <- endpoints(premium_xts, on = "years")
premium_xts[ep]

as.xts(premium_ts)   # ts → xts
as.ts(premium_xts)   # xts → ts




