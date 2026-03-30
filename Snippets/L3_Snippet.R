library(readr)
library(tidyverse)
raw <- read.csv("L3/L3_policyholders.csv")

raw %>%
  mutate(
    policy_type = str_trim(policy_type),
    policy_type = str_to_title(policy_type)
  ) %>%
  count(policy_type)

# 'MOTOR'vs  'MOTOR ' 
# 'MOTOR' and 'motor' both become 'Motor'

library(lubridate)
raw %>%
  mutate(
    policy_start_date = parse_date_time(
      policy_start_date,
      orders = c("ymd", "dmy", "mdy", "d b Y")
    ),
    policy_year = year(policy_start_date)
  ) %>%
  count(policy_year)


raw %>%
  mutate(premium_band = case_when(
    annual_premium < 1000 ~ "Low",
    annual_premium < 2000 ~ "Mid",
    TRUE                  ~ "High"
  )) %>%
  count(policy_type, premium_band) %>%
  pivot_wider(names_from  = premium_band,
              values_from = n,
              values_fill = 0)














