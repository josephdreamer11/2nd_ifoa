library(tidyverse)
library(skimr)

policyholders <- read_csv("L2/L2_policyholders.csv") %>%
  mutate(churned = if_else(policy_duration_months < 24, 1, 0))


policyholders %>%
  select(age, annual_premium, policy_duration_months, churned) %>%
  skim()

policyholders %>%
  group_by(policy_type) %>%
  summarise(
    Policyholders = n(),
    Churned       = sum(churned),
    Churn_Rate    = round(mean(churned) * 100, 1)
  )

policyholders %>%
  group_by(policy_type) %>%
  summarise(churn_rate = mean(churned) * 100) %>%
  ggplot(aes(x = reorder(policy_type, churn_rate),
             y = churn_rate,
             fill = policy_type)) +
  geom_col() +
  coord_flip() +
  labs(title = "Churn Rate by Policy Type",
       x = NULL, y = "Churn Rate (%)") +
  theme_minimal()
