library(forecast)
library(tidyverse)

policyholders   <- read_csv("L2/L2_policyholders.csv")
monthly_premiums <- read_csv("L5/L5_monthly_premiums.csv") %>%
  mutate(month = as.Date(month))

premium_ts <- ts(
  monthly_premiums$total_premium,
  start     = c(2019, 1),
  frequency = 12
)

autoplot(premium_ts) +
  labs(title = "Monthly Premium Income",
       x = "Date", y = "Total Premium (£)") +
  theme_minimal()

ggplot(monthly_premiums, aes(x = month, y = total_premium)) +
  geom_line(color = "#2c7bb6", linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE,
              color = "#d7191c", linetype = "dashed") +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%b %Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

retention <- policyholders %>%
  group_by(policy_type, policy_duration_months) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(policy_type) %>%
  mutate(
    total         = sum(n),
    survivors     = total - cumsum(lag(n, default = 0)),
    retention_pct = survivors / total * 100
  )

ggplot(retention, aes(x = policy_duration_months,
                      y = retention_pct,
                      color = policy_type)) +
  geom_step(linewidth = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Retention Curves by Policy Type",
       x = "Duration (Months)", y = "Retention (%)") +
  theme_minimal()

