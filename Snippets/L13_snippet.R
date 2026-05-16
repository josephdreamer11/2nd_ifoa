
install.packages('survival')

library(tidyverse)
library(survival)

policyholders <- read_csv("L2_policyholders.csv") %>%
  mutate(
    time  = policy_duration_months,
    event = if_else(policy_duration_months < 60, 1, 0)
  )

sample_ids <- policyholders %>%
  slice_sample(n = 40) %>%
  arrange(time) %>%
  mutate(rank = row_number())

ggplot(sample_ids) +
  geom_segment(aes(x = 0, xend = time,
                   y = rank, yend = rank,
                   color = factor(event)),
               linewidth = 0.7) +
  geom_point(aes(x = time, y = rank,
                 color = factor(event),
                 shape = factor(event)),
             size = 2.5) +
  scale_color_manual(values = c("0" = "#2c7bb6", "1" = "#d7191c"),
                     labels = c("0" = "Censored", "1" = "Lapsed")) +
  scale_shape_manual(values = c("0" = 1, "1" = 16),
                     labels = c("0" = "Censored", "1" = "Lapsed")) +
  theme_minimal()


surv_obj <- Surv(time  = policyholders$time,
                 event = policyholders$event)

surv_obj[1:20]

fit <- survfit(Surv(time, event) ~ 1, data = policyholders)
summary(fit, times = c(12, 24, 36, 48, 60))

fit_type <- survfit(Surv(time, event) ~ policy_type,
                    data = policyholders)

