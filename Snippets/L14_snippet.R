install.packages('survminer')

library(tidyverse)
library(survival)
library(survminer)
library(ggplot2)
library(tibble)


policyholders <- read_csv("L2_policyholders.csv") %>%
  mutate(time  = policy_duration_months,
         event = if_else(policy_duration_months < 60, 1, 0))

fit_km <- survfit(Surv(time, event) ~ 1, data = policyholders)

ggsurvplot(
  fit_km,
  data       = policyholders,
  conf.int   = TRUE,
  risk.table = TRUE,
  palette    = "#2c7bb6",
  xlab       = "Months in Force",
  ylab       = "Survival Probability S(t)",
  ggtheme    = theme_minimal()
)

fit_type <- survfit(Surv(time, event) ~ policy_type,
                    data = policyholders)

ggsurvplot(fit_type, data = policyholders,
           conf.int = TRUE, risk.table = TRUE,
           pval = TRUE, palette = "Set1", risk.table.fontsize = 2,
           ggtheme = theme_minimal())

fit_na <- survfit(Surv(time, event) ~ 1,
                  data = policyholders,
                  type = "fh")

na_df <- tibble(
  time   = fit_na$time,
  cumhaz = -log(fit_na$surv)
)

# H(t) = -log(S(t))

lr_test <- survdiff(Surv(time, event) ~ policy_type,
                    data = policyholders)

p_val <- 1 - pchisq(lr_test$chisq,
                    df = length(lr_test$n) - 1)

lr_test$obs
lr_test$exp 
