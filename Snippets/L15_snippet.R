# h(t) = λ

# h(t) = (k/λ)(t/λ)^(k-1)

survreg()

library(tidyverse)
library(survival)

policyholders <- read_csv("L2_policyholders.csv") %>%
  mutate(time  = policy_duration_months,
         event = if_else(policy_duration_months < 60, 1, 0))

fit_exp <- survreg(Surv(time, event) ~ 1,
                   data = policyholders,
                   dist = "exponential")
summary(fit_exp)

lambda_exp <- 1 / exp(coef(fit_exp)[1])

S(t) = exp(-λt)


# mean = 1/λ, median = log(2)/λ

fit_weib <- survreg(Surv(time, event) ~ 1,
                    data = policyholders,
                    dist = "weibull")

k_weib      <- 1 / fit_weib$scale
lambda_weib <- exp(coef(fit_weib)[1])


# S(t) = exp(-(t/λ)^k) 
# h(t) = (k/λ)(t/λ)^(k-1)

cat("k =", round(k_weib, 3), "\n")

AIC(fit_exp)
AIC(fit_weib)
