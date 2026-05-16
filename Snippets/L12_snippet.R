library(tidyverse)
set.seed(42)

policyholders <- read_csv("L2_policyholders.csv")

lambda    <- sum(policyholders$has_claim) / 48
claims_only <- policyholders$claim_amount[policyholders$has_claim == 1]
mu_sev    <- mean(log(claims_only))
sigma_sev <- sd(log(claims_only))

rpois(1, lambda = lambda)

monthly_claims    <- rpois(36, lambda = lambda)
cumulative_claims <- cumsum(monthly_claims)

n          <- rpois(1, lambda = lambda)
severities <- rlnorm(n, meanlog = mu_sev, sdlog = sigma_sev)
S          <- sum(severities)

n_sims <- 5000

annual_losses <- replicate(n_sims, {
  monthly <- sapply(1:12, function(t) {
    n <- rpois(1, lambda = lambda)
    sum(rlnorm(n, meanlog = mu_sev, sdlog = sigma_sev))
  })
  sum(monthly)
})

quantile(annual_losses, probs = c(0.95, 0.99, 0.995))

