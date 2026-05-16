install.packages('markovchain')

library(tidyverse)
library(markovchain)

set.seed(42)
policyholders <- read_csv("L2_policyholders.csv")

P <- matrix(
  c(0.85, 0.10, 0.05,
    0.00, 1.00, 0.00,
    0.00, 0.00, 1.00),
  nrow  = 3,
  byrow = TRUE,
  dimnames = list(
    c("Active", "Lapsed", "Claimed"),
    c("Active", "Lapsed", "Claimed")
  )
)

rowSums(P)

mc <- new("markovchain",
          states           = c("Active", "Lapsed", "Claimed"),
          transitionMatrix = P,
          name             = "Policyholder Status")

initial <- c(Active = 1, Lapsed = 0, Claimed = 0)

install.packages("expm")
library(expm)

initial %*% (P %^% 12)

steadyStates(mc)


