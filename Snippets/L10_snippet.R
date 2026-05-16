set.seed(42)

policyholders <- read_csv("L2_policyholders.csv")

mu    <- mean(policyholders$claim_amount[policyholders$has_claim == 1])
sigma <- sd(policyholders$claim_amount[policyholders$has_claim == 1])

n_steps    <- 60
increments <- rnorm(n_steps, mean = mu, sd = sigma)
rw_path    <- cumsum(increments)
rw_path[1]

tibble(step = 1:n_steps, value = rw_path) %>%
  ggplot(aes(x = step, y = value)) +
  geom_line(color = "#2c7bb6") +
  theme_minimal()

n_paths    <- 200
sim_matrix <- replicate(
  n_paths,
  cumsum(rnorm(n_steps, mean = mu, sd = sigma))
)

as.data.frame(sim_matrix) %>%
  mutate(step = 1:n_steps) %>%
  pivot_longer(-step, names_to = "path", values_to = "value") %>%
  ggplot(aes(x = step, y = value, group = path)) +
  geom_line(alpha = 0.07, color = "#2c7bb6") +
  theme_minimal()


final_values <- sim_matrix[n_steps, ]

quantile(final_values, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))


