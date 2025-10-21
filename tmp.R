library(tidyverse)
set.seed(123)

# Simulate production data from two machines
machine_A <- rnorm(1000, mean = 50, sd = 2)  # Machine A: mean=50g, sd=2g
machine_B <- rnorm(1000, mean = 75, sd = 5)  # Machine B: mean=75g, sd=5g

# Plot
tibble(
  value = c(machine_A, machine_B),
  machine = rep(c("A", "B"), each = 1000)
) %>%
  ggplot(aes(x = value, fill = machine)) +
  geom_density(alpha = 0.5) +
  labs(title = "Fill Weights from Two Machines (Different Means/SDs)")
