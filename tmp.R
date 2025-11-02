library(tidyverse)
library(broom)

# Function to calculate power for a given n
calculate_power <- function(n, p_h1 = 0.6, alpha = 0.05) {
  # Critical value under H0 (exact binomial)
  c <- qbinom(1 - alpha, n, 0.5)
  
  # Power = P(X >= c | H1)
  power <- 1 - pbinom(c - 1, n, p_h1)
  
  tibble(n = n, critical_value = c, power = power)
}

# Calculate for multiple n
n_values <- c(10, 20, 30, 50)
power_results <- map_dfr(n_values, ~calculate_power(.x))

# Visualize
ggplot(power_results, aes(n, power)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Power Analysis for a Biased Coin (p = 0.6)",
    x = "Number of Flips (n)",
    y = "Power (1 - β)"
  ) +
  theme_minimal()
