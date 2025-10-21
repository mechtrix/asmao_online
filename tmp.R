library(tidyverse)
library(patchwork)

# 1D Gaussian
p1 <- tibble(x = seq(-4, 4, length.out = 1000)) %>%
  mutate(density = dnorm(x)) %>%
  ggplot(aes(x, density)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = 0, ymax = density), fill = "blue", alpha = 0.2) +
  labs(title = "1D Gaussian PDF", x = "x", y = "Density") +
  theme(aspect.ratio = 1)

# 2D Gaussian (radially symmetric)
grid <- expand_grid(
  x = seq(-3, 3, length.out = 100),
  y = seq(-3, 3, length.out = 100)
) %>%
  mutate(z = dnorm(x) * dnorm(y))

p2 <- ggplot(grid, aes(x, y, fill = z)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_fixed() +
  labs(title = "2D Gaussian: e^(-(x^2 + y^2)/2)", fill = "Density") +
  theme(aspect.ratio = 1)

# Polar coordinates visualization
theta <- seq(0, 2 * pi, length.out = 100)
r <- seq(0, 3, length.out = 100)
polar_grid <- expand_grid(r = r, theta = theta) %>%
  mutate(x = r * cos(theta), y = r * sin(theta), z = dnorm(x) * dnorm(y))

p3 <- ggplot(polar_grid, aes(x, y, color = z)) +
  geom_point(alpha = 0.5) +
  scale_color_viridis_c() +
  coord_fixed() +
  labs(title = "Polar Coordinates: Radial Symmetry", color = "Density") +
  theme(aspect.ratio = 1)

# Combine plots
(p1 + p2) / p3


set.seed(123)
n_trials <- 10000
L <- 1
D <- 1

theta <- runif(n_trials, 0, pi / 2)
y <- runif(n_trials, 0, D / 2)
crosses <- y < (L / 2) * sin(theta)
pi_estimate <- 2 * L / (mean(crosses) * D)

# Plot needle orientations
tibble(theta = theta, crosses = crosses) %>%
  ggplot(aes(x = theta, fill = crosses)) +
  geom_histogram(bins = 30, aes(y = ..density..)) +
  stat_function(
    fun = dbeta,
    args = list(shape1 = 1, shape2 = 1),
    color = "red",
    linetype = "dashed"
  ) +
  labs(
    title = paste("Buffon's Needle: Estimated π =", round(pi_estimate, 4)),
    x = "Needle Angle (θ)",
    y = "Density",
    fill = "Crosses Line?"
  ) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "gray"))
