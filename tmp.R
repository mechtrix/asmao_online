library(ggplot2)

# Parameters
min_val <- 1 # Minimum value
max_val <- 10 # Maximum value
n_discrete <- 10 # Number of discrete points
n_continuous <- 1000 # Points for continuous curve

# Create data frames
discrete_data <- data.frame(
  x = min_val:max_val,
  y = rep(1 / n_discrete, n_discrete),
  type = "Discrete Uniform"
)

continuous_data <- data.frame(
  x = seq(min_val, max_val, length.out = n_continuous),
  y = dunif(seq(min_val, max_val, length.out = n_continuous), min_val, max_val),
  type = "Continuous Uniform"
)

# Combine data for plotting
plot_data <- rbind(
  transform(discrete_data, distribution = "Discrete"),
  transform(continuous_data, distribution = "Continuous")
)

# Create plot
ggplot() +
  # Continuous uniform distribution
  geom_line(
    data = subset(plot_data, distribution == "Continuous"),
    aes(x = x, y = y, color = type),
    size = 1
  ) +
  # Discrete uniform distribution
  geom_point(
    data = subset(plot_data, distribution == "Discrete"),
    aes(x = x, y = y, color = type),
    size = 3
  ) +
  geom_segment(
    data = subset(plot_data, distribution == "Discrete"),
    aes(x = x, xend = x, y = 0, yend = y, color = type),
    linetype = "dashed",
    size = 0.5
  ) +
  # Aesthetics
  scale_color_manual(
    values = c("Continuous Uniform" = "blue", "Discrete Uniform" = "red")
  ) +
  labs(
    title = "Discrete vs Continuous Uniform Distribution",
    x = "Value",
    y = "Probability/Density",
    color = "Distribution Type"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
