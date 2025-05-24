# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Set seed for reproducibility
set.seed(42)

# Create a large population from which to sample
population <- rnorm(100000, mean = 0, sd = 1)

true_population_variance <- var(population) * (length(population) - 1) / length(population)

# Define sample sizes
sample_sizes <- c(2,10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)

# Initialize list to store results
results <- data.frame()

# Loop through sample sizes
for (n in sample_sizes) {
  sample <- sample(population, size = n, replace = FALSE)
  sample_mean <- mean(sample)
  ssq <- sum((sample - sample_mean)^2)
  
  biased_var <- ssq / n
  unbiased_var <- ssq / (n - 1)
  
  results <- rbind(results, data.frame(
    SampleSize = n,
    BiasedVariance = biased_var,
    UnbiasedVariance = unbiased_var,
    Difference = unbiased_var - biased_var
  ))
}

# Reshape data for ggplot
results_long <- results %>%
  pivot_longer(cols = c("BiasedVariance", "UnbiasedVariance"),
               names_to = "Type", values_to = "Variance") |> 
  mutate(
    diff_to_true_var = abs(1-Variance)
  )




results_long |> 
  ggplot(
    aes(
      x = SampleSize,
      y = Difference
    )
  )+
  geom_line(size = 1)+
  scale_x_log10()+
  labs(
    title = "Difference between biased (n) and unbiased (n - 1) variance estimates",
    x = "Sample Size", 
    y = "biased (n) variance - unbiased (n-1) variance",
    color = "Variance Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# Plot
results_long |> 
  ggplot(
    aes(
      x = SampleSize, 
      y = Variance, 
      color = Type)) +
  geom_line(
    size = 1
    ) +
  geom_hline(yintercept = 1)+
  labs(
    title = "Biased (n) and unbiased (n - 1) variance estimates",
    x = "Sample Size", 
    y = "Estimated Variance",
    color = "Variance Type") +
  theme_minimal(base_size = 14) +
  scale_x_log10()+
  scale_color_manual(values = c("steelblue", "firebrick")) +
  theme(legend.position = "top")

# title = "Impact of Bessel's Correction on Sample Variance Estimates",

plt <- plt_var+plt_diff
