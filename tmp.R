library(tidyverse)

# Simulate some data: study hours (x) and exam scores (y)
set.seed(123)
hours <- 1:10
scores <- c(55, 60, 65, 70, 75, 78, 80, 85, 90, 92) + rnorm(10, sd = 3)
data <- tibble(hours, scores)

# Fit a linear model
model <- lm(scores ~ hours, data = data)

# Manually compute SST, SSR, SSE
y_bar <- mean(data$scores)
SST <- sum((data$scores - y_bar)^2)
SSR <- sum((predict(model) - y_bar)^2)
SSE <- sum((data$scores - predict(model))^2)

# Compute R² both ways
R2_SSR <- SSR / SST
R2_SSE <- 1 - SSE / SST

# Compare with R's built-in R²
summary(model)$r.squared

# Should all match!
c(R2_SSR, R2_SSE, summary(model)$r.squared)
