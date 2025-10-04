# Load tidyverse
library(tidyverse)

# Set seed for reproducibility
set.seed(123)

# Define grid dimensions
n_cols <- 8 # Columns (desks per row)
n_rows <- 5 # Rows (number of desk rows)

# Generate desk IDs and coordinates
student_data <- expand_grid(
  Row = 1:n_rows,
  Col = 1:n_cols
) %>%
  # Create desk IDs (e.g., "R1C1")
  mutate(
    Desk = str_c("R", Row, "C", Col),
    # Assign x (column) and y (row) coordinates
    x = Col,
    y = n_rows - Row + 1, # Flip y-axis to match Cartesian (bottom-left = origin)
    # Random age (22-38) and gender
    Age = sample(22:38, n_rows * n_cols, replace = TRUE),
    Gender = sample(c("Male", "Female"), n_rows * n_cols, replace = TRUE)
  )

# Reshape into wide-format grids (for visualization)
age_grid <- student_data %>%
  select(Desk, Age) %>%
  pivot_wider(names_from = Desk, values_from = Age)

gender_grid <- student_data %>%
  select(Desk, Gender) %>%
  pivot_wider(names_from = Desk, values_from = Gender)

# Print results
cat("=== Student Data (Long Format with x,y Coordinates) ===\n")
print(student_data, n = n_rows * n_cols)

cat("\n=== Age Grid (6×4) ===\n")
print(age_grid)

cat("\n=== Gender Grid (6×4) ===\n")
print(gender_grid)

save(
  file = here("data", "classroom_sampling.Rdata"),
  student_data,
  age_grid,
  gender_grid
)

ggplot(student_data, aes(x = x, y = y, fill = Age)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Desk), size = 3) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Classroom Age Distribution (x,y Coordinates)",
    x = "Column (x)",
    y = "Row (y)"
  ) +
  theme_minimal()

ggplot(student_data, aes(x = x, y = y, fill = Gender)) +
  geom_tile(color = "white") +
  geom_text(aes(label = str_sub(Desk, 1, 3)), size = 3) +
  scale_fill_manual(values = c("Male" = "skyblue", "Female" = "pink")) +
  labs(
    title = "Classroom Gender Distribution (x,y Coordinates)",
    x = "Column (x)",
    y = "Row (y)"
  ) +
  theme_minimal()
