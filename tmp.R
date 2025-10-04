library(tidyverse)
library(ggplot2)

# Example data (replace with your dataset)
student_data <- tibble(
  Row = rep(1:4, each = 4),
  Col = rep(1:4, 4),
  Desk = paste0("R", Row, "C", Col),
  Age = c(30, 22, 27, 35, 28, 25, 29, 32, 23, 31, 34, 26, 21, 33, 24, 36),
  Gender = rep(c("Male", "Female"), 8),
  AgeRange = cut(
    Age,
    breaks = c(0, 25, 30, 40),
    labels = c("18-25", "26-30", "31-38")
  )
)
set.seed(123) # For reproducibility

set.seed(123)

set.seed(123)

stratified_sample <- student_data %>%
  group_by(Gender) %>%
  sample_n(2) %>% # Take 2 random students per gender
  arrange(Row, Col) %>% # Sort by physical position (critical!)
  ungroup() %>%
  mutate(
    # Assign TRUE/FALSE alternately, starting with TRUE for the first student
    is_highlighted = rep(c(TRUE, FALSE), times = n() / 2)
  )

ggplot() +
  # All students (light gray background)
  geom_tile(
    data = student_data,
    aes(x = Col, y = Row, fill = Age),
    color = "gray50",
    linewidth = 0.5,
    alpha = 0.3
  ) +
  # Stratified sample (darker borders)
  geom_tile(
    data = stratified_sample,
    aes(x = Col, y = Row, fill = Age),
    color = "black",
    linewidth = 1
  ) +
  # Highlight every second student (red border)
  geom_tile(
    data = filter(stratified_sample, is_highlighted),
    aes(x = Col, y = Row),
    fill = NA,
    color = "red",
    linewidth = 2
  ) +
  # Add desk labels
  geom_text(
    data = student_data,
    aes(x = Col, y = Row, label = str_sub(Desk, 3, 4)),
    size = 4
  ) +
  # Customize colors and labels
  scale_fill_gradient(
    low = "lightblue",
    high = "darkblue",
    name = "Age"
  ) +
  labs(
    title = "Classroom Seating: Stratified Sample (Every 2nd Student Highlighted)",
    x = "Column",
    y = "Row"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )
