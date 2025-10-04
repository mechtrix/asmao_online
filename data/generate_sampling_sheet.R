library(readxl)
library(tidyverse)
library(writexl)
n_rows = 7
n_vols = 5


actual_student_data <- expand_grid(
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
  )

write_xlsx(
  actual_student_data,
  here("data", "student_data_sampling_methods.xlsx")
)
