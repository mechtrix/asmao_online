library(tidyverse)
library(qqplotr)

dataset <- data.frame(
  student_A = c(5.95,7.83,7.20,7.65,7.09,6.75,7.30,7.27,7.80,7.63),
  student_B = c(7.80,6.45,7.70,7.17,6.75,7.34,7.80,7.51,8.75,8.52)
)

data_long <- dataset |> pivot_longer(cols = starts_with("student"),names_to = "student",values_to = "length")


data_long |> 
  ggplot(
    aes(
      x = student,
      y = length
    )
  )+
  geom_boxplot()
