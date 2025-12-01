library(tidyverse)

student_A = c(6.9,7.8,5.38,5,5.67,6.62,6.40,5.1,6.46,5.52)
student_B = c(4.84,7.10,5.78,5.16, 6.75,6.53,7.34,5.68,6.45,5.74) 

wet_projectile = c(5.4, 3.56, 4.3, 3.72, 4.27, 4.35, 3.33, 4.70,3.03,3.48)

var_a = sd(student_A)^2
var_b = sd(student_B)^2
var_c = sd(wet_projectile)^2


dataset <- data.frame(
  student_A,student_B, idx = seq(1,10)
)

data_long <- dataset |> 
  pivot_longer(cols = starts_with("stud"), values_to = "length", names_to = "student")


data_long |> 
  ggplot(
    aes(
      x = student,
      y = length,
      # shape = student
    )
  )+
  geom_boxplot(
    # size = 5
  )+
  geom_jitter()
  
