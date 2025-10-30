dataset <- data.frame(
  idx = seq(1,35),
  gender = c("F","F","M","M","M","M","M","M","M","M","M","M","M","F","F","F","M","M","F","F","M","M","M","M","F","M","M","M","F","F","M","F","F","F","M"),
  age = c(30,25,23,24,22,27,24,25,24,26,28,23,22,26,23,28,25, 32, 22, 24, 24, 24, 22, 23, 22, 25, 25, 25, 26, 28,26,29,33,31,24)
)


male_data <- dataset |> filter(gender=="M")
female_data <- dataset |> filter(gender =="F")

smpl_male <- sample(male_data$age, size = 6)
smpl_female <- sample(male_data$age, size = 4)

mean(c(smpl_female,smpl_male))

syst_idx <- seq(3,35,by = 4)

tmp <- dataset |> filter(idx %in% syst_idx) |> pull(age) |> sd()


sample(dataset$age,size = 10) |> sd()

