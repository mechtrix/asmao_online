dataset <- data.frame(
  student_id = seq(1,25),
  gender = c("m","m", "m", "m", "m", "m", "m", "m", "m", "m", "m", "f","f","m","m","m","m","m","m","m","m","f","f","f","m"),
  age = c(27,29,24,23,25,25,29,26,27,25,25,34,31,25,60,23,22,23,26,29,28,22,39,30,23)
)

mean(dataset$age)
sd(dataset$age)

set.seed(42)

rs_sample <- sample(dataset$age,size=10)

rs_sample |> mean()
rs_sample |> sd()


stratified_prop_n10 <- dataset |> 
  group_by(gender) %>%
  sample_frac(0.4) |> 
  ungroup() 

mean(stratified_prop_n10$age)
sd(stratified_prop_n10$age)

stratified_prop_n15 <- dataset |> 
  group_by(gender) %>%
  sample_frac(0.6) |> 
  ungroup() 

mean(stratified_prop_n15$age)
sd(stratified_prop_n15$age)


syst_s <- dataset |> 
  slice(seq(1, n(), by = 3)) 

mean(syst_s$age)
sd(syst_s$age)


syst_s2 <- dataset |> 
  slice(seq(1, n(), by = 2)) 

mean(syst_s2$age)
sd(syst_s2$age)


clst_smpl_n9 <- dataset |> 
  filter(student_id %in% c(9,10,11,12,13,18,19,20,21))

mean(clst_smpl_n9$age)
sd(clst_smpl_n9$age)


clst_smpl_n13 <- dataset |> 
  filter(student_id %in% c(9,10,11,12,13,18,19,20,21,22,23,24,25))

mean(clst_smpl_n13$age)
sd(clst_smpl_n13$age)
