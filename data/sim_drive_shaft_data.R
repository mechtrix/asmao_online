
# Drive Shaft group data

set.seed(1230)

sample_size <- 100

drive_shaft <- data.frame(
  group01 = round(rnorm(sample_size,mean = 12, sd = 0.1),digits = 2),
  group02 = round(rnorm(sample_size,mean = 12.3, sd = 0.2),digits = 2),
  group03 = round(rnorm(sample_size,mean = 13, sd = 0.1),digits = 2),
  group04 = round(rnorm(sample_size,mean = 11.5, sd = 0.1),digits = 2),
  group05 = round(rnorm(sample_size,mean = 12, sd = 0.025),digits = 2),
  sample_no = seq(1,sample_size)
) %>% 
  pivot_longer(cols = starts_with("group"),names_to = "group", values_to = "diameter")


save(file = here("data","drive_shaft_data.Rdata"),drive_shaft)

# Drive Shaft failure time data (Weibull)

set.seed(123)  # For reproducibility
n <- 100  # Number of drive shafts
shape <- 2  # Shape parameter
scale <- 500  # Scale parameter (in hours)

# Generate random failure times following Weibull distribution
failure_times <- rweibull(n, shape, scale)

# Create a data frame for plotting
drive_shaft_failure <- data.frame(Time_to_Failure = failure_times)

save(file = here("data","drive_shaft_failures.Rdata"),drive_shaft_failure)

# Drive Shaft wear and tear data (One sample wilcoxon test)

drive_shaft_wear_and_tear <- 
  data.frame(group01 = sample(1:5, 100, replace = TRUE),
             group02 = sample(3:5, 100, replace = TRUE),
             group03 = sample(1:2, 100, replace = TRUE),
             group04 = sample(1:5, 100, replace = TRUE),
             group05 = sample(2:5, 100, replace = TRUE)
  )

save(file = here("data","drive_shaft_wear_and_tear.Rdata"),drive_shaft_wear_and_tear)

# Drive Shaft Operator data (Chi^2 Test of independence)

drive_shaft_chi_sq <- data.frame(
  Defects = c("Yes", "No", "Yes", "No", "No", "Yes", "No", "Yes", "Yes", "No"),
  Operator = c("Operator A", "Operator B", "Operator A", "Operator B", "Operator A",
               "Operator B", "Operator A", "Operator A", "Operator B", "Operator B")
)

save(file = here("data","drive_shaft_chi_sq.Rdata"),drive_shaft_chi_sq)

# Drive shaft correlatioin data (rpm vs. diameter) - Pearson Correlation

samples = 500
r = 0.95


corr_data <-  mvrnorm(n=samples, mu=c(12, 10), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE) %>% as.data.frame()

drive_shaft_rpm_dia <- data.frame(
  rpm = corr_data$V2,
  diameter = corr_data$V1
) %>% 
  mutate(
    rpm = rpm*100
  )


drive_shaft_rpm_dia %>% 
  ggplot(aes(x = rpm, y = diameter))+
  geom_point()

save(file = here("data","drive_shaft_rpm_dia.Rdata"),drive_shaft_rpm_dia)


# Drive Shaft failure data for Spearman Correlation

set.seed(42)  # For reproducibility

n <- 100  # Number of data points

# Generating chi-squared distributed data
data_Y <- cumsum(rchisq(n, df = 2))
data_X <- cumsum(rgamma(n, shape = 2, rate = 3))

drive_shaft_time_defect <- data.frame(
  Defects = round(abs(log(data_X)),digits = 0),
  Production_Time = data_Y
) 

save(file = here("data","drive_shaft_time_defects.Rdata"),drive_shaft_time_defect)

# Drive Shaft group data - but non-normally disitributed - Mann Whitney U Test

set.seed(1230)

sample_size <- 100

drive_shaft_mann_u <- data.frame(
  group01 = round(rgamma(sample_size,rate = 3, shape = 1),digits = 2)+12,
  group02 = round(rexp(sample_size,rate = 12),digits = 2)+12,
  sample_no = seq(1,sample_size)
) %>% 
  pivot_longer(cols = starts_with("group"),names_to = "group", values_to = "diameter")

drive_shaft_mann_u %>% 
  group_by(group) %>% 
  summarise(
    mean_d = mean(diameter)
  )

drive_shaft_mann_u %>% 
  ggplot(aes(x = diameter))+
  geom_histogram()+
  facet_wrap(~group,scales = "free")

drive_shaft_mann_u %>% 
  ggplot(aes(sample = diameter))+
  stat_qq_band()+
  stat_qq_line()+
  stat_qq_point()+
  facet_wrap(~group,scales = "free")

save(file = here("data","drive_shaft_mann_u.Rdata"),drive_shaft_mann_u)

#Drive shaft Treatment data - Paired t-test

# Hypothetical data
before <- c(20.1, 19.8, 20.2, 20.0, 20.3, 20.1, 19.9, 20.0, 19.8, 20.2)
after <- c(20.3, 20.0, 20.4, 20.2, 20.5, 20.2, 20.1, 20.3, 20.0, 20.4)

drive_shaft_treatment <- data.frame(
  smpl_idx = seq_along(before),
  diameter_before = before,
  diameter_after = after
) %>% 
  pivot_longer(cols = starts_with("diameter"),values_to = "diameter",names_to = "timepoint") %>% 
  mutate(
    timepoint = case_when(
      str_detect(timepoint, "before") ~ "t0",
      TRUE~"t1"
    )
  )

save(file = here("data","drive_shaft_treatment.Rdata"),drive_shaft_treatment)

# drive shaft for the paired wilcoxon rank sum test

before <- rgamma(20,shape = 3,rate = 1)
after <- rchisq(20,df = 3)

drive_shaft_wilcox_signed_rank <- data.frame(
  t0 = before+12,
  t1 = after+13,
  smpl_idx = seq_along(before)
) %>% 
  pivot_longer(cols = starts_with("t"),names_to = "timepoint",values_to = "diameter")

save(file = here("data","drive_shaft_wilcox_signed_rank.Rdata"),drive_shaft_wilcox_signed_rank)

# drive shaft data for the kruskal wallis test

# Set seed for reproducibility
set.seed(123)

# Number of observations per group
n <- 50

# Create a dataframe for simulation
kw_shaft_data <- data.frame(
  group = rep(c("Method_A", "Method_B", "Method_C"), each = n),
  strength = c(
    # Simulate strength measurements for Method_A using gamma distribution
    rgamma(n, shape = 2, rate = 0.1)+200,
    # Simulate strength measurements for Method_B using gamma distribution
    rgamma(n, shape = 2.5, rate = 0.09)+150,
    # Simulate strength measurements for Method_C using gamma distribution
    rgamma(n, shape = 1.8, rate = 0.11)+180
  )
)

kw_shaft_data %>% 
  ggplot(aes(sample = strength))+
  stat_qq_band()+
  stat_qq_line()+
  stat_qq_point()+
  facet_wrap(~group)

kw_shaft_data %>% 
  ggplot(aes(x = group, y = strength))+
  geom_violin()

save(file = here("data","drive_shaft_kruskal_wallis.Rdata"),kw_shaft_data)

# drive shaft data for repeated measures ANOVA

set.seed(123)

# Simulating data with no significance between After_Machining and After_Inspection, but significance between Before_Machining and After_Machining
rep_meas_ds <- data.frame(
  Subject_ID = 1:20 %>% as.factor(),
  diameter_Before_Machining = rnorm(20, mean = 12.5, sd = 0.3),
  diameter_After_Machining = rnorm(20, mean = 12.1, sd = 0.3),
  diameter_After_Inspection = rnorm(20, mean = 12.1, sd = 0.3)  # Keeping the same mean as After_Machining
) %>% 
  pivot_longer(
    contains("diameter"),
    values_to = "diameter",
    names_to = "timepoint"
    ) %>% 
  mutate(
    timepoint = str_sub(timepoint,10)
  )

rep_meas_ds %>% 
  anova_test(
    dv = diameter,
    wid = Subject_ID,
    within = timepoint
  ) %>% 
  get_anova_table()



save(file = here("data","drive_shaft_repeated_measures.Rdata"),rep_meas_ds)


# drive shaft multiple linear regression data

samples = 500
r1 = 0.95
r2 = 0.85

site <- c("A","B","C")

corr_mat <- matrix(data = c(1,r1,r2,r2,1,r1,r2,r1,1),ncol = 3,byrow = T) %>% print()

corr_data <-  mvrnorm(n=samples, mu=c(12, 10,40), Sigma=corr_mat, empirical=TRUE) %>% as.data.frame()

drive_shaft_rpm_dia_feed <- data.frame(
  rpm = corr_data$V2,
  diameter = corr_data$V1,
  feed = corr_data$V3,
  site = as.factor(sample(site, size = samples, replace = TRUE))
) %>% 
  mutate(
    rpm = rpm*100,
    # feed = feed*3
  )


drive_shaft_rpm_dia_feed %>% 
  ggplot(aes(x = feed, y = rpm,color = diameter))+
  geom_point()+
  facet_wrap(~site)



save(file = here("data","drive_shaft_rpm_dia_feed.Rdata"),drive_shaft_rpm_dia_feed)

# drive shaft logistic regression data

N=500
feed = rnorm(N,0,1)

error = rnorm(N)

y = feed +error

y_proba = plogis(y,0,1)

y_dummy = rbinom(n = N, size = 1, prob = y_proba)

drive_shaft_log_reg = data.frame(id = 1:N, feed, pass_1_fail_0 = y_dummy) %>% 
  mutate(
    feed = 2*feed + 20,
  ) 

save(file = here("data","drive_shaft_log_reg.Rdata"),drive_shaft_log_reg)


df_sim %>% 
  ggplot(aes(x = feed))+
  # geom_point(aes(y = y, color = "y"))+
  # geom_point(aes(y = y_dummy, color = "y_dummy"))+
  geom_point(aes(y = pass_1_fail_0))

glm_model1 = glm(pass_1_fail_0 ~ feed , data = df_sim, family = 'binomial')

glm_model1 %>% tbl_regression()

predictions <- data.frame(
  feed = seq(1:100)
) %>% 
  add_predictions(model = glm_model1,type = "response",conf)



# sim proportions



prop_fail = data.frame(
  failed_units = rbinom(n = 100,10,0.2)) %>% 
  mutate(
    prop = failed_units/sum(failed_units)
  ) %>% print()
