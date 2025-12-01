library(tidyverse)

dataset <- expand_grid(
  resample = seq(1,5),
  sample_index = seq(1,20)
)

resample_01 <- data.frame(idx = 1, data = c(2009,2012,2011,2011,2015,2023,2028,2019,2011,2007,2015,2014,2010,2020,2011,2005,2021,2008,2011,2012))
resample_02 <-  data.frame(idx = 2, data = c(2009,2004,2010,2008,2012,2011,2018,2012,2015,2015,2008,2005,2020,2009,2023,2002,2002,2007,2007,2012))
resample_03 <-  data.frame(idx = 3, data = c(2018,2004,2004,2021,2019,2015,2023,2022,2015,2007,2019,2017,2014,2018,2016,2023,2011,2009,2012,2010))
resample_04 <-  data.frame(idx = 4, data = c(2019,2018,2011,2018,2004,2019,2013,2006,2011,2011,2022,2018,2022,2009,2015,2015,2021,2016,2009,2003))
resample_05 <-  data.frame(idx = 5, data = c(2009,2007,2004,2006,2002,2020,2019,2015,2004,2009,2011,2023,2011,2013,2019,2014,2003,2018,2011,2023))

mean(coins_sample$year)

dataset <- bind_rows(
  resample_01,
  resample_02,
  resample_03,
  resample_04,
  resample_05
)

dataset_sum <- dataset |> 
  group_by(idx) |> 
  summarize(
    mean_year = mean(data),
    sd_year = sd(data)
  )

dataset |> 
  ggplot(
    aes(
      x = data,
      color = as.factor(idx),
      fill = as.factor(idx)
    )
  )+
  geom_histogram()+
  theme_minimal()+
  facet_wrap(~idx)
