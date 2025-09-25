library(readxl)

runs <- seq(1, 10)
appraiser <- seq(1, 3)
part <- seq(1, 3)

df <- expand_grid(
  runs = runs,
  appraiser = appraiser,
  part = part
) %>%
  rowid_to_column(
    var = "idx"
  )


df_shuffle = df[sample(1:nrow(df)), ]

write.csv(df_shuffle, file = here("ss25", "gagerr.csv"))

### gage rr done

gagerr <- read_excel("ss25/gagerr.xlsx")

gage <- gagerr |>
  mutate(
    result_num = parse_number(result)
  )

tmp <- gagerr %>%
  filter(runs %in% c(1, 2, 2, 3, 4, 5)) %>%
  select(
    runs,
    appraiser,
    part,
    results
  )

ss.rr(
  var = result,
  part = part,
  appr = appraiser,
  data = gagerr,
  method = "crossed"
)


gage |>
  ggplot(
    aes(
      x = runs,
      y = result_num
    )
  ) +
  geom_point() +
  geom_line() +
  facet_wrap(part ~ appraiser, labeller = label_both)

gage %>%
  ggplot(
    aes(
      x = result_num
    )
  ) +
  geom_density() +
  facet_grid(
    appraiser ~ part,
    labeller = label_both,
    scales = "free"
  )

tmp <- gage |>
  summarise(
    mean_r = mean(result_num)
  )

gage |>
  ggplot(
    aes(
      x = as.factor(appraiser),
      y = result_num,
    )
  ) +
  geom_boxplot() +
  geom_hline(yintercept = tmp$mean_r) +
  facet_wrap(~part)


ss.rr(
  var = result_num,
  part = part,
  appr = appraiser,
  data = gage,
  method = "crossed"
)
