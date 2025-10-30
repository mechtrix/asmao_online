runs <- seq(1,5)
appraiser <- seq(1,3)
part <- seq(1,3)

df <- expand_grid(
  runs = runs,
  appraiser = appraiser,
  part = part
) %>% 
  rowid_to_column(
    var = "idx"
  )


df_shuffle = df[sample(1:nrow(df)), ] 

write.csv(df_shuffle,file = here("ws2425","gagerr.csv"))

### gage rr done

gagerr <- read_excel("ws2425/gagerr.xlsx") 
                     

tmp <- gagerr %>% 
  filter(runs %in% c(1,2,2,3,4,5)) %>% 
  select(
    runs, appraiser, part, results
  )

ss.rr(var = result, part = part, appr = appraiser, data = gagerr, method = "crossed")


gagerr |> 
  ggplot(
    aes(
      x = runs,
      y = result
    )
  )+
  geom_point()+
  geom_line()+
  facet_wrap(part~appraiser,
             labeller = label_both)

gagerr %>% 
  ggplot(
    aes(
      x = result
    )
  )+
  geom_density()+
  facet_grid(
    appraiser~part,
    labeller = label_both,
    scales = "free"
  )
