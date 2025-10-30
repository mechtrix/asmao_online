
data.frame(
  dof = c(1,4,5,seq(10,100,10))
) %>%
  mutate(
    x = list(seq(-4,4,0.01)),
    d = map2(dof,x,function(x,y)dt(y,x)),
    d_norm = map(x,dnorm,mean = 0, sd = 1)
  ) %>%
  mutate(dof = as.factor(dof)) %>%
  unnest(cols = c(x,d,d_norm)) %>%
  ggplot(aes(x = x, y = d, linetype = dof))+
  geom_ribbon(aes(ymax = d_norm,ymin = 0),alpha = 0.4,fill = "azure3",show.legend = F)+
  geom_line()+
  scale_x_continuous(
    expand = c(0,0,0,0)
  )+
  scale_y_continuous(
    expand = c(0,0,0.05,0)
  )+
  labs(title = "t-distribution with varying degrees of freedom",
       subtitle = "As degrees of freedom increase, the t-distribution approaches the normal distribution")
