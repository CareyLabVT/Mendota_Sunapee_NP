pacman::p_load(akima, cowplot, tidyverse)
#options(scipen = 999)

sun <-  read_csv('./Sunapee/GLM/observed_data/raw/sunapee_area_by_depth.csv')

men <- read_csv('./Mendota/GLM/observed_data/raw/mendota_area_by_depth.csv') 

# What % of Mendota area is at/below 20.1m? ####
men %>% select(Depth, Area) %>% 
  spread(Depth, Area) %>% 
  select('20.1','0') %>% 
  mutate(PropArea = 7777000/39866000*100)
# 19.5% at/below 20.1m (80.5% area in shallower water)

# What depth in Sunapee leaves 19.5% of lake area below it?
sun2 <- sun %>% select(Depth, Area) %>% 
  mutate(PropArea = round(Area/16934251.60000*100, 1))
# 18m is 19.3% at/below

# Plot hyposgraphic curves ####
a <- ggplot(sun, aes(x = Area, y = Depth)) +
  geom_point() + scale_x_continuous(position = "top") +
  scale_y_reverse(lim=c(35,0), breaks=seq(0,33, 3)) +
  annotate("text", x = 1.25e+07, y = 33, label= "Sunapee", size=14) +
  geom_hline(yintercept = 18, lty=2) +
  geom_vline(xintercept = 3264022.975, lty=2)

b <- ggplot(men, aes(x = Area, y = Depth)) +
  geom_point() + geom_line()+
  scale_x_continuous(position = "top") +
  scale_y_reverse(lim=c(26,0), breaks=seq(0,25,5)) +
  annotate("text", x = 3e+07, y = 25, label= "Mendota", size=14)+
  geom_hline(yintercept = 20, lty=2) +
  geom_vline(xintercept = 7777000, lty=2)

plot_grid(a, b, ncol=2)
