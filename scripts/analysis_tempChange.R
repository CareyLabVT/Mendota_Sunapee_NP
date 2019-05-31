# Calculate changes in water temperature between scenarios ####
pacman::p_load(tidyverse, lubridate)
options(scipen = 999)
jet.colors <- colorRampPalette(c("dodgerblue4",  "dodgerblue1", "cyan", 
                                 "yellow", "orange2", "red1", "red4")) 

mytheme <- theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), 
                 legend.background = element_blank(),
                 legend.box.background = element_blank(),
                 legend.key=element_blank(),
                 axis.line.x = element_line(colour = "black"), 
                 axis.line.y = element_line(colour = "black"), 
                 axis.text.x = element_text(size=18, colour = "black"), 
                 axis.text.y = element_text(size=18, colour='black'), 
                 axis.title.x=element_text(size=18), 
                 axis.title.y=element_text(size=18),
                 legend.title.align=0.5, legend.text.align=1,
                 legend.title = element_text(size=14, colour='black'),
                 legend.text = element_text(size=12, colour='black'))

# Load output from GRAPLEr sims, both lakes #### 
lakes <- bind_rows(read_csv('./output/Mendota_11AprAll_20190419.csv'), 
                   read_csv('./output/Sunapee_11AprAll_20190416.csv')) 

temps <- lakes %>% 
  select(DateTime, Lake, Sim, depth, Temp) %>% 
  mutate(Sim = as.factor(Sim), year = year(DateTime), month = month(DateTime)) %>% 
  mutate(DateTime = as.Date(DateTime)) %>% 
  filter(ifelse(Lake == "Mendota", depth %in% c(0,20), depth %in% c(0,18)),
         month >=4, month <=10, Sim %in% c('0','6'))
  
temp_year_by_year <- temps %>% 
  group_by(Lake, Sim, year, depth) %>%
  summarize(maxT = max(Temp), median = median(Temp)) %>% 
  gather(variable, value, maxT:median) 

# Per-year percent changes between scenarios
per_change <- temp_year_by_year %>% 
  spread(Sim, value) %>% 
  mutate(Diff = round(`6`-`0`,1), PerChange = round(((`6` - `0`)/`0`*100), 1))

# Overall changes between scenarios
overall_median <- temps %>% 
  group_by(Lake, Sim, depth) %>%
  summarize(maxT = max(Temp), median = median(Temp)) %>% 
  gather(variable, value, maxT:median) %>% 
  spread(Sim, value) %>% 
  mutate(Diff = round(`6`-`0`,1), PerChange = round(((`6` - `0`)/`0`*100), 1))


# Boxplot of year-by-year changes in max, median temps
ggplot(temp_year_by_year, aes(x = Lake, y = value, fill= Sim)) + mytheme+ 
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend=F) +
  facet_wrap(variable ~ depth, scales='free_y') +
  scale_fill_manual(values=c('#104E8B','#8B0000')) +
  labs(x ="", y = expression(Summer~water~temp~(degree*C))) +
  guides(fill=guide_legend(keyheight=0.1,title="Scenario")) +
  theme(legend.position = 'none')

#### Year-by-year scenario comparisons by lake
# Mendota
ggplot(subset(temp_year_by_year, Lake=="Mendota"), aes(x = year, y = value, group=as.factor(year),
                              col= as.factor(year), pch=Sim)) + mytheme+ 
  geom_point(position=position_dodge(width = .75), cex= 4) +
  geom_path()+
  facet_wrap(variable ~ depth) +
  scale_x_continuous(limits=c(2004,2014))+
  labs(x ="", y = expression(Summer~water~temp~(degree*C))) +
  guides(pch=guide_legend(keyheight=0.1,title="Scenario"), col='none')

# Sunapee
ggplot(subset(temp_year_by_year, Lake=="Sunapee"), aes(x = year, y = value, group=as.factor(year),
                                                       col= as.factor(year), pch=Sim)) + mytheme+ 
  geom_point(position=position_dodge(width = .75), cex= 4) +
  geom_path()+
    facet_wrap(variable ~ depth) +
  scale_x_continuous(limits=c(2004,2014))+
  labs(x ="", y = expression(Summer~water~temp~(degree*C))) +
  guides(pch=guide_legend(keyheight=0.1,title="Scenario"), col='none')

#### Calculate duration of stratification in +0, +6 scenarios ####
# Strat defined by > 1 degree difference between surface, deep (Woolway et al 14)
  # Using deep as defined per lake in low-DO days (20m Men; 18m Sun)
strat <- lakes %>% 
  select(DateTime, Lake, Sim, depth, Temp) %>% 
  filter(ifelse(Lake == "Mendota", depth %in% c('0','20'), depth %in% c('0','18'))) %>% 
  group_by(Lake, Sim) %>% 
  spread(depth, Temp) %>% 
  mutate(DOY = yday(DateTime), year = year(DateTime),
         Diff = round(ifelse(Lake == "Mendota", (`0`-`20`), (`0`-`18`)),2),
         Stratified = ifelse(Diff > 1, "Y", "N"))

strat_duration <- strat %>% 
  filter(Stratified == "Y") %>% 
  select(Lake, Sim, DateTime, year, DOY, Diff) %>% 
  group_by(Lake, Sim, year) %>% 
  summarize(max = max(DOY), min = min(DOY)) %>% 
  mutate(duration = max-min)

duration_plot <- strat_duration %>% 
  gather(variable, value, max:duration)

ggplot(duration_plot, aes(x = Sim, y = value, col=as.factor(year))) +
  geom_point() + 
  facet_grid(Lake~variable, scales='free_y')

ggplot(duration_plot, aes(x = Lake, y = value, fill=as.factor(Sim))) +
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) +
  scale_fill_manual(values=jet.colors(7),
                    labels=c("+0°C","+1°C","+2°C","+3°C","+4°C","+5°C","+6°C"))+
  facet_grid(variable~., scales='free_y')

med_duration <- strat_duration %>% select(-year) %>% 
  group_by(Lake, Sim) %>% 
  summarize_all(median)

duration_change <- strat_duration %>% 
  select(-max, - min) %>% 
  group_by(Lake, year) %>% 
  spread(Sim, duration) %>% 
  mutate(Diff = `6`-`0`)

turnover_change <- strat_duration %>% 
  select(-duration, - min) %>% 
  group_by(Lake, year) %>% 
  spread(Sim, max) %>% 
  mutate(Diff = `6`-`0`)

turnover_change %>% select(Lake, Diff) %>% 
  group_by(Lake) %>% 
  summarize(turnover_c = median(Diff))

onset_change <- strat_duration %>% 
  select(-duration, - max) %>% 
  group_by(Lake, year) %>% 
  spread(Sim, min) %>% 
  mutate(Diff = `6`-`0`)

onset_change %>%select(Lake, Diff) %>% 
  group_by(Lake) %>% 
  summarize(onset_c = median(Diff)