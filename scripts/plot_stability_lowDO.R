##### Plot stability and days of low DO ####
pacman::p_load(cowplot, tidyverse, lubridate)

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

#### Read in stability data ####
raw <- read_csv('./output/stability_all.csv') %>%
  mutate(Sim = as.factor(Sim), year = year(DateTime), month = month(DateTime))

sim_diffs <-  raw %>%
  filter(month >= 4, month <=10, Sim %in% c('0','6')) %>%
  group_by(Lake, Sim) %>%
  summarize_all(median) %>% select(Lake, Sim, St) %>% 
  spread(Sim, St) %>% 
  mutate('0_v_6' = round(((`6` - `0`)/`0`*100), 1))

# Apr 1 - Oct 31 "yearly" medians by sim and lake
active_median <- raw %>%
  filter(month >= 4, month <=10, Sim != '7') %>%
  group_by(Lake, Sim, year) %>%
  summarize_all(median)  %>% select(-DateTime) %>% 
  mutate(Trophic = ifelse(Lake =="Mendota", "High-nutrient", "Low-nutrient"))

extremes <- active_median %>% filter(Sim %in% c(0,6))

#### Schmidt plot: resistance to mixing due to the potential energy  ####
# inherent in the stratification of the water column
schmidt_overall <- raw %>% select(Lake, Sim, St, month) %>% 
  filter(month >= 4, month <=10, Sim != '7') %>%
  group_by(Lake, Sim) %>%
  summarize(median = median(St)) %>% 
  spread(Sim, median) %>% 
  mutate('0_v_6' = round(((`6` - `0`)/`0`*100), 1))

schmidt_year_by_year <- raw %>% select(Lake, Sim, St, year, month) %>% 
  filter(month >= 4, month <=10, Sim != '7') %>%
  group_by(Lake, Sim, year) %>% 
  summarize(median=median(St)) %>% 
  spread(Sim, median) %>% 
  mutate('0_v_6' = round(((`6` - `0`)/`0`*100), 0))

median_of_extremes <- extremes %>% # for "among-year median changes" in text
  group_by(Lake, Sim) %>% 
  summarize(median = median(St))%>% 
  spread(Sim, median) %>% 
  mutate('0_v_6' = round(((`6` - `0`)/`0`*100), 0))

schmidt_all <- ggplot(active_median, aes(x = Trophic, y = St, fill= Sim)) + mytheme + 
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) +
  scale_fill_manual(values=jet.colors(7)) +
  scale_y_continuous(limits=c(100,610), breaks=seq(100,600,100)) +
  #labs(x ="", y = expression(atop(paste("Summer Schmidt stability"),
  #                                (J~m^-2)))) +
  labs(x ="", y = expression(Summer~Schmidt~stability~(J~m^-2)))+
  guides(fill=guide_legend(keyheight=0.1,title="Scenario")) +
  theme(legend.position = 'none')

#### Count days of low DO ####
# Load output from GRAPLEr sims, both lakes ## 
lakes <- bind_rows(read_csv('./output/Mendota_11AprAll_20190419.csv'), 
                   read_csv('./output/Sunapee_11AprAll_20190416.csv')) %>% 
  select(DateTime, Lake, Sim, depth, DO_mgL) %>% 
  mutate(Sim = as.factor(Sim),
         year = year(DateTime), month = month(DateTime), DOY = yday(DateTime)) 

# Calculate low DO days ####
rawDO <- lakes %>% 
  filter(ifelse(Lake == "Mendota", depth == '20', depth == '18')) %>% 
  mutate(LowDO = ifelse(DO_mgL < 2, "Y", "N"))

# Calculate duration, start/end of hypoxia ####
hypox_duration <- rawDO %>% 
  filter(LowDO == "Y") %>% 
  select(Lake, Sim, DateTime, year, DOY) %>% 
  group_by(Lake, Sim, year) %>% 
  summarize(max = max(DOY), min = min(DOY)) %>% 
  mutate(duration = max-min)

med_duration <- hypox_duration %>% select(-year) %>% 
  group_by(Lake, Sim) %>% 
  summarize_all(median)

duration_change <- hypox_duration %>% 
  select(-max, - min) %>% 
  group_by(Lake, year) %>% 
  spread(Sim, duration) %>% 
  mutate(Diff = `6`-`0`)

turnover_change <- hypox_duration %>% 
  select(-duration, - min) %>% 
  group_by(Lake, year) %>% 
  spread(Sim, max) %>% 
  mutate(Diff = `6`-`0`)

onset_change <- hypox_duration %>% 
  select(-duration, - max) %>% 
  group_by(Lake, year) %>% 
  spread(Sim, min) %>% 
  mutate(Diff = `6`-`0`)

# Median days of low DO ####
DO <- rawDO %>%
  filter(month >=4, month <=10) %>%  # 214 days: Apr 1 - Oct 31
  group_by(Lake, Sim, year, LowDO) %>% 
  summarize(nDays = n_distinct(DateTime)) %>%
  spread(LowDO, nDays) %>% 
  mutate(Y = ifelse(is.na(Y), 0, Y))%>% 
  mutate(Trophic = ifelse(Lake =="Mendota", "High-nutrient", "Low-nutrient"))

DOmed <- DO %>% 
  group_by(Sim, Lake) %>%
  summarize(median = median(Y), min= min(Y), max = max(Y)) 

lowDO_all <- ggplot(DO, aes(x = Trophic, y = Y, fill= Sim)) + mytheme+ 
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) +
  scale_fill_manual(values=jet.colors(7),
                    labels=c("+0°C","+1°C","+2°C","+3°C","+4°C","+5°C","+6°C")) +
  scale_y_continuous(limits=c(0,150), breaks=seq(0,150,25)) +
  labs(x ="", y = expression(Summer~hypoxia~days~(italic(n)))) +
  guides(fill=guide_legend(keyheight=0.1,title="Scenario")) +
  theme(legend.position = c(1,1), legend.justification = c(1,1))

# Plot all scenarios, stability & lowDO ####
fig6 <- plot_grid(schmidt_all, lowDO_all, labels='AUTO', align='hv')

tiff(filename = "./output/figures/Figure6.tif", width = 10, height = 5, units = "in", compression = c("none"),res = 500)
fig6
dev.off()
