# ####
pacman::p_load(glmtools, kSamples, lubridate, tidyverse)

jet.colors <- colorRampPalette(c("dodgerblue4",  "dodgerblue1", "cyan", 
                                 "yellow", "orange2", "red1", "red4")) 

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.line.x = element_line(colour = 'black'), 
                 axis.line.y = element_line(colour = 'black'), 
                 axis.text.x=element_text(size=16, colour='black'), 
                 axis.text.y=element_text(size=16, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 legend.text = element_text(size=18), legend.title=element_text(size=18),
                 legend.background = element_blank(), legend.key=element_blank(),
                 strip.text.x = element_text(margin = margin(.25,0,.25,0, 'cm'), size=18),
                 strip.text.y = element_text(margin = margin(0,.25,0,.25, 'cm'), size=18))

#### Load P flux data; all scenarios ####
Pflux <- bind_rows(read_csv('./output/Mendota_psed_20190419.csv'),
                   read_csv('./output/Sunapee_psed_20190904.csv')) %>% 
  mutate(year = year(DateTime), month = month(DateTime),
         sed_area_m2 = ifelse(Lake =="Mendota", 56502822, 16879430),
         SedPflux_mmold = PHS_sed_frp * sed_area_m2) %>%  # Sediment P flux
  select(Lake, Sim, year, month, DateTime, SedPflux_mmold) %>% 
  filter(month >= 4, month <=10)

#### Summer median fluxes ####
annual_median <- Pflux %>% 
  group_by(Lake, Sim, year) %>% 
  summarize(sed_med = median(SedPflux_mmold)) %>% 
  gather(var, value, sed_med)

overall_median <- Pflux %>% 
  group_by(Lake, Sim) %>% 
  summarize(sed_med = median(SedPflux_mmold)) %>% 
  gather(var, value, sed_med)

median_of_medians <- annual_median %>% ungroup() %>% 
  group_by(Lake, Sim, var) %>% 
  summarize(median = median(value))

# Percent change between +0 and +6 scenarios
perChange <- Pflux %>% 
  group_by(Lake, Sim) %>% 
  summarize(sed_med = median(SedPflux_mmold)) %>%  
  gather(var, value, sed_med) %>% 
  spread(Sim, value) %>% 
  mutate(PerChange = round(((`6` - `0`)/`0`)*100, 1)) %>% 
  select(Lake, var, PerChange)

##### P flux plots ####
ggplot(overall_median, aes(x = Sim, y = value, col = var)) + mytheme + 
  geom_point() +
  geom_smooth() + 
  scale_x_continuous(breaks=seq(0,6,1),limits=c(0,6)) +
  facet_wrap(. ~ Lake, scales = 'free_y')  + 
  theme(legend.position='none')

# Plot as above, but using yearly medians
ggplot(annual_median, aes(x = Sim, y = value, col = var)) + mytheme + 
  geom_point() +
  geom_smooth() +
  scale_x_continuous("Temp. Increase", breaks=seq(0,6,1),limits=c(0,6)) +
  scale_y_continuous("Flux (mmol/d)") +
  facet_wrap(. ~ Lake, scales = 'free_y')  + 
  theme(legend.position='none')

# Boxplot
ggplot(annual_median, aes(x = Lake, y = value, fill= as.factor(Sim))) + mytheme+ 
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) +
  scale_fill_manual(values=jet.colors(7),
                    labels=c("+0°C","+1°C","+2°C","+3°C","+4°C","+5°C","+6°C")) +
  #scale_y_continuous(limits=c(0,150), breaks=seq(0,150,25)) +
  facet_wrap(.~Lake, scales='free_y')+
  labs(x ="", y = expression("P sed flux mmol d"^-1)) +
  guides(fill=guide_legend(keyheight=0.1,title="Scenario")) +
  theme(legend.position = c(0.7,1), legend.justification = c(0.7,1))


# AD test ####
ad_P_Men <- annual_median %>% spread(Sim, value) %>% 
  filter(Lake=="Mendota")
ad.test(ad_P_Men$`0`, ad_P_Men$`1`) 
ad.test(ad_P_Men$`0`, ad_P_Men$`2`) 
ad.test(ad_P_Men$`0`, ad_P_Men$`3`)
ad.test(ad_P_Men$`0`, ad_P_Men$`4`) 
ad.test(ad_P_Men$`0`, ad_P_Men$`5`) 
ad.test(ad_P_Men$`0`, ad_P_Men$`6`)

ad_P_Sun <- annual_median %>% spread(Sim, value) %>% 
  filter(Lake=="Sunapee")
ad.test(ad_P_Sun$`0`, ad_P_Sun$`1`) 
ad.test(ad_P_Sun$`0`, ad_P_Sun$`2`) 
ad.test(ad_P_Sun$`0`, ad_P_Sun$`3`)
ad.test(ad_P_Sun$`0`, ad_P_Sun$`4`) 
ad.test(ad_P_Sun$`0`, ad_P_Sun$`5`) 
ad.test(ad_P_Sun$`0`, ad_P_Sun$`6`)
