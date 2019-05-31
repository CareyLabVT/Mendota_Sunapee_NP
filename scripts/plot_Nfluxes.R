# ####
pacman::p_load(glmtools, lubridate, tidyverse)

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

#### Load N flux data; all scenarios ####
Nflux <- bind_rows(read_csv('./output/Mendota_rates_20190419.csv'),
                   read_csv('./output/Sunapee_rates_20190416.csv')) %>% 
  mutate(year = year(DateTime), month = month(DateTime),
         sed_area_m2 = ifelse(Lake =="Mendota", 56502822, 16879430),
         SedDenit_mmold = NIT_sed_nit * sed_area_m2, # Sediment denitrification
         AmmMin_mmold   = NIT_sed_amm * sed_area_m2, # NH4 mineralization
         WCDenit_mmold    = NIT_denit_1 * Tot_V) %>% # water column denitrification
  select(Lake, Sim, year, month, DateTime, SedDenit_mmold, AmmMin_mmold, WCDenit_mmold) %>% 
  filter(month >= 4, month <=10)

#### Summer median fluxes ####
annual_median <- Nflux %>% 
  group_by(Lake, Sim, year) %>% 
  summarize(sed_med = -median(SedDenit_mmold), amm_med = median(AmmMin_mmold), 
            wc_median = median(WCDenit_mmold)) %>% 
  gather(var, value, sed_med:wc_median)

overall_median <- Nflux %>% 
  group_by(Lake, Sim) %>% 
  summarize(sed_med = -median(SedDenit_mmold), amm_med = median(AmmMin_mmold), 
            wc_median = median(WCDenit_mmold)) %>% 
  gather(var, value, sed_med:wc_median)

diffs <- overall_median %>% 
  spread(var, value) %>% 
  mutate(sed_over_amm = sed_med / amm_med)

median_of_medians <- annual_median %>% ungroup() %>% 
  group_by(Lake, Sim, var) %>% 
  summarize(median = median(value))

# Percent change between +0 and +6 scenarios
perChange <- Nflux %>% 
  group_by(Lake, Sim) %>% 
  summarize(sed_med = -median(SedDenit_mmold), amm_med = median(AmmMin_mmold), 
            wc_median = median(WCDenit_mmold)) %>%  
  gather(var, value, sed_med:wc_median) %>% 
  spread(Sim, value) %>% 
  mutate(PerChange = round((`6` - `0`)/`0`*100, 1)) %>% 
  select(Lake, var, PerChange)

##### N flux plots ####
# Ammonium mineralization vs. sediment denitrification
ggplot(subset(overall_median , var %in% c('amm_med','sed_med')), 
       aes(x = Sim, y = value, col = var)) + mytheme + 
  geom_point() +
  geom_smooth() + 
  scale_x_continuous(breaks=seq(0,6,1),limits=c(0,6)) +
  facet_wrap(. ~ Lake, scales = 'free_y')  + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_colour_manual(values=c('dodgerblue4','tomato'),
                      name = "N Flux (mmol/d)", 
                    breaks=c('amm_med', 'sed_med'),
                    labels=c('NH4 Miner.','Sed Denit.'))

# Plot as above, but using yearly medians
ggplot(annual_median, 
       aes(x = jitter(Sim), y = value, col = var)) + mytheme + 
  geom_point() +
  geom_smooth(method='lm') +
  scale_x_continuous("Temp. Increase", breaks=seq(0,6,1),limits=c(0,6)) +
  scale_y_continuous("Flux (mmol/d)") +
  facet_wrap(. ~ Lake, scales = 'free_y')  + 
  guides(color=guide_legend(override.aes=list(fill=NA)))  +
  scale_colour_manual(values=c('dodgerblue4','tomato', 'forestgreen'),
                        name = "Rate", 
                        breaks=c('amm_med', 'sed_med', 'wc_med'),
                        labels=c('NH4 Miner.','Sed Denit.', 'WC Denit'))
  