# Plot TN/TP distributions ####
pacman::p_load(cowplot, generics, ggridges, kSamples, tidyverse, lubridate)

options(scipen = 999) # disable scientific notation

jet.colors <- colorRampPalette(c("dodgerblue4",  "dodgerblue1", "cyan", 
                                 "yellow", "orange2", "red1", "red4")) 

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.line.x = element_line(colour = 'black'), 
                 axis.line.y = element_line(colour = 'black'), 
                 axis.text.x=element_text(size=16, colour='black'), 
                 axis.text.y=element_text(size=16, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 legend.text = element_text(size=18), legend.title=element_text(size=18),
                 strip.text.x = element_text(margin = margin(.25,0,.25,0, 'cm'), size=18),
                 strip.text.y = element_text(margin = margin(0,.25,0,.25, 'cm'), size=18))

# Load output from GRAPLEr sims, both lakes #### 
lakes <- bind_rows(read_csv('./output/Mendota_11AprAll_20190419.csv'), 
                   read_csv('./output/Sunapee_11AprAll_20190416.csv')) %>%
  select(-Tot_V) %>% 
  mutate(TNTP = (TN_mgL/TP_mgL)*(30.97/14.01))

# Summarize output as median annual values ####
lakeYears <- lakes %>% 
  mutate(year = year(DateTime), month = month(DateTime),
         WaterYear = ifelse(month > 10, year+1, year)) %>% #Water year Nov. 1 to Oct. 31
  filter(WaterYear >2004 & WaterYear < 2015, # exclude partial year
         ifelse(Lake == "Mendota", depth %in% c(0,20), depth %in% c(0,18))) %>% 
  mutate(Sim = as.factor(Sim))

annual_summary <- lakeYears %>% 
  select(Lake, Sim, depth, WaterYear, Temp:PON_mgL, TNTP) %>%
  gather(variable, value, Temp:TNTP) %>%
  group_by(Lake, Sim, WaterYear, depth, variable) %>%
  summarize(median = median(value, na.rm=T))

activeseason_summary <- lakes %>% 
  mutate(year = year(DateTime), month = month(DateTime)) %>% #Water year Nov. 1 to Oct. 31
  filter(month >=4, month<=10,  # Select 'active months'
         ifelse(Lake == "Mendota", depth %in% c(0,20), depth %in% c(0,18))) %>% 
  mutate(Sim = as.factor(Sim)) %>% 
  select(Lake, Sim, year, depth, Temp:PON_mgL, TNTP) %>%
  gather(variable, value, Temp:TNTP) %>%
  group_by(Lake, Sim, year, depth, variable) %>%
  summarize(median = median(value, na.rm=T)) %>% 
  mutate(Trophic = ifelse(Lake =="Mendota", "High-nutrient", "Low-nutrient"))

active_totalChange <- lakes %>% 
  mutate(year = year(DateTime), month = month(DateTime)) %>% 
  filter(month >=4, month<=10, # Select 'active months'
         ifelse(Lake == "Mendota", depth %in% c(0,20), depth %in% c(0,18))) %>% 
  mutate(Sim = as.factor(Sim)) %>% 
  filter(Sim %in% c('0','6')) %>%
  select(Lake, Sim, depth, TN_mgL, TP_mgL, TNTP) %>%
  gather(variable, value, TN_mgL:TNTP) %>%
  group_by(Lake, Sim, depth, variable) %>%
  summarize(median = median(value, na.rm=T)) %>% 
  spread(Sim, median) %>% 
  mutate(PerChange = round((`6`-`0`)/`0`*100, 1))

overall_summary <- lakeYears %>% 
  select(Lake, Sim, year, depth, DO_mgL:PON_mgL, TNTP) %>%
  gather(variable, value, DO_mgL:TNTP) %>%
  group_by(Lake, Sim, depth, variable) %>%
  summarize(median_ugL = median(value, na.rm=T)*1000)

# Joyplot: Epi TN ####
tn <- ggplot(data=subset(activeseason_summary, variable=='TN_mgL' & depth=='0'), 
       aes(x= median*1000, y = Sim, fill=Sim, group = Sim)) +
  geom_density_ridges() + mytheme + #mytheme + 
  facet_grid(.~ Trophic, scales="free_x") +
  scale_fill_manual(values=jet.colors(7)) +
  scale_y_discrete(breaks=seq(0,6,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='none') +
  labs(x=(expression(Total~nitrogen~(mu*g~L^-1))), 
       y = (expression(Air~temperature~increase~(degree*C)))) 

# Boxplot 
ggplot(subset(activeseason_summary, variable=='TN_mgL'),
       aes(x = Sim, y = median*1000, group=as.factor(Sim),fill= as.factor(year))) + mytheme+ 
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) +
  #scale_fill_manual(values=jet.colors(7),
  #                  labels=c("+0°C","+1°C","+2°C","+3°C","+4°C","+5°C","+6°C")) +
  #geom_text(aes(label=as.factor(WaterYear)),position=position_dodge(width = .75),hjust=1, vjust=0, cex=3, col='black') +
  facet_wrap(Lake~depth, scales='free_y')+
  labs(x ="", y = expression("TN ug L"^-1)) +
  guides(fill=guide_legend(keyheight=0.1,title="Scenario"))# +
  theme(legend.position = 'none')

# Joyplot: Epi TP ####
tp <- ggplot(data=subset(activeseason_summary, variable=='TP_mgL' & depth=='0'), 
       aes(x= (median*(1000)), y = Sim, fill=Sim, group = Sim)) +
  geom_density_ridges() + mytheme + #mytheme + 
  facet_grid(. ~ Trophic, scales = 'free_x') +
  scale_fill_manual(values=jet.colors(7)) +
  scale_shape_manual(values=c(21, 24)) +
  #scale_x_continuous(limits=c(0,4), breaks=seq(0,4,1)) +
  scale_y_discrete(breaks=seq(0,6,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  labs(x=(Total~phosphorus~(mu*g~L^-1)), y = "")
 
# Boxplot
ggplot(subset(activeseason_summary, variable=='TP_mgL'),
       aes(x = Sim, y = median*1000, group=as.factor(Sim), fill= as.factor(year))) + mytheme+ 
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) +
  #scale_fill_manual(values=jet.colors(7),
  #                  labels=c("+0°C","+1°C","+2°C","+3°C","+4°C","+5°C","+6°C")) +
  #geom_text(aes(label=as.factor(WaterYear)),position=position_dodge(width = .75),hjust=1, vjust=0, cex=3, col='black') +
  facet_wrap(Lake~depth, scales='free_y')+
  labs(x ="", y = expression("TP ug L"^-1)) +
  guides(fill=guide_legend(keyheight=0.1,title="Year")) #+
  theme(legend.position = 'none')

test <- activeseason_summary %>% filter(variable=="TP_mgL", depth =='0')
ggplot(data=test, 
       aes(x= median*1000, y = Sim, fill=Sim, group = Sim)) +
  geom_density_ridges() + mytheme + #mytheme + 
  geom_point(data = subset(test, Sim =='0'), aes(x = median*1000, y = Sim),
             pch=23, col='black',fill='yellow', cex=2)+
  facet_grid(.~ Lake, scales="free_x") +
  scale_fill_manual(values=jet.colors(7)) +
  scale_y_discrete(breaks=seq(0,6,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='none') +
  labs(x=(expression(Total~phosphorus~(mu*g~L^-1))), 
       y = (expression(Air~temperature~increase~(degree*C)))) 

# Joyplot: Epi NP ####
np <- ggplot(data=subset(activeseason_summary, variable=='TNTP' & depth=='0'), 
             aes(x= median, y = Sim, fill=Sim, group = Sim)) +
  geom_density_ridges() + mytheme + #mytheme + 
  facet_grid(. ~ Trophic, scales = 'free_x') +
  scale_fill_manual(values=jet.colors(7)) +
  scale_shape_manual(values=c(21, 24)) +
  #scale_x_continuous(limits=c(0,4), breaks=seq(0,4,1)) +
  scale_y_discrete(breaks=seq(0,6,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  labs(x="TN:TP (molar)", y = "")

# Boxplot
ggplot(subset(activeseason_summary, variable=='TNTP'),
       aes(x = Sim, y = median, fill= as.factor(Sim))) + mytheme+ 
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) +
  scale_fill_manual(values=jet.colors(7),
                    labels=c("+0°C","+1°C","+2°C","+3°C","+4°C","+5°C","+6°C")) +
  #geom_text(aes(label=as.factor(WaterYear)),position=position_dodge(width = .75),hjust=1, vjust=0, cex=3, col='black') +
  facet_wrap(Lake~depth, scales='free_y')+
  labs(x ="", y = "Molar TN:TP") +
  guides(fill=guide_legend(keyheight=0.1,title="Scenario")) +
  theme(legend.position = 'none')

joy <- plot_grid(tn, tp, np, nrow=1, labels='AUTO', align='hv')

tiff(filename = "./output/figures/Figure7.tif", width = 12.14, height = 4.84, units = "in", compression = c("none"),res = 500)
joy
dev.off()

# Anderson-Darling test, TN ####
# Mendota epi TN
ad_TN_Men <- activeseason_summary %>% spread(Sim, median) %>% 
  filter(Lake=="Mendota", variable == 'TN_mgL', depth == '0')
ad.test(ad_TN_Men$`0`, ad_TN_Men$`1`)
ad.test(ad_TN_Men$`0`, ad_TN_Men$`2`)
ad.test(ad_TN_Men$`0`, ad_TN_Men$`3`)
ad.test(ad_TN_Men$`0`, ad_TN_Men$`4`)
ad.test(ad_TN_Men$`0`, ad_TN_Men$`5`)
ad.test(ad_TN_Men$`0`, ad_TN_Men$`6`)

# Sunapee epi TN
ad_TN_Sun <- activeseason_summary %>% spread(Sim, median) %>% 
  filter(Lake=="Sunapee", variable == 'TN_mgL', depth == '0')
ad.test(ad_TN_Sun$`0`, ad_TN_Sun$`1`) # NS
ad.test(ad_TN_Sun$`0`, ad_TN_Sun$`2`) # S
ad.test(ad_TN_Sun$`0`, ad_TN_Sun$`3`)
ad.test(ad_TN_Sun$`0`, ad_TN_Sun$`4`)
ad.test(ad_TN_Sun$`0`, ad_TN_Sun$`5`)
ad.test(ad_TN_Sun$`0`, ad_TN_Sun$`6`)

# Anderson-Darling test, TP ####
ad_TP_Men <- activeseason_summary %>% spread(Sim, median) %>% 
  filter(Lake=="Mendota", variable == 'TP_mgL', depth == '0')
ad.test(ad_TP_Men$`0`, ad_TP_Men$`1`)
ad.test(ad_TP_Men$`0`, ad_TP_Men$`2`)
ad.test(ad_TP_Men$`0`, ad_TP_Men$`3`)
ad.test(ad_TP_Men$`0`, ad_TP_Men$`4`)
ad.test(ad_TP_Men$`0`, ad_TP_Men$`5`)
ad.test(ad_TP_Men$`0`, ad_TP_Men$`6`)

ad_TP_Sun <- activeseason_summary %>% spread(Sim, median) %>% 
  filter(Lake=="Sunapee", variable == 'TP_mgL', depth == '0')
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`1`)
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`2`)
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`3`)
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`4`)
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`5`)
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`6`)

# Anderson-Darling test, N:P ####
ad_NP_Men <- activeseason_summary %>% spread(Sim, median) %>% 
  filter(Lake=="Mendota", variable == 'TNTP', depth == '0')
ad.test(ad_NP_Men$`0`, ad_NP_Men$`1`)
ad.test(ad_NP_Men$`0`, ad_NP_Men$`2`)
ad.test(ad_NP_Men$`0`, ad_NP_Men$`3`)
ad.test(ad_NP_Men$`0`, ad_NP_Men$`4`)
ad.test(ad_NP_Men$`0`, ad_NP_Men$`5`)
ad.test(ad_NP_Men$`0`, ad_NP_Men$`6`)

ad_NP_Sun <- activeseason_summary %>% spread(Sim, median) %>% 
  filter(Lake=="Sunapee", variable == 'TNTP', depth == '0')
ad.test(ad_NP_Sun$`0`, ad_NP_Sun$`1`)
ad.test(ad_NP_Sun$`0`, ad_NP_Sun$`2`)
ad.test(ad_NP_Sun$`0`, ad_NP_Sun$`3`)
ad.test(ad_NP_Sun$`0`, ad_NP_Sun$`4`)
ad.test(ad_NP_Sun$`0`, ad_NP_Sun$`5`)
ad.test(ad_NP_Sun$`0`, ad_NP_Sun$`6`)

# IQRs for variability ####
n_p <- activeseason_summary %>% 
  filter(variable %in% c('TN_mgL', 'TP_mgL', 'TNTP')) %>% 
  mutate(median = ifelse(variable != 'TNTP', median*1000, median))  # TN, TP ugL

quantiles <- n_p %>% 
  group_by(Lake, Sim, depth, variable) %>% 
  do(tidy(quantile(.$median))) %>% 
  spread(names, x) %>% 
  mutate(IQR = round((`75%`-`25%`),2),
         QCD = round(((`75%`-`25%`)/(`75%`+`25%`)),2)) %>% # Quartile Coefficient of Dispersion
  select(Lake:variable, IQR:QCD)
  
epi_hypo <- quantiles %>%  
  spread(depth, QCD) %>% 
  mutate(epi_minus_hypo = `0`-`20`,
         epi_prop = round(`0`/`20`,2),
         epi_percent = round(((`0`-`20`)/`20`*100),2))

# Joyplot: Hypo TN ####
ggplot(data=subset(annual_summary, variable=='TN_mgL' & depth%in% c('18','20')), 
       aes(x= median*1000, y = Sim, fill=Sim, group = Sim)) +
  geom_density_ridges() + mytheme + #mytheme + 
  facet_grid(.~ Lake, scales="free_x") +
  scale_fill_manual(values=jet.colors(7)) +
  scale_y_discrete(breaks=seq(0,7,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='none') +
  labs(x=(expression(median~hypolimnion~total~nitrogen~(mu*g~L^-1))), 
       y = (expression(Air~temperature~increase~(degree*C)))) 

# Joyplot: Hypo TP ####
ggplot(data=subset(annual_summary, variable=='TP_mgL' & depth%in% c('18','20')), 
       aes(x= (median*(1000)), y = Sim, fill=Sim, group = Sim)) +
  geom_density_ridges() + mytheme + #mytheme + 
  facet_grid(. ~ Lake, scales = 'free_x') +
  scale_fill_manual(values=jet.colors(7)) +
  scale_shape_manual(values=c(21, 24)) +
  #scale_x_continuous(limits=c(0,4), breaks=seq(0,4,1)) +
  scale_y_discrete(breaks=seq(0,6,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none') +
  labs(x=(median~hypolimnion~total~phosphorus~(mu*g~L^-1)), 
       y = "")

# Anderson-Darling test, deep TP ####
ad_TP_Men <- activeseason_summary %>% spread(Sim, median) %>% 
  filter(Lake=="Mendota", variable == 'TP_mgL', depth == '20')
ad.test(ad_TP_Men$`0`, ad_TP_Men$`1`) # p = 0.9541
ad.test(ad_TP_Men$`0`, ad_TP_Men$`2`) # p = 0.9545
ad.test(ad_TP_Men$`0`, ad_TP_Men$`3`) # p = 0.1817
ad.test(ad_TP_Men$`0`, ad_TP_Men$`4`) # p = 0.0770 
ad.test(ad_TP_Men$`0`, ad_TP_Men$`5`) # p = 0.07474
ad.test(ad_TP_Men$`0`, ad_TP_Men$`6`) # p = 0.02127

ad_TP_Sun <- activeseason_summary %>% spread(Sim, median) %>% 
  filter(Lake=="Sunapee", variable == 'TP_mgL', depth == '18')
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`1`) # p = 0.8217
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`2`) # p = 0.2851
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`3`) # p = 0.1403
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`4`) # p = 0.04949
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`5`) # p = 0.02450
ad.test(ad_TP_Sun$`0`, ad_TP_Sun$`6`) # p = 0.01358

# Joyplot: Epi and Hypo water temps ####
ggplot(data=subset(annual_summary, variable=='Temp' & depth %in% c('0','18','20')), aes(x= median, y = Sim, fill=Sim, group = Sim)) +
  geom_density_ridges() + mytheme + 
  facet_grid(depth ~ Lake) +
  scale_fill_manual(values=jet.colors(7)) +
  scale_shape_manual(values=c(21, 24)) +
  #scale_x_continuous(limits=c(5,20), breaks=seq(5,20,5)) +
  scale_y_discrete(limits=seq(0,6,1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x=(expression(median~annual~water~temperature~(degree*C))), y = (expression(Air~temperature~increase~(degree*C))))

# Standardize daily values against baseline ####
annual_med_norm <- activeseason_summary %>%
  spread(Sim, median) %>% 
  group_by(Lake, year, depth, variable) %>% 
  mutate(`1` = `1`/`0`,
         `2` = `2`/`0`,
         `3` = `3`/`0`,
         `4` = `4`/`0`,
         `5` = `5`/`0`,
         `6` = `6`/`0`) %>% na.omit() %>% select(-`0`) %>% 
  gather(Sim, norm_median, `1`:`6`)

annual_sum_norm <- activeseason_summary %>%
  select(-median) %>% 
  spread(Sim, sum) %>% 
  group_by(Lake, WaterYear, zone, variable) %>% 
  mutate(`1` = `1`/`0`,
         `2` = `2`/`0`,
         `3` = `3`/`0`,
         `4` = `4`/`0`,
         `5` = `5`/`0`,
         `6` = `6`/`0`) %>% na.omit() %>% select(-`0`) %>% 
  gather(Sim, norm_sum, `1`:`6`)

# Boxplots of relative change: Median concentrations
ggplot(data=subset(annual_med_norm, variable=='TN_mgL' & depth=='0'), 
       aes(x= Sim, y = norm_median, fill=Sim, group = Sim)) +
  geom_boxplot() + mytheme + 
  geom_jitter(width=.15) +
  facet_grid(.~ Lake, scales="free_x") +
  scale_fill_manual("Temp.\nIncrease",values=c("#1E90FF", "#00FFFF","#FFFF00","#EE9A00", "#FF0000", "#8B0000")) +
  scale_y_continuous(limits=c(.5, 1.05), breaks=seq(.5,1.05,.05)) +
  geom_hline(yintercept=1, lty=2) +
  labs(y=(expression(Relative~change:~TN~(mu*g~L^-1))), 
       x = (expression(Temperature~increase~from~baseline~(degree*C)))) 

ggplot(data=subset(annual_med_norm, variable=='TN_mgL' & depth=='0'), 
       aes(x= Sim, y = norm_median, fill=as.factor(year), group = Sim)) +
  geom_boxplot() + mytheme + 
  geom_jitter(width=.15, pch=21, cex=3) +
  facet_grid(.~ Lake, scales="free_x") +
  # scale_fill_manual("Temp.\nIncrease",values=c("dodgerblue1", "deepskyblue","cyan","yellow", "orange2", "red1", "red4")) +
  #scale_y_continuous(limits=c(.75, 1.75), breaks=seq(0.75,1.75,.25)) +
  geom_hline(yintercept=1, lty=2) +
  labs(y=(expression(Relative~change:~median~epi~TN~(mu*g~L^-1))), 
       x = (expression(Temperature~increase~from~baseline~(degree*C)))) 

ggplot(data=subset(annual_med_norm, variable=='TP_mgL' & depth=='0'), 
       aes(x= Sim, y = norm_median, fill=Sim, group = Sim)) +
  geom_boxplot() + mytheme + 
  geom_jitter(width=.15) +
  facet_grid(.~ Lake, scales="free_x") +
  scale_fill_manual("Temp.\nIncrease",values=c("#1E90FF", "#00FFFF","#FFFF00","#EE9A00", "#FF0000", "#8B0000")) +
  scale_y_continuous(limits=c(.8, 1.4), breaks=seq(0.8,1.4,.05)) +
  geom_hline(yintercept=1, lty=2) +
  labs(y=(expression(Relative~change:~TP~(mu*g~L^-1))), 
       x = (expression(Temperature~increase~from~baseline~(degree*C)))) 

ggplot(data=subset(annual_med_norm, variable=='TP_mgL' & depth=='0'), 
       aes(x= Sim, y = norm_median, fill=as.factor(year), group = Sim)) +
  geom_boxplot() + mytheme + 
  geom_jitter(width=.15, pch=21, cex=3) +
  facet_grid(.~ Lake, scales="free_x") +
  # scale_fill_manual("Temp.\nIncrease",values=c("dodgerblue1", "deepskyblue","cyan","yellow", "orange2", "red1", "red4")) +
  #scale_y_continuous(limits=c(.75, 1.75), breaks=seq(0.75,1.75,.25)) +
  geom_hline(yintercept=1, lty=2) +
  labs(y=(expression(Relative~change:~median~epi~TP~(mu*g~L^-1))), 
       x = (expression(Temperature~increase~from~baseline~(degree*C)))) 

# Nutrient diagnostic stacked bar charts for N, P constituents ####
# Total epi P
tp_fractions <- c('TP_mgL', 'FRP_mgL','FRP_ads_mgL','DOP_mgL','POP_mgL')

tp_parts <- activeseason_summary %>% filter(variable %in% tp_fractions, 
                                      Sim %in% c('0','6'))

ggplot(subset(tp_parts, Lake == "Mendota" & variable !='TP_mgL'), 
       aes(x = year , y = median*1000, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(tp_parts, Lake=="Mendota" & variable=='TP_mgL'),
             aes(x = year, y = median*1000), cex=4, pch=18, show.legend=F) +
  facet_wrap(depth ~Sim, ncol=2) + 
  scale_x_continuous(breaks=seq(2004,2014,4)) +
  #scale_y_continuous(limits=c(0,60)) +
  labs(title= "Mendota TP", x= "Water year", 
       y= "median P (ug/L)")

ggplot(subset(tp_parts, Lake == "Sunapee" & variable !='TP_mgL'), aes(x = year , y = median*1000, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(tp_parts, Lake=="Sunapee" & variable=="TP_mgL"), 
             aes(x = year, y = median*1000), cex=4, pch=18, show.legend=F) +
  facet_wrap(depth~Sim, ncol=2) + 
  scale_x_continuous(breaks=seq(2004,2014,4)) +
  #scale_y_continuous(limits=c(0,15)) +
  labs(title= "Sunapee TP", x= "Water year", 
       y= "median P (ug/L)")

tp_parts2 <- overall_summary %>% filter(variable %in% tp_fractions)

ggplot(subset(tp_parts2, variable !='TP_mgL'), 
       aes(x = Sim , y = median_ugL, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(tp_parts2, variable=='TP_mgL'),
             aes(x = Sim, y = median_ugL), cex=4, pch=18, show.legend=F) +
  facet_wrap(Lake~depth, ncol=2, scales='free_y') + 
  labs(title= "Mendota TP", x= "Water year", y= "median P (ug/L)")

tn_fractions <- c('TN_mgL','AMM_mgL', 'NIT_mgL', 'DON_mgL', 'PON_mgL')
tn_parts <- annual_summary %>% filter(variable %in% tn_fractions,
                                      Sim %in% c('0','6'))

ggplot(subset(tn_parts, Lake == "Mendota" & variable !='TN_mgL'), aes(x = WaterYear , y = median*1000, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(tn_parts, Lake=="Mendota" & variable=='TN_mgL'),
             aes(x = WaterYear, y = median*1000), cex=4, pch=18, show.legend=F) +
  facet_wrap(depth ~ Sim, ncol=2) + 
  scale_x_continuous(breaks=seq(2004,2014,4)) +
  #scale_y_continuous(limits=c(0,60)) +
  labs(title= "Mendota TN", x= "Water year", 
       y= "median N (ug/L)")

ggplot(subset(tn_parts, Lake == "Sunapee" & variable !='TN_mgL'), aes(x = WaterYear , y = median*1000, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(tn_parts, Lake=="Sunapee" & variable=="TN_mgL"), 
             aes(x = WaterYear, y = median*1000), cex=4, pch=18, show.legend=F) +
  facet_wrap(depth~Sim, ncol=2) + 
  scale_x_continuous(breaks=seq(2004,2014,4)) +
  #scale_y_continuous(limits=c(0,15)) +
  labs(title= "Sunapee TN", x= "Water year", 
       y= "median N (ug/L)")

tn_parts2 <- overall_summary %>% filter(variable %in% tn_fractions)

ggplot(subset(tn_parts2, variable !='TN_mgL'), 
       aes(x = Sim , y = median_ugL, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(tn_parts2, variable=='TN_mgL'),
             aes(x = Sim, y = median_ugL), cex=4, pch=18, show.legend=F) +
  facet_wrap(Lake~depth, ncol=2) + 
  labs(title= "Mendota TN", x= "Water year", y= "median N (ug/L)")

# Grand median (across all dates) nutrients ####
NutMedian <- lakeYears %>% 
  select(Lake, Sim, depth, TP_mgL:PON_mgL, TNTP) %>%
  gather(variable, value, TP_mgL:TNTP) %>%
  group_by(Lake, Sim, depth, variable) %>% 
  summarise_all(median, na.rm=T) 

NutPercentChange <- NutMedian %>% 
  filter(Sim %in% c('0','6'), variable %in% c('TN_mgL','TP_mgL', 'TNTP')) %>%
  spread(Sim, value) %>%
  mutate(MagChange = round((`6`-`0`),2),
         PerChange = round(((`6`-`0`)/`0`)*100,2))

medianTP <- NutMedian %>% filter(variable %in% tp_fractions)

ggplot(subset(medianTP, Lake=="Mendota" & variable!='TP_mgL'), aes(x = Sim , y = value*1000, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(medianTP, Lake == "Mendota" & variable =='TP_mgL'),
             aes(x = Sim, y = value*1000), cex=4, pch=18, show.legend=F) +
  facet_grid(.~depth) +
  labs(title= "Mendota TP", x= "Air temp. increase (C)", 
       y= "Median epi P (ug/L)")

ggplot(subset(medianTP, Lake=="Sunapee" & variable!='TP_mgL'), aes(x = Sim , y = value*1000, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(medianTP, Lake == "Sunapee" & variable =='TP_mgL'),
             aes(x = Sim, y = value*1000), cex=4, pch=18, show.legend=F) +
  facet_grid(.~depth) +
  # scale_y_continuous(limits=c(0,15)) +
  labs(title= "Sunapee TP", x= "Air temp. increase (C)", 
       y= "Median epi P (ug/L)")

# TN 
medianTN <- NutMedian %>% filter(variable %in% tn_fractions)

ggplot(subset(medianTN, Lake=="Mendota" & variable!='TN_mgL'), aes(x = Sim , y = value*1000, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(medianTN, Lake == "Mendota" & variable =='TN_mgL'),
             aes(x = Sim, y = value*1000), cex=4, pch=18, show.legend=F) +
  facet_grid(.~depth) +
  labs(title= "Mendota TN", x= "Air temp. increase (C)", 
       y= "Median epi N (ug/L)")

ggplot(subset(medianTN, Lake=="Sunapee" & variable!='TN_mgL'), aes(x = Sim , y = value*1000, fill=variable)) +
  geom_bar(stat = "identity", col = "black") +
  geom_point(data= subset(medianTN, Lake == "Sunapee" & variable =='TN_mgL'),
             aes(x = Sim, y = value*1000), cex=4, pch=18, show.legend=F) +
  facet_grid(.~depth) +
  #scale_y_continuous(limits=c(0,15)) +
  labs(title= "Sunapee TN", x= "Air temp. increase (C)", 
       y= "Median epi N (ug/L)")
