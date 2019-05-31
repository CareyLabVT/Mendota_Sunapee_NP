# Calculate TN, TP export from 0-4 m each lake ####
pacman::p_load(cowplot, ggridges, lubridate, tidyverse)

#options(scipen = 999) # disable scientific notation

jet.colors <- colorRampPalette(c("dodgerblue4",  "dodgerblue1", "cyan", 
                                 "yellow", "orange2", "red1", "red4")) 

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), 
                 axis.line.x = element_line(colour = 'black'), axis.line.y = element_line(colour = 'black'), 
                 axis.text.x=element_text(size=16, colour='black'), axis.text.y=element_text(size=16, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 legend.text = element_text(size=18), legend.title=element_text(size=18),
                 strip.text.x = element_text(margin = margin(.25,0,.25,0, 'cm'), size=18),
                 strip.text.y = element_text(margin = margin(0,.25,0,.25, 'cm'), size=18))

# Load output from GRAPLEr sims, both lakes #### 
lakes <- bind_rows(read_csv('./output/Mendota_11AprAll_20190419.csv'), 
                   read_csv('./output/Sunapee_11AprAll_20190416.csv')) %>%
  select(Lake, Sim, DateTime, depth, TN_mgL, TP_mgL) %>% 
  filter(depth == 4) %>% 
  mutate(Sim = as.factor(Sim), Date = as.Date(DateTime),
         year = year(DateTime), month = month(DateTime))

# Load inflow data, transform nutrient inflows from mmol/m3 to mg ####
mendota_in <- bind_rows((read_csv('./Mendota/GLM/inflow_Balance.csv') %>%
                          mutate(Source = "Balance")),
                       read_csv('./Mendota/GLM/inflow_Pheasant.csv') %>%
                         mutate(Source = "Pheasant")) %>%
  bind_rows(., (read_csv('./Mendota/GLM/inflow_YaharaHighway.csv') %>%
                  mutate(Source = "Yahara"))) %>% mutate(Lake = "Mendota")

sunapee_in <- read_csv('./Sunapee/GLM/oneInflow.csv') %>%
  mutate(Source = "Balance", Lake = "Sunapee",
         time = as.Date(time)) %>%
  filter(time <= ymd('2014-12-31'))

inflow <- bind_rows(mendota_in, sunapee_in) %>% 
  rename(flow_cms = FLOW, Date = time) %>% 
  mutate(flow_Ls = flow_cms*1000,      # convert outflow to L/s
         flow_Ld = flow_Ls*86400) %>%  # convert outflow to L/d
  mutate(NIT_mg = (NIT_nit * 14/1000) * flow_Ld, # transform to daily loads; mg
         AMM_mg = (NIT_amm * 14/1000) * flow_Ld, # Conc from mmol/m3 to mg/L
         DON_mg = (OGM_don * 14/1000) * flow_Ld, # times flow in L/d
         PON_mg = (OGM_pon * 14/1000) * flow_Ld,
         TN_mg = NIT_mg + AMM_mg + DON_mg + PON_mg,
         
         FRP_mg = (PHS_frp * 30.97/1000) * flow_Ld,
         FRP_ads_mg = (PHS_frp_ads * 30.97/1000) * flow_Ld,
         DOP_mg = (OGM_dop * 30.97/1000) * flow_Ld,
         POP_mg = (OGM_pop * 30.97/1000) * flow_Ld,
         TP_mg = FRP_mg + FRP_ads_mg + DOP_mg + POP_mg) %>%
  select(Lake, Source, Date, flow_Ld, TN_mg, TP_mg) %>% 
  gather(Variable, Value, flow_Ld:TP_mg) %>%
  spread(Source, Value) %>%
  mutate(DailyIn_mg = ifelse(Lake == "Mendota", round(Balance + Pheasant + Yahara, 2),
                           round(Balance, 2))) %>%
  select(-(Balance:Yahara)) %>% 
  spread(Variable, DailyIn_mg) %>% 
  mutate(Inflow_Ld = flow_Ld, In_DailyTN_kgd = TN_mg*1e-6, In_DailyTP_kgd = TP_mg*1e-6) %>% 
  select(Date, Lake, Inflow_Ld, In_DailyTN_kgd, In_DailyTP_kgd) 

# Plot timeseries of inflow volume, TN, TP loads
ggplot(inflow, aes(x = Date, y = Inflow_Ld, col=Lake)) + 
  geom_point() +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  facet_wrap(Lake~., scales='free_y')

ggplot(inflow, aes(x = Date, y = In_DailyTN_kgd, col=Lake)) + 
  geom_point() +
  scale_y_continuous()+
  facet_wrap(Lake~., scales='free_y', ncol=1)

ggplot(inflow, aes(x = Date, y = In_DailyTP_kgd, col=Lake)) + 
  geom_point() +
  scale_y_continuous()+
  facet_wrap(Lake~., scales='free_y', ncol=1)

# Plot summed inflow per year ####
summer_flow <- inflow %>% 
  mutate(month=month(Date), year=year(Date)) %>% 
  filter(month >=4, month<=10) %>% 
  group_by(Lake, year) %>% 
  summarize(Inflow = sum(Inflow_Ld))

annual_flow <- inflow %>% 
  mutate(month=month(Date), year=year(Date)) %>% 
  group_by(Lake, year) %>% 
  summarize(Inflow = sum(Inflow_Ld))

ggplot(summer_flow, aes(x = year, y = Inflow, col=Lake)) +
  geom_path(cex=2)+ geom_point(cex=4)+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_x_continuous(limits=c(2004, 2014),breaks=seq(2004,2014,1))

# Calculate mean daily concentration for TN, TP between 0-4m ####     
summer_daily_conc <- lakes %>% 
  filter(month >=4, month<=10) %>%  # Select 1 Apr - 31Oct
  group_by(Lake, Sim, Date, year) %>% 
  summarize(TN_mgL = mean(TN_mgL), TP_mgL = mean(TP_mgL)) # daily mean, 0-4m

# Load outflow data, transform from m3/s to L/d 
outflow <- bind_rows((read_csv('./Mendota/GLM/outflow.csv') %>% 
                        mutate(Lake = 'Mendota')),
                     (read_csv('./Sunapee/GLM/outflow.csv') %>% 
                        mutate(Lake = 'Sunapee', time = as.Date(time)))) %>% 
  rename(flow_cms = FLOW, Date = time) %>% 
  mutate(flow_Ls = flow_cms*1000,      # convert outflow to L/s
         flow_Ld = flow_Ls*86400) %>%  # convert outflow to L/d
  select(Date, Lake, flow_Ld)

# Multiply mean daily concentrations by outflow volume
summer_daily_outflow <- left_join(summer_daily_conc, outflow) %>% 
  mutate(Out_DailyTN_kgd = TN_mgL*flow_Ld*1e-6, 
         Out_DailyTP_kgd = TP_mgL*flow_Ld*1e-6) %>% 
  select(Date, Lake, Sim, Out_DailyTN_kgd, Out_DailyTP_kgd)

# Bind inflow, outflow nutrients, summarize TN, TP in and outputs by summer ####
inout <- left_join(summer_daily_outflow, inflow) %>% 
  select(-Inflow_Ld) %>% 
  gather(variable, value, Out_DailyTN_kgd:In_DailyTP_kgd) %>% 
  mutate(year = year(Date)) %>% 
  group_by(Lake, Sim, year, variable) %>% 
  summarize(sum_kg = sum(value)) %>% 
  separate(variable, c('Source','Nut'), sep="_") %>% 
  spread(Source, sum_kg) %>% 
  mutate(Flux = round(((Out-In)/In)*100,1), 
         Nut = factor(Nut, levels=c('DailyTN','DailyTP'),
                      labels=c('Total Nitrogen','Total Phosphorus')))

# Boxplots of flux ####
ggplot(inout, aes(x= Sim, y = Flux, fill=Sim, group = Sim)) +
  geom_point(position=position_dodge(width = .75), cex= 4, pch=21) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) + mytheme +
  facet_grid(Nut~Lake, scales='free') +
  #scale_y_continuous(limits=c(-100,25), breaks=seq(-100,25,25)) +
  scale_fill_manual(values=jet.colors(7)) +
  geom_hline(data=subset(inout, Nut=='Total Phosphorus'), aes(yintercept= 0), lty=2) +
  labs(x = expression(Warming~scenario~(degree*C)), y= "Summer net nutrient flux (%)") +
  theme(panel.spacing.y = unit(2, "lines"),
        panel.background=element_rect(fill='gray95'),
        legend.position='none')

# Calculate changes in medians ####
flux_year_by_year <- inout %>%
  select(Lake:Nut, Flux) %>% 
  spread(Sim, Flux) %>% 
  mutate('0_v_6' = round(((`6` - `0`)/abs(`0`)*100), 1))

median_of_extremes <- inout %>%
  select(Lake:Nut, Flux) %>% filter(Sim %in% c('0','6')) %>% 
  group_by(Lake, Sim, Nut) %>% 
  summarize(median = median(Flux))%>% 
  spread(Sim, median) %>% 
  mutate('0_v_6' = round(((`6` - `0`)/abs(`0`)*100), 1))

# Diagnostic boxplots (colored by year, not sim) ####
ggplot(subset(inout, Lake=='Mendota'), aes(x= Sim, y = Flux, fill=as.factor(year), group = Sim)) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) + mytheme +
  geom_point(position=position_dodge(width = .75), cex= 2, pch=21) +
  facet_wrap(.~Nut, scales='free_y') +
  geom_text(aes(label=as.factor(year)),hjust=1, vjust=0, cex=3, col='black') +
  geom_hline(data=subset(inout, Nut=='Total Phosphorus'), aes(yintercept= 0), lty=2) +
  labs(y= "Summer flux (%)") + ggtitle("Mendota") 

ggplot(subset(inout, Lake=='Sunapee'), aes(x= Sim, y = Flux, fill=as.factor(year), group = Sim)) +
  geom_boxplot(alpha=0.75, outlier.shape=NA, show.legend = F) + mytheme +
  geom_point(position=position_dodge(width = .75), cex= 2, pch=21) +
  facet_wrap(.~Nut, scales='free_y') +
  geom_text(aes(label=as.factor(year)),hjust=1, vjust=0, cex=3, col='black') +
  #geom_hline(data=subset(inout, Nut=='Total Phosphorus'), aes(yintercept= 0), lty=2) +
  labs(y= "Summer flux (%)") + ggtitle("Sunapee") 
