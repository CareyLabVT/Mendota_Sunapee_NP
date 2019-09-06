#### Plot N, P mass balances for each lake ####
pacman::p_load(cowplot, tidyverse, lubridate)
options(scipen = 999) # disable scientific notation

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.line.x = element_line(colour = "black"), 
                 axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=16, colour='black'), 
                 axis.text.y=element_text(size=16, colour='black'), 
                 axis.title.x=element_text(size=16), axis.title.y=element_text(size=16),
                 legend.text = element_text(size=16), legend.title=element_text(size=16),
                 strip.text.x = element_text(margin = margin(.25,0,.25,0, "cm"), size=16))

diagnostic_depths <- c(1, 20)

lakes <- bind_rows(read_csv('./output/Mendota_11AprAll_20190416.csv'), 
                   read_csv('./output/Sunapee_20AugAll_20190820.csv')) %>%
  filter(Sim %in% c('0','6'), depth %in% diagnostic_depths) %>%
  mutate(Year = year(DateTime), yday = yday(DateTime))

#### Baseline inflows and outflows ####
mendota_0 <- bind_rows((read_csv('./Mendota/GLM/inflow_Balance.csv') %>%
                          mutate(Source = "Balance")),
                       read_csv('./Mendota/GLM/inflow_Pheasant.csv') %>%
                          mutate(Source = "Pheasant")) %>%
  bind_rows(., (read_csv('./Mendota/GLM/inflow_YaharaHighway.csv') %>%
                          mutate(Source = "Yahara"))) %>% mutate(Lake = "Mendota") 

sunapee_0 <- read_csv('./Sunapee/GLM/oneInflow.csv') %>%
                          mutate(Source = "Balance", Lake = "Sunapee",
                                 time = as.Date(time)) %>%
  filter(time <= ymd('2014-12-31'))

inflow_0 <- bind_rows(mendota_0, sunapee_0) %>%
  #select(-SALT, -TEMP, -OGM_doc, - OGM_poc) %>%
  mutate(NIT_mg = (NIT_nit * 14/1000) * (FLOW * 1000), # transform to daily loads; mg
         AMM_mg = (NIT_amm * 14/1000) * (FLOW * 1000), # Conc from mmol/m3 to mg/L
         DON_mg = (OGM_don * 14/1000) * (FLOW * 1000), # FLOW from m3 to L
         PON_mg = (OGM_pon * 14/1000) * (FLOW * 1000),
         TN_mg = NIT_mg + AMM_mg + DON_mg + PON_mg,
         
         FRP_mg = (PHS_frp * 30.97/1000) * (FLOW * 1000),
         FRP_ads_mg = (PHS_frp_ads * 30.97/1000) * (FLOW * 1000),
         DOP_mg = (OGM_dop * 30.97/1000) * (FLOW * 1000),
         POP_mg = (OGM_pop * 30.97/1000) * (FLOW * 1000),
         TP_mg = FRP_mg + FRP_ads_mg + DOP_mg + POP_mg) %>%
  select(- (FLOW:PHS_frp_ads) ) %>% 
  gather(Variable, Value, NIT_mg:TP_mg) %>%
  spread(Source, Value) %>%
  mutate(Total_In = ifelse(Lake == "Mendota", round(Balance + Pheasant + Yahara, 2),
                           round(Balance, 2)), Sim = "0", DateTime = time) %>%
  select(DateTime, Sim, Lake, Variable, Total_In) %>%
  arrange(Lake, Sim, DateTime, Variable)

# Bind same inflows for +7 scenario (inflows don't change)
inflow <- bind_rows(inflow_0, (inflow_0 %>% mutate(Sim = "6")))

outflow_vol <- bind_rows((read_csv('./Mendota/GLM/outflow.csv') %>% 
                                 mutate(Lake = "Mendota")),
                              (read_csv('./Sunapee/GLM/outflow.csv') %>%
                                 mutate(Lake = "Sunapee", time = as.Date(time)))) %>%
  rename(DateTime = "time") %>% select(DateTime, Lake, FLOW) %>% filter(DateTime <= ymd('2014-12-31'))

outflow <- lakes %>% filter(depth == '1') %>% 
  select(DateTime, TP_mgL:Lake) %>% mutate(DateTime = as.Date(DateTime)) %>%
  full_join(., outflow_vol) %>%
  mutate(NIT_mg = NIT_mgL * (FLOW * 1000), # transform to daily loads; mg
         AMM_mg = AMM_mgL * (FLOW * 1000), # FLOW from m3 to L
         DON_mg = DON_mgL * (FLOW * 1000),
         PON_mg = PON_mgL * (FLOW * 1000),
         TN_mg = NIT_mg + AMM_mg + DON_mg + PON_mg,
         
         FRP_mg = FRP_mgL * (FLOW * 1000),
         FRP_ads_mg = FRP_ads_mgL * (FLOW * 1000),
         DOP_mg = DOP_mgL * (FLOW * 1000),
         POP_mg = POP_mgL * (FLOW * 1000),
         TP_mg = FRP_mg + FRP_ads_mg + DOP_mg + POP_mg) %>%
  select(- (TP_mgL:PON_mgL), -FLOW) %>%
  gather(Variable, Total_Out, NIT_mg:TP_mg) %>% mutate(Sim = as.character(Sim),
                                                       Total_Out = round(Total_Out, 2)) %>%
  arrange(Lake, Sim, DateTime, Variable)

in_out <- full_join(inflow, outflow) %>% 
  filter(DateTime > '2003-11-08')

in_outLong <- in_out %>% 
  gather(Direction, Value, Total_In:Total_Out) %>% 
  filter(DateTime > ymd('2003-11-08'), DateTime <= ymd('2015-01-01'), !is.na(Value), 
         Sim %in% c('0','6'))

in_out_difference <- in_out %>% mutate(storage = Total_In - Total_Out) 

# Plot inflow vs. outflow for N ####
Pvars <- c("FRP_mg","FRP_ads_mg","DOP_mg","POP_mg")
Nvars <- c("AMM_mg","NIT_mg","DON_mg","PON_mg")

# Mendota TN, TP
ggplot(subset(in_outLong, Variable %in% c('TN_mg','TP_mg') & Lake == "Mendota"), 
       aes(x = DateTime, y = (Value/1000), col = Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ Direction, scales= 'free_y') +
  ggtitle("Mendota TN, TP")

# Sunapee TN, TP
ggplot(subset(in_outLong, Variable %in% c('TN_mg','TP_mg') & Lake == "Sunapee"), 
       aes(x = DateTime, y = (Value/1000), col = Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ Direction, scales= 'free_y') +
  ggtitle("Sunapee TN, TP")

# Mendota Nitrogen Baseline In vs. Out
ggplot(subset(in_outLong, Variable %in% Nvars & Lake == "Mendota" & Sim == '0'), 
       aes(x = DateTime, y = (Value/1000), col = Direction)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  ggtitle("Mendota Nitrogen Baseline")

# Mendota Nitrogen Baseline Storage
ggplot(subset(in_out_difference, Variable %in% Nvars & Lake == "Mendota" & Sim == '0'), 
       aes(x = DateTime, y = (storage/1000))) + 
  geom_line() + mytheme + 
  scale_y_continuous("In minus out, grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  geom_hline(yintercept = 0, col='red') +
  ggtitle("Mendota Nitrogen Baseline Storage")

# Mendota Nitrogen Outflow Between Sims
ggplot(subset(in_outLong, Variable %in% Nvars & Lake == "Mendota" & Direction == "Total_Out"), 
       aes(x = DateTime, y = (Value/1000), col = Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y')+
  ggtitle("Mendota Nitrogen Outflow")

# Mendota Nitrogen Storage +0 vs +7
ggplot(subset(in_out_difference, Variable %in% Nvars & Lake == "Mendota"), 
       aes(x = DateTime, y = (storage/1000), col=Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("In minus out, grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  geom_hline(yintercept = 0, col='red') +
  ggtitle("Mendota Nitrogen Storage +0 vs +7")

# Sunapee Nitrogen Baseline In vs. Out
ggplot(subset(in_outLong, Variable %in% Nvars & Lake == "Sunapee" & Sim == '0'), 
       aes(x = DateTime, y = (Value/1000), col = Direction)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ Sim, scales= 'free_y')+
  ggtitle("Sunapee Nitrogen Baseline")

# Sunapee Nitrogen Baseline Storage
  ggplot(subset(in_out_difference, Variable %in% Nvars & Lake == "Sunapee" & Sim == '0'), 
       aes(x = DateTime, y = (storage/1000))) + 
  geom_line() + mytheme + 
  scale_y_continuous("In minus out, grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  geom_hline(yintercept = 0, col='red') +
  ggtitle("Sunapee Nitrogen Baseline Storage")

# Sunapee Nitrogen Outflow Between Sims
ggplot(subset(in_outLong, Variable %in% Nvars & Lake == "Sunapee" & Direction == "Total_Out"), 
       aes(x = DateTime, y = (Value/1000), col = Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y')+
  ggtitle("Sunapee Nitrogen Outflow")

# Sunapee Nitrogen Storage +0 vs +7
ggplot(subset(in_out_difference, Variable %in% Nvars & Lake == "Sunapee"), 
       aes(x = DateTime, y = (storage/1000), col=Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("In minus out, grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  geom_hline(yintercept = 0, col='red') +
  ggtitle("Sunapee Nitrogen Storage +0 vs +7")

# Plot inflow vs. outflow for P ####
# Mendota Phosphorus Baseline In vs. Out
ggplot(subset(in_outLong, Variable %in% Pvars & Lake == "Mendota" & Sim == '0'), 
       aes(x = DateTime, y = (Value/1000), col = Direction)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  ggtitle("Mendota Phosphorus Baseline")

# Mendota Phosphorus Baseline Storage
ggplot(subset(in_out_difference, Variable %in% Pvars & Lake == "Mendota" & Sim == '0'), 
       aes(x = DateTime, y = (storage/1000))) + 
  geom_line() + mytheme + 
  scale_y_continuous("In minus out, grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  geom_hline(yintercept = 0, col='red') +
  ggtitle("Mendota Phosphorus Baseline Storage")

# Mendota Phosphorus Outflow Between Sims
ggplot(subset(in_outLong, Variable %in% Pvars & Lake == "Mendota" & Direction == "Total_Out"), 
       aes(x = DateTime, y = (Value/1000), col = Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y')+
  ggtitle("Mendota Phosphorus Outflow")

# Mendota Phosphorus Storage +0 vs +6
ggplot(subset(in_out_difference, Variable %in% Pvars & Lake == "Mendota"), 
       aes(x = DateTime, y = (storage/1000), col=Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("In minus out, grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  geom_hline(yintercept = 0, col='red') +
  ggtitle("Mendota Phosphorus Storage +0 vs +6")

# Sunapee Phosphorus Baseline In vs. Out
ggplot(subset(in_outLong, Variable %in% Pvars & Lake == "Sunapee" & Sim == '0'), 
       aes(x = DateTime, y = (Value/1000), col = Direction)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y')+
  ggtitle("Sunapee Phosphorus Baseline")

# Sunapee Phosphorus Baseline Storage
ggplot(subset(in_out_difference, Variable %in% Pvars & Lake == "Sunapee" & Sim == '0'), 
       aes(x = DateTime, y = (storage/1000))) + 
  geom_line() + mytheme + 
  scale_y_continuous("In minus out, grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  geom_hline(yintercept = 0, col='red') +
  ggtitle("Sunapee Phosphorus Baseline Storage")

# Sunapee Phosphorus Outflow Between Sims
ggplot(subset(in_outLong, Variable %in% Pvars & Lake == "Sunapee" & Direction == "Total_Out"), 
       aes(x = DateTime, y = (Value/1000), col = Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y')+
  ggtitle("Sunapee Phosphorus Outflow")

# Sunapee Phosphorus Storage +0 vs +7
ggplot(subset(in_out_difference, Variable %in% Pvars & Lake == "Sunapee"), 
       aes(x = DateTime, y = (storage/1000), col=Sim)) + 
  geom_line() + mytheme + 
  scale_y_continuous("In minus out, grams per day") + 
  facet_grid(Variable ~ ., scales= 'free_y') +
  geom_hline(yintercept = 0, col='red') +
  ggtitle("Sunapee Phosphorus Storage +0 vs +7")

#### SUNAPEE inflow concentrations vs. baseline surface ####
pfact <- 30.97/1000
nfact <- 14/1000

inflow <- read_csv('./Sunapee/GLM/oneInflow.csv') %>% 
  mutate(DateTime = time, depth = 0) %>% 
  select(DateTime, depth, OGM_don, NIT_nit, NIT_amm, OGM_pon, PHS_frp, OGM_dop, OGM_pop, PHS_frp_ads) %>% 
  mutate(FRP_mgL = PHS_frp*pfact, FRP_ads_mgL = PHS_frp_ads*pfact, DOP_mgL = OGM_dop*pfact,
         POP_mgL = OGM_pop*pfact, TP_mgL = FRP_mgL+FRP_ads_mgL+DOP_mgL+POP_mgL,
         AMM_mgL = NIT_amm*nfact, NIT_mgL = NIT_nit*nfact, DON_mgL = 0, PON_mgL = OGM_pon*nfact,
         TN_mgL = AMM_mgL+NIT_mgL+DON_mgL+PON_mgL) %>% 
  group_by(DateTime, depth) %>% 
  select(DateTime, depth, FRP_mgL:TN_mgL) %>% 
  gather(var, value, FRP_mgL:TN_mgL) %>% ungroup() %>% 
  mutate(type = 'inflow', DateTime = as.Date(DateTime)) 

surf <- read_csv('./output/Sunapee_20Augall_20190820.csv') %>% 
  filter(depth == '0', Sim == '0') %>% 
  select(DateTime, depth, TP_mgL:PON_mgL) %>% 
  group_by(DateTime, depth) %>%
  gather(var, value, TP_mgL:PON_mgL) %>% ungroup() %>% 
  mutate(type = 'baseline', DateTime = as.Date(DateTime))

compare <- bind_rows(inflow, surf) %>% ungroup() %>% select(-depth) %>% 
  mutate(type = factor(type, levels=c('inflow','baseline')))

ncomp <- compare %>% filter(var %in% c('TN_mgL', 'AMM_mgL', 'NIT_mgL', 'DON_mgL', 'PON_mgL'))
ggplot(ncomp, aes(x = DateTime, y = value, lwd=type, col=type)) + 
  geom_path() + 
  scale_size_manual(values=c(0.5,1.5)) + mytheme+
  facet_wrap(. ~ var, scales= 'free_y') +
  scale_y_continuous('Concentration (mg/L)')

pcomp <- compare %>% filter(var %in% c('TP_mgL','FRP_mgL','FRP_ads_mgL','DOP_mgL','POP_mgL'))
ggplot(pcomp, aes(x = DateTime, y = value, lwd=type, col=type)) + 
  geom_path() + 
  scale_size_manual(values=c(0.5,1.5)) + mytheme+
  facet_wrap(. ~ var, scales= 'free_y') +
  scale_y_continuous('Concentration (mg/L)')

# inflow (x) vs. baseline (y)
wide <- compare %>% group_by(DateTime, var) %>% 
  spread(type, value) 

# all N's
nwide <- wide %>% filter(var %in% c('TN_mgL', 'AMM_mgL', 'NIT_mgL', 'DON_mgL', 'PON_mgL'))

ggplot(nwide, aes(x = inflow, y = baseline, col=var)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty=2, col='red') +
  scale_x_continuous('Inflow (mgL)', limits=c(0, 3)) + 
  scale_y_continuous('Baseline 0m (mgL)',limits=c(0, 3))+
  ggtitle('Sunapee TN: inflow vs. baseline') + mytheme+
  theme(legend.position = c(0.01,1), legend.justification = c(0,1))

# Amm
ggplot(subset(wide, var == 'AMM_mgL'), aes(x = inflow, y = baseline)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty=2, col='red') +
  scale_x_continuous(limits=c(0, .5)) + 
  scale_y_continuous(limits=c(0, .5)) 

# Nit
ggplot(subset(wide, var == 'NIT_mgL'), aes(x = inflow, y = baseline)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty=2, col='red') +
  scale_x_continuous(limits=c(0, .5)) + 
  scale_y_continuous(limits=c(0, .5)) 

# PON
ggplot(subset(wide, var == 'PON_mgL'), aes(x = inflow, y = baseline)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty=2, col='red') +
  scale_x_continuous(limits=c(0, 2)) + 
  scale_y_continuous(limits=c(0, 2)) 

# All P's 
pwide <- wide %>% filter(var %in% c('TP_mgL','FRP_mgL','FRP_ads_mgL','DOP_mgL','POP_mgL'))

ggplot(pwide, aes(x = inflow, y = baseline, col=var)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty=2, col='red') +
  scale_x_continuous('Inflow (mgL)', limits=c(0, .04)) + 
  scale_y_continuous('Baseline 0m (mgL)',limits=c(0, .04))+
  ggtitle('Sunapee TP: inflow vs. baseline') + mytheme+
  theme(legend.position = c(0.01,1), legend.justification = c(0,1))

# FRP's
ggplot(subset(wide, var == 'FRP_mgL' | var == 'FRP_ads_mgL'), aes(x = inflow, y = baseline, col=var)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty=2, col='red') +
  scale_x_continuous(limits=c(0, .0175)) + 
  scale_y_continuous(limits=c(0, .0175)) 

# DOP / POP
ggplot(subset(wide, var == 'DOP_mgL' | var == 'POP_mgL'), aes(x = inflow, y = baseline, col=var)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty=2, col='red') +
  scale_x_continuous(limits=c(0, .015)) + 
  scale_y_continuous(limits=c(0, .015)) 