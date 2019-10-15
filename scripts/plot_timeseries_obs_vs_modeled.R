# Time series plots of modeled vs. observed for Mendota, Sunapee
# Load packages and set plotting themes ####
pacman::p_load(cowplot, glmtools, lubridate, tidyverse)
options(scipen = 999) # disable scientific notation

jet.colors <- colorRampPalette(c('dodgerblue4',  'dodgerblue1', 'deepskyblue','cyan', 
                                 'yellow', 'orange2', 'red1', 'red4')) 

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), axis.line.x = element_line(colour = 'black'), 
                 axis.line.y = element_line(colour = 'black'), 
                 axis.text.x=element_text(size=16, colour='black'), 
                 axis.text.y=element_text(size=16, colour='black'), 
                 axis.title.x=element_text(size=18), axis.title.y=element_text(size=18),
                 legend.text = element_text(size=18), legend.title=element_text(size=18),
                 strip.text.x = element_text(margin = margin(.25,0,.25,0, 'cm'), size=18),
                 strip.text.y = element_text(margin = margin(0,.25,0,.25, 'cm'), size=18),
                 plot.margin = unit(c(0,.3,0,0), "cm"))

startDate = ymd('2004-04-01') # start date for model runs
endDate = ymd('2014-12-31') # end date for model runs

# Load model output for baseline scenario ####
men_folder <- './Mendota/GLM'

menbaseline <- file.path('./output/Mendota_11AprAll_20190419.csv') %>%
  read_csv(.) %>% filter(Sim == '0') %>% rename(Depth = depth) %>% 
  select(DateTime, Depth, Temp, DO_mgL, TP_mgL, TN_mgL) %>% 
  mutate(Lake = 'Mendota',
         zone = ifelse(Depth <=4, '0-4m', 
                              ifelse(Depth >= 16 & Depth <= 20, '16-20m', NA))) %>% 
  filter(!is.na(zone))

sun_folder <- './Sunapee/GLM'

sunbaseline <- file.path('./output/Sunapee_11AprAll_20190416.csv') %>%
  read_csv(.) %>% filter(Sim == '0') %>% rename(Depth = depth) %>% 
  select(DateTime, Depth, Temp, DO_mgL, TP_mgL, TN_mgL) %>% 
  mutate(Lake = 'Sunapee',
         zone = ifelse(Depth <=4, '0-4m', 
                       ifelse(Depth >= 16 & Depth <= 20, '16-20m', NA))) %>% 
  filter(!is.na(zone))

########## TEMPERATURE ##########################
buoyTemps <- bind_rows((file.path(men_folder, './observed_data/buoy_temps.csv') %>% 
                          read_csv(.) %>% 
                          mutate(Lake = 'Mendota')),
                       (file.path(sun_folder, './observed_data/buoy_temps.csv') %>% 
                          read_csv(.) %>% 
                          mutate(Lake = 'Sunapee'))) %>% 
  mutate(zone = ifelse(Depth <=4, '0-4m', NA)) %>% filter(!is.na(zone))

manTemps <- bind_rows((file.path(men_folder, './observed_data/manual_temps.csv') %>% 
                         read_csv(.) %>% 
                         mutate(Lake = 'Mendota')),
                      (file.path(sun_folder, './observed_data/manual_temps.csv') %>% 
                         read_csv(.) %>% 
                         mutate(Lake = 'Sunapee'))) %>% 
  mutate(zone = ifelse(Depth <=4, '0-4m', 
                       ifelse(Depth >= 16 & Depth <= 20, '16-20m', NA))) %>% 
  filter(!is.na(zone)) %>% group_by(DateTime, zone) %>% 
  mutate(meanTemp = mean(Temp))

modTemps <- bind_rows(menbaseline, sunbaseline) %>% 
  group_by(DateTime, Lake, zone) %>% 
  mutate(meanTemp = mean(Temp)) %>% ungroup() %>% 
  mutate(DateTime = as.Date(DateTime))

# Water temp plot
ggplot(modTemps, aes(x = DateTime, y = meanTemp)) + mytheme + 
  scale_y_continuous(limits=c(0,30)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y') +
  labs(x= '', y=expression(Water~temperature~(degree*C))) +
  geom_line(lwd=1, col='gray30') + 
  theme(legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())+
  facet_grid(Lake ~ zone, scales='free_y') +
  geom_point(data = subset(buoyTemps, zone=='0-4m' & Lake == 'Mendota'), 
             aes(x = DateTime, y = Temp), cex=1.75, pch=21, fill='dodgerblue') +
  geom_point(data = subset(manTemps, Lake == 'Mendota'),
             aes(x = DateTime, y = meanTemp), cex=1.75, pch=24, fill='tomato') +
  geom_point(data = subset(buoyTemps, zone=='0-4m' & Lake == 'Sunapee'), 
             aes(x = DateTime, y = Temp), cex=1.75, pch=21, fill='dodgerblue') +
  geom_point(data = subset(manTemps, Lake == 'Sunapee'),
             aes(x = DateTime, y = meanTemp), cex=1.75, pch=24, fill='tomato')

########## DISSOLVED OXYGEN ##########################
# Buoy
buoyDO <- bind_rows((file.path(men_folder, './observed_data/buoy_DO.csv') %>% 
  read_csv(.) %>% 
  mutate(Lake = 'Mendota')),
  (file.path(sun_folder, './observed_data/buoy_DO.csv') %>% 
     read_csv(.) %>% 
     mutate(Lake = 'Sunapee'))) %>% 
    mutate(zone = ifelse(Depth <=4, '0-4m', NA)) %>% filter(!is.na(zone))

manDO <- bind_rows((file.path(men_folder, './observed_data/manual_DO.csv') %>% 
  read_csv(.) %>% 
  mutate(Lake = 'Mendota')),
  (file.path(sun_folder, './observed_data/manual_DO.csv') %>% 
     read_csv(.) %>% 
     mutate(Lake = 'Sunapee'))) %>% 
  mutate(zone = ifelse(Depth <=4, '0-4m', 
                          ifelse(Depth >= 16 & Depth <= 20, '16-20m', NA))) %>% 
     filter(!is.na(zone)) %>% group_by(DateTime, zone) %>% 
     mutate(meanDO = mean(DO_mgL))

modDO <- bind_rows(menbaseline, sunbaseline) %>% 
  group_by(DateTime, Lake, zone) %>% 
  mutate(meanDO = mean(DO_mgL)) %>% ungroup() %>% 
  mutate(DateTime = as.Date(DateTime))

# DO plot
ggplot(modDO, aes(x = DateTime, y = DO_mgL)) + mytheme + 
  scale_y_continuous(limits=c(0,15)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(Dissolved~oxygen~(mg~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(Lake ~ zone) +
  geom_point(data = subset(buoyDO, zone=='0-4m' & Lake == 'Mendota'), 
             aes(x = DateTime, y = DO_mgL), cex=1.75, pch=21, fill='dodgerblue') +
  geom_point(data = subset(manDO, Lake == 'Mendota'),
             aes(x = DateTime, y = meanDO), cex=1.75, pch=24, fill='tomato') +
  geom_point(data = subset(buoyDO, zone=='0-4m' & Lake == 'Sunapee'), 
             aes(x = DateTime, y = DO_mgL), cex=1.75, pch=21, fill='dodgerblue') +
  geom_point(data = subset(manDO, Lake == 'Sunapee'),
             aes(x = DateTime, y = meanDO), cex=1.75, pch=24, fill='tomato')

########## TN ##########################
manTN <- bind_rows((file.path(men_folder, './observed_data/manual_TN.csv') %>% 
                      read_csv(.) %>% 
                      mutate(Lake = 'Mendota', DateTime= as.Date(DateTime))),
                   (file.path(sun_folder, './observed_data/manual_TN.csv') %>% 
                      read_csv(.) %>% 
                      mutate(Lake = 'Sunapee'))) %>% 
  mutate(zone = ifelse(Depth <=4, '0-4m', 
                       ifelse(Depth >= 16 & Depth <= 20, '16-20m', NA))) %>% 
  filter(!is.na(zone)) %>% group_by(DateTime, zone) %>% 
  mutate(meanTN = mean(TN_mgL))

modTN <- bind_rows(menbaseline, sunbaseline) %>% 
  group_by(DateTime, Lake, zone) %>% 
  mutate(meanTN = mean(TN_mgL)) %>% ungroup() %>% 
  mutate(DateTime = as.Date(DateTime))

# TN plot
ggplot(modTN, aes(x = DateTime, y = TN_mgL)) + mytheme + 
  #scale_y_continuous(limits=c(0,7)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(TN~(mg~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(Lake ~ zone, scales='free_y') +
  geom_point(data = subset(manTN, Lake == 'Mendota'),
             aes(x = DateTime, y = meanTN), cex=1.75, pch=24, fill='tomato') +
  geom_point(data = subset(manTN, Lake == 'Sunapee'),
             aes(x = DateTime, y = meanTN), cex=1.75, pch=24, fill='tomato') 

########## TP ##########################
manTP <- bind_rows((file.path(men_folder, './observed_data/manual_TP.csv') %>% 
                      read_csv(.) %>% 
                      mutate(Lake = 'Mendota', DateTime= as.Date(DateTime))),
                   (file.path(sun_folder, './observed_data/manual_TP.csv') %>% 
                      read_csv(.) %>% 
                      mutate(Lake = 'Sunapee'))) %>% 
  mutate(zone = ifelse(Depth <=4, '0-4m', 
                       ifelse(Depth >= 16 & Depth <= 20, '16-20m', NA))) %>% 
  filter(!is.na(zone)) %>% group_by(DateTime, zone) %>% 
  mutate(meanTP = mean(TP_mgL))

modTP <- bind_rows(menbaseline, sunbaseline) %>% 
  group_by(DateTime, Lake, zone) %>% 
  mutate(meanTP = mean(TP_mgL)) %>% ungroup() %>% 
  mutate(DateTime = as.Date(DateTime))

# TP plot
ggplot(modTP, aes(x = DateTime, y = TP_mgL)) + mytheme + 
  #scale_y_continuous(limits=c(0,7)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(TP~(mg~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(Lake ~ zone, scales='free_y') +
  geom_point(data = subset(manTP, Lake == 'Mendota'),
             aes(x = DateTime, y = meanTP), cex=1.75, pch=24, fill='tomato') +
  geom_point(data = subset(manTP, Lake == 'Sunapee'),
             aes(x = DateTime, y = meanTP), cex=1.75, pch=24, fill='tomato')

######## MENDOTA PLOTS ALONE #######
temp_m <- ggplot(subset(modTemps, Lake=='Mendota'), aes(x = DateTime, y = meanTemp)) + mytheme + 
  scale_y_continuous(limits=c(0,30)) +
  scale_x_date(date_breaks = '2 years', labels= NULL) +
  labs(x= '', y=expression(Temp.~(degree*C))) +
  geom_line(lwd=1, col='gray30') + 
  facet_grid(. ~ zone, scales='free_y') +
  geom_point(data = subset(buoyTemps, zone=='0-4m' & Lake == 'Mendota'), 
             aes(x = DateTime, y = Temp), cex=1.75, pch=21, fill='dodgerblue') +
  geom_point(data = subset(manTemps, Lake == 'Mendota'),
             aes(x = DateTime, y = meanTemp), cex=1.75, pch=24, fill='tomato')

do_m <- ggplot(subset(modDO, Lake=='Mendota'), aes(x = DateTime, y = DO_mgL)) + mytheme + 
  scale_y_continuous(limits=c(0,15)) +
  scale_x_date(date_breaks = '2 years', labels= NULL,
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(DO~(mg~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(. ~ zone) +
  theme(strip.text.x = element_blank())+
  geom_point(data = subset(buoyDO, zone=='0-4m' & Lake == 'Mendota'), 
             aes(x = DateTime, y = DO_mgL), cex=1.75, pch=21, fill='dodgerblue') +
  geom_point(data = subset(manDO, Lake == 'Mendota'),
             aes(x = DateTime, y = meanDO), cex=1.75, pch=24, fill='tomato')

tn_m <- ggplot(subset(modTN, Lake=='Mendota'), aes(x = DateTime, y = TN_mgL*1000)) + mytheme + 
  scale_y_continuous(breaks=seq(0,6000,2000)) +
  scale_x_date(date_breaks = '2 years', labels= NULL,
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(TN~(mu*g~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(. ~ zone) +
  theme(strip.text.x = element_blank())+
  geom_point(data = subset(manTN, Lake == 'Mendota'),
             aes(x = DateTime, y = meanTN*1000), cex=1.75, pch=24, fill='tomato')# +
  #geom_hline(data = subset(manTN, Lake == 'Mendota' & zone=='0-4m'), aes(yintercept=960), 
  #           col='tomato', lty=2, lwd=1)

tp_m <- ggplot(subset(modTP, Lake=='Mendota'), aes(x = DateTime, y = TP_mgL*1000)) + mytheme + 
  scale_y_continuous(breaks=seq(0,600,200)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(TP~(mu*g~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(. ~ zone) +
  theme(strip.text.x = element_blank())+
  geom_point(data = subset(manTP, Lake == 'Mendota'),
             aes(x = DateTime, y = meanTP*1000), cex=1.75, pch=24, fill='tomato') #+
  #geom_hline(data = subset(manTP, Lake == 'Mendota' & zone=='0-4m'), aes(yintercept=85), 
  #           col='tomato', lty=2, lwd=1)

MEND <- plot_grid(temp_m, do_m, tn_m, tp_m, ncol=1, align='v', labels='AUTO', 
          rel_heights=c(1.1,1,1,1), label_y=c(1,1.1,1.1,1.15))

tiff(filename = "./output/figures/Figure2.tif", width = 8, height = 10, units = "in", compression = c("none"),res = 500)
MEND
dev.off()

######## SUNAPEE PLOTS ALONE #######
temp_s <- ggplot(subset(modTemps, Lake=='Sunapee'), aes(x = DateTime, y = meanTemp)) + mytheme + 
  scale_y_continuous(limits=c(0,30)) +
  scale_x_date(date_breaks = '2 years', labels= NULL) +
  labs(x= '', y=expression(Temp.~(degree*C))) +
  geom_line(lwd=1, col='gray30') + 
  theme(legend.position = c(1,1), legend.justification = c(1,1), legend.title = element_blank())+
  facet_grid(. ~ zone) +
  geom_point(data = subset(buoyTemps, zone=='0-4m' & Lake == 'Sunapee'), 
             aes(x = DateTime, y = Temp), cex=1.75, pch=21, fill='dodgerblue') +
  geom_point(data = subset(manTemps, Lake == 'Sunapee'),
             aes(x = DateTime, y = meanTemp), cex=1.75, pch=24, fill='tomato')

do_s <- ggplot(subset(modDO, Lake=='Sunapee'), aes(x = DateTime, y = DO_mgL)) + mytheme + 
  scale_y_continuous(limits=c(0,15)) +
  scale_x_date(date_breaks = '2 years', labels= NULL,
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(DO~(mg~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(. ~ zone) +
  theme(strip.text.x = element_blank())+
  geom_point(data = subset(buoyDO, zone=='0-4m' & Lake == 'Sunapee'), 
             aes(x = DateTime, y = DO_mgL), cex=1.75, pch=21, fill='dodgerblue') +
  geom_point(data = subset(manDO, Lake == 'Sunapee'),
             aes(x = DateTime, y = meanDO), cex=1.75, pch=24, fill='tomato')

tn_s <- ggplot(subset(modTN, Lake=='Sunapee'), aes(x = DateTime, y = TN_mgL*1000)) + mytheme + 
  scale_y_continuous(limits=c(0,1000)) +
  scale_x_date(date_breaks = '2 years', labels= NULL,
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(TN~(mu*g~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(. ~ zone) +
  theme(strip.text.x = element_blank())+
  geom_point(data = subset(manTN, Lake == 'Sunapee'),
             aes(x = DateTime, y = meanTN*1000), cex=1.75, pch=24, fill='tomato') #+
  #geom_hline(data = subset(manTN, Lake == 'Sunapee' & zone=='0-4m'), aes(yintercept=170), 
  #           col='tomato', lty=2, lwd=1)

tp_s <- ggplot(subset(modTP, Lake=='Sunapee'), aes(x = DateTime, y = TP_mgL*1000)) + mytheme + 
  scale_y_continuous(limits=c(0,40), breaks=seq(0,40,10)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= 'Year', y=expression(TP~(mu*g~L^-1))) +
  geom_line(lwd=1, col='gray30') +
  facet_grid(. ~ zone) +
  theme(strip.text.x = element_blank())+
  geom_point(data = subset(manTP, Lake == 'Sunapee'),
             aes(x = DateTime, y = meanTP*1000), cex=1.75, pch=24, fill='tomato')#+
  #geom_hline(data = subset(manTP, Lake == 'Sunapee' & zone=='0-4m'), aes(yintercept=5.3), 
  #           col='tomato', lty=2, lwd=1)

SUN <- plot_grid(temp_s, do_s, tn_s, tp_s, ncol=1, align='v', labels='AUTO', 
          rel_heights=c(1.1,1,1,1), label_y=c(1,1.1,1.1,1.15))

tiff(filename = "./output/figures/Figure3.tif", width = 8, height = 10, units = "in", compression = c("none"),res = 500)
SUN
dev.off()
