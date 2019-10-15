# Plot TN/TP distributions ####
pacman::p_load(cowplot, tidyverse, lubridate)

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

lakeYears <- lakes %>% 
  mutate(year = year(DateTime), month = month(DateTime),
         WaterYear = ifelse(month > 10, year+1, year)) %>% #Water year Nov. 1 to Oct. 31
  filter(WaterYear >2004 & WaterYear < 2015, # exclude partial year
         ifelse(Lake == "Mendota", depth %in% c(0,20), depth %in% c(0,18))) %>% 
  mutate(Sim = as.factor(Sim),
         DateTime = as.Date(DateTime)) %>% 
  filter(Sim %in% c('0','2','4','6'))

jet.colors2 <- colorRampPalette(c("dodgerblue4", "cyan", "orange2","red4")) 

######## MENDOTA PLOTS ALONE #######
depth.labs <- c("0m","20m")
names(depth.labs) <- c("0", "20")

temp_m <- ggplot(subset(lakeYears, Lake=='Mendota'), aes(x = DateTime, y = Temp, col= Sim)) + mytheme + 
  scale_y_continuous(limits=c(0,30)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(Temp.~(degree*C))) +
  geom_line() + 
  scale_color_manual(values=jet.colors(4)) +
  facet_grid(. ~ depth, scales='free_y',
             labeller = labeller(depth = depth.labs)) +
  theme(legend.position='none')

do_m <- ggplot(subset(lakeYears, Lake=='Mendota'), aes(x = DateTime, y = DO_mgL, col= Sim)) + mytheme + 
  scale_y_continuous(limits=c(0,15)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(DO~(mg~L^-1))) +
  geom_line() + 
  scale_color_manual(values=jet.colors(4)) +
  facet_grid(. ~ depth, scales='free_y') +
  theme(strip.text.x = element_blank(), legend.position='none')

tn_m <- ggplot(subset(lakeYears, Lake=='Mendota'), aes(x = DateTime, y = TN_mgL*1000, col= Sim)) + mytheme + 
  scale_y_continuous(breaks=seq(0,6000,2000)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(TN~(mu*g~L^-1))) +
  geom_line() + 
  scale_color_manual(values=jet.colors(4)) +
  facet_grid(. ~ depth, scales='free_y') +
  theme(strip.text.x = element_blank(), legend.position='none')

tp_m <- ggplot(subset(lakeYears, Lake=='Mendota'), aes(x = DateTime, y = TP_mgL*1000, col= Sim)) + mytheme + 
  scale_y_continuous(breaks=seq(0,600,200), limits=c(0,600)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(TP~(mu*g~L^-1))) +
  geom_line() + 
  scale_color_manual(values=jet.colors(4),
                     labels=c("+0°C","+2°C","+4°C","+6°C")) +
  facet_grid(. ~ depth, scales='free_y')+ 
  guides(col=guide_legend(keyheight=0.1,title="Scenario")) +
  theme(strip.text.x = element_blank(), legend.position = 'bottom',
        legend.justification = 'center')

MEND <- plot_grid(temp_m, do_m, tn_m, tp_m, ncol=1, align='v', labels='AUTO', 
          rel_heights=c(1.1,1,1,1.3), label_y=c(1,1.1,1.1,1.15))

tiff(filename = "./output/figures/FigureA1.tif", width = 8, height = 10, units = "in", compression = c("none"),res = 500)
MEND
dev.off()

######## SUNAPEE PLOTS ALONE #######
depth.labs <- c("0m","18m")
names(depth.labs) <- c("0", "18")

temp_s <- ggplot(subset(lakeYears, Lake=='Sunapee'), aes(x = DateTime, y = Temp, col=Sim)) + mytheme + 
  scale_y_continuous(limits=c(0,30)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(Temp.~(degree*C))) +
  geom_line() + 
  scale_color_manual(values=jet.colors(4)) +
  facet_grid(. ~ depth, scales='free_y',
             labeller = labeller(depth = depth.labs)) +
  theme(legend.position='none')

do_s <- ggplot(subset(lakeYears, Lake=='Sunapee'), aes(x = DateTime, y = DO_mgL, col=Sim)) + mytheme + 
  scale_y_continuous(limits=c(0,15)) +
  scale_x_date(date_breaks = '2 years', labels= NULL,
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(DO~(mg~L^-1))) +
  geom_line() + 
  scale_color_manual(values=jet.colors(4)) +
  facet_grid(. ~ depth, scales='free_y') +
  theme(strip.text.x = element_blank(), legend.position='none')

tn_s <- ggplot(subset(lakeYears, Lake=='Sunapee'), aes(x = DateTime, y = TN_mgL*1000, col=Sim)) + mytheme + 
  scale_y_continuous(limits=c(0,800)) +
  scale_x_date(date_breaks = '2 years', labels= NULL,
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= '', y=expression(TN~(mu*g~L^-1))) +
  geom_line() + 
  scale_color_manual(values=jet.colors(4)) +
  facet_grid(. ~ depth, scales='free_y') +
  theme(strip.text.x = element_blank(), legend.position='none')

tp_s <- ggplot(subset(lakeYears, Lake=='Sunapee'), aes(x = DateTime, y = TP_mgL*1000, col=Sim)) + mytheme + 
  scale_y_continuous(limits=c(0,20)) +
  scale_x_date(date_breaks = '2 years', date_labels= '%Y',
               limits = as.Date(c('2004-04-01', '2015-01-01'))) +
  labs(x= 'Year', y=expression(TP~(mu*g~L^-1))) +
  geom_line() + 
  scale_color_manual(values=jet.colors(4),
                     labels=c("+0°C","+2°C","+4°C","+6°C")) +
  facet_grid(. ~ depth, scales='free_y')+ 
  guides(col=guide_legend(keyheight=0.1,title="Scenario")) +
  theme(strip.text.x = element_blank(), legend.position = 'bottom',
        legend.justification = 'center')

SUN <- plot_grid(temp_s, do_s, tn_s, tp_s, ncol=1, align='v', labels='AUTO', 
          rel_heights=c(1.1,1,1,1.3), label_y=c(1,1.1,1.1,1.15))

tiff(filename = "./output/figures/FigureA2.tif", width = 8, height = 10, units = "in", compression = c("none"),res = 500)
SUN
dev.off()
