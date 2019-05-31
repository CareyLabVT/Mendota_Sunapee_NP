#### Plot baseline modeled nutrients for visualizations ####
# Preliminary analysis of climate warming scenarios ####
pacman::p_load(tidyverse, lubridate)

jet.colors <- colorRampPalette(c("dodgerblue4",  "dodgerblue1", "deepskyblue","cyan", 
                                 "yellow", "orange2", "red1", "red4")) 

mytheme <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), 
                 axis.line.x = element_line(colour = "black"), axis.line.y = element_line(colour = "black"), 
                 axis.text.x=element_text(size=16, colour='black'), axis.text.y=element_text(size=16, colour='black'), 
                 axis.title.x=element_text(size=16), axis.title.y=element_text(size=16))

# Read in Mendota, Sunapee data ####
output <- bind_rows(read_csv('./output/Mendota_ValDayAll_20190226.csv'), 
                   read_csv('./output/Sunapee_all_20190226.csv')) %>%
  mutate(Sim = as.factor(Sim), Lake = as.factor(Lake)) %>%
  select(Lake, Sim, DateTime, depth, Temp:PON_mgL) %>%
  gather(metric, value, Temp:PON_mgL) 

baseline <- output %>% filter(Sim == 0) 

## Use filter to summarize and analyze by specific metrics and depths ####
TNTP_baseline <- baseline %>% filter( metric == 'TN_mgL' | metric == 'TP_mgL') %>%
  mutate(zone = ifelse((Lake == 'Mendota' & depth > 22), 'bottom',
                       ifelse((Lake == 'Sunapee' & depth > 30), 'bottom', 
                              ifelse(depth <= 2, "top", NA)))) %>%
  mutate(year = year(DateTime), month = month(DateTime), day = day(DateTime)) %>%
  filter(!is.na(zone)) %>%
  group_by(metric, Lake, Sim, zone, year, month, day) %>%
  summarize(meanVal = mean(value, na.rm=T)) %>% 
  unite(ymd, year:day, sep = '-') %>%
  mutate(ymd = as.POSIXct(ymd))

labels <- c('top' = "Surface", "bottom" = "Deep")

# Moving average TN, TP ####
ggplot(subset(TNTP_baseline, zone=='top' & metric=='TN_mgL'), 
       aes(y = meanVal*1000, x = ymd, col = Lake)) + mytheme +
  geom_line(lwd=.1, alpha=0.5) + geom_smooth(se= F, lwd=2) +
  #facet_grid(.~Lake, labeller=labeller(zone = labels), scales="free_y") +
  geom_abline(slope= 0, intercept=0.5, col='black',lty=2)+
  scale_color_manual(values=c('tomato','dodgerblue')) +
  scale_x_datetime(date_breaks='2 year', date_labels='%Y',
                   #expand = c(0,0), 
                   limits=c(as.POSIXct('2005-01-01'), 
                            as.POSIXct('2015-01-01'))) +
  labs(y = expression(TN~(mu*g~L^-1)), x = "Year") +
  ggtitle("Surface (0-2 m)") +
  theme(strip.text.y = element_text(margin = margin(0,.25,0,.25, "cm")),
        legend.position=c(0,1), legend.justification = c(0,1),
        legend.title=element_blank(), plot.title=element_text(hjust=0))

# Just Sunapee 
ggplot(subset(TNTP_baseline, zone=='top' & metric=='TN_mgL' & Lake =='Sunapee'), 
       aes(y = meanVal*1000, x = ymd)) + mytheme + 
  geom_line(lwd=.1, alpha=0.5, col='dodgerblue') + geom_smooth(se= F, lwd=2, col='dodgerblue') +
  scale_x_datetime(date_breaks='2 year', date_labels='%Y',
                   #expand = c(0,0), 
                   limits=c(as.POSIXct('2005-01-01'), 
                            as.POSIXct('2015-01-01'))) +
  labs(y = expression(Surface~TN~(mu*g~L^-1)), x = "Year") +
  theme(strip.text.y = element_text(margin = margin(0,.25,0,.25, "cm")),
        legend.position="none")

# TP, both lakes
ggplot(subset(TNTP_baseline, zone=='top' & metric=='TP_mgL'), 
       aes(y = meanVal*1000, x = ymd, col = Lake)) + mytheme +
  geom_line(lwd=.1, alpha=0.5) + geom_smooth(se= F, lwd=2) +
  #facet_grid(.~Lake, labeller=labeller(zone = labels), scales="free_y") +
  scale_color_manual(values=c('tomato','dodgerblue')) +
  scale_x_datetime(date_breaks='2 year', date_labels='%Y',
                   #expand = c(0,0), 
                   limits=c(as.POSIXct('2005-01-01'), 
                            as.POSIXct('2015-01-01'))) +
  labs(y = expression(TP~(mu*g~L^-1)), x = "Year") +
  ggtitle("Surface (0-2 m)") +
  theme(strip.text.y = element_text(margin = margin(0,.25,0,.25, "cm")),
        legend.position=c(0,1), legend.justification = c(0,1),legend.title=element_blank(),
        plot.title=element_text(hjust=0))

# Just Sunapee 
ggplot(subset(TNTP_baseline, zone=='top' & metric=='TP_mgL' & Lake =='Sunapee'), 
       aes(y = meanVal*1000, x = ymd)) + mytheme + 
  geom_line(lwd=.1, alpha=0.5, col='dodgerblue') + geom_smooth(se= F, lwd=2, col='dodgerblue') +
  scale_x_datetime(date_breaks='2 year', date_labels='%Y',
                   #expand = c(0,0), 
                   limits=c(as.POSIXct('2005-01-01'), 
                            as.POSIXct('2015-01-01'))) +
  labs(y = expression(Surface~TP~(mu*g~L^-1)), x = "Year") +
  theme(strip.text.y = element_text(margin = margin(0,.25,0,.25, "cm")),
        legend.position="none")

#### TOTAL NITROGEN IN SURFACE, all sims ####
TN <- output %>% filter(metric == 'TN_mgL')

ggplot(subset(TN, depth=='1'), aes(y = value, x = DateTime, colour=Sim))+ 
  geom_line() +
  scale_colour_manual(values=c(jet.colors(8))) +
  scale_x_datetime(date_breaks='1 year', date_labels='%Y',
                   #expand = c(0,0), 
                   limits=c(as.POSIXct('2005-01-01'), 
                            as.POSIXct('2015-01-01'))) +
  facet_grid(Lake~., scales="free_y")

#### TOTAL P IN SURFACE, all sims ####
TP <- output %>% filter(metric == 'TP_mgL')

ggplot(subset(TP, depth=='1'), aes(y = value, x = DateTime, colour=Sim))+ 
  geom_line() +
  scale_colour_manual(values=c(jet.colors(8))) +
  scale_x_datetime(date_breaks='1 year', date_labels='%Y') +
  facet_grid(Lake~., scales="free_y")
