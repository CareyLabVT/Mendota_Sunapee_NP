# Load libraries ####
pacman::p_load(akima, colorRamps, cowplot, glmtools, lubridate, tidyverse)

mytheme <- theme(panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), 
                 axis.line.x = element_line(colour = "black"), 
                 axis.line.y = element_line(colour = "black"), 
                 axis.text.x = element_text(size=14, colour = "black"), 
                 axis.text.y = element_text(size=14, colour='black'), 
                 axis.title.x=element_text(size=14), 
                 axis.title.y=element_text(size=14),
                 plot.title = element_text(hjust = 0, size=16))

#### Pull temp, DO at 0.5m intervals for baseline and +7 scenarios ####
#### MENDOTA ######
sim_folder <- './Mendota/GLM/Mendota_GRAPLE/GRAPLE_20190418'

#### BASELINE, Mendota ####
nc_file <- file.path(sim_folder, 'baseline.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,.5)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,.5))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,.5)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,.5)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))

output <- left_join(Temp, DO) %>% left_join(., TP) %>% left_join(.,TN) %>% 
  mutate(Sim = "0")

#### PLUS 6, Mendota ####
nc_file <- file.path(sim_folder, 'plus6.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,.5)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,.5))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,.5)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,.5)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% left_join(.,TN) %>% 
                               mutate(Sim = "6"))) %>% mutate(Lake = "Mendota")

write_csv(output, paste("./output/Mendota_highres_", format(Sys.Date(), "%Y%m%d"),'.csv', sep=""), append=F)

#### SUNAPEE ######
sim_folder <- './Sunapee/GLM/Sunapee_GRAPLE/newInflow_20190820'

#### BASELINE, Sunapee ####
nc_file <- file.path(sim_folder, 'baseline.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,.5)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,.5))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,.5)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,.5)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))

output <- left_join(Temp, DO) %>% left_join(., TP) %>% left_join(.,TN) %>%
  mutate(Sim = "0")

#### PLUS 6, Sunapee ####
nc_file <- file.path(sim_folder, 'plus6.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,.5)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,.5))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,.5)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,.5)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% left_join(.,TN) %>% 
                               mutate(Sim = "6"))) %>% mutate(Lake = "Sunapee")

write_csv(output, paste("./output/Sunapee_highres_", format(Sys.Date(), "%Y%m%d"),'.csv', sep=""), append=F)


#### Join Mendota, Sunapee data, trim to target water year ####
plotYear <- 2011

mendota <- read_csv('./output/Mendota_highres_20190422.csv') %>% 
  mutate(year = year(DateTime), month = month(DateTime),
         WaterYear = ifelse(month > 10, year+1, year)) %>% #Water year Nov. 1 to Oct. 31
  filter(month >=4, month<=10, year == plotYear, depth <= '20') %>% 
  mutate(Date = as.Date(DateTime)) %>% 
  select(Date, depth, Temp:Sim)

men0 <- mendota %>% filter(Sim=='0') %>%# select(Date, depth, Temp) %>% 
  arrange(Date) %>% group_by(depth) %>% 
  mutate(index = row_number()+90) %>% select(-Date)

interp_temp_men0 <- interp(x = men0$index,
                           y = men0$depth,
                           z = men0$Temp, 
                           yo = seq(0, 20, by = 0.1))

interp_temp_men0 <- interp2xyz(interp_temp_men0, data.frame=T)

interp_do_men0 <- interp(x = men0$index,
                           y = men0$depth,
                           z = men0$DO_mgL, 
                           yo = seq(0, 20, by = 0.1))

interp_do_men0 <- interp2xyz(interp_do_men0, data.frame=T)

men6 <- mendota %>% filter(Sim=='6') %>%# select(Date, depth, Temp) %>% 
  arrange(Date) %>% group_by(depth) %>% 
  mutate(index = row_number()+90) %>% select(-Date)

interp_temp_men6 <- interp(x = men6$index,
                           y = men6$depth,
                           z = men6$Temp, 
                           yo = seq(0, 20, by = 0.1))

interp_temp_men6 <- interp2xyz(interp_temp_men6, data.frame=T)

interp_do_men6 <- interp(x = men6$index,
                         y = men6$depth,
                         z = men6$DO_mgL, 
                         yo = seq(0, 20, by = 0.1))

interp_do_men6 <- interp2xyz(interp_do_men6, data.frame=T)

sunapee <- read_csv('./output/Sunapee_highres_20190422.csv') %>% 
  mutate(year = year(DateTime), month = month(DateTime),
         WaterYear = ifelse(month > 10, year+1, year)) %>% #Water year Nov. 1 to Oct. 31
  filter(month >=4, month<=10, year == plotYear, depth <= '20') %>% 
  mutate(Date = as.Date(DateTime)) %>% 
  select(Date, depth, Temp:Sim)
  
sun0 <- sunapee %>% filter(Sim=='0' & depth <33) %>%# select(Date, depth, Temp) %>% 
  arrange(Date) %>% group_by(depth) %>% 
  mutate(index = row_number()+90) %>% select(-Date)
  
interp_temp_sun0 <- interp(x = sun0$index,
                          y = sun0$depth,
                          z = sun0$Temp, 
                          yo = seq(0, 20, by = 0.1))


interp_temp_sun0 <- interp2xyz(interp_temp_sun0, data.frame=T)

interp_do_sun0 <- interp(x = sun0$index,
                           y = sun0$depth,
                           z = sun0$DO_mgL, 
                           yo = seq(0, 20, by = 0.1))


interp_do_sun0 <- interp2xyz(interp_do_sun0, data.frame=T)

sun6 <- sunapee %>% filter(Sim=='6' & depth <33) %>%# select(Date, depth, Temp) %>% 
  arrange(Date) %>% group_by(depth) %>% 
  mutate(index = row_number()+90) %>% select(-Date)

interp_temp_sun6 <- interp(x = sun6$index,
                           y = sun6$depth,
                           z = sun6$Temp, 
                           yo = seq(0, 20, by = 0.1))


interp_temp_sun6 <- interp2xyz(interp_temp_sun6, data.frame=T)

interp_do_sun6 <- interp(x = sun6$index,
                         y = sun6$depth,
                         z = sun6$DO_mgL, 
                         yo = seq(0, 20, by = 0.1))

interp_do_sun6 <- interp2xyz(interp_do_sun6, data.frame=T)

# Plotting Temperature ####
menTemp0 <- ggplot(interp_temp_men0, aes(x= x, y= y))+
  geom_raster(aes(fill=z)) +
  scale_x_continuous(expand=c(0,0), limits=c(91,305), breaks=c(91,152,213,274),
                     labels=c('Apr-1','Jun-1','Aug-1','Oct-1')) +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours = blue2green2red(60), limits=c(0,35),
                       breaks=seq(0,35,5), labels=seq(0,35,5))+
  labs(x = "", y = "Depth (m)", fill=expression(''*~degree*C*'')) +
  ggtitle(expression(Mendota~~+0*degree*C)) +
  mytheme + theme(legend.position='none')

menTemp6 <- ggplot(interp_temp_men6, aes(x= x, y= y))+
  geom_raster(aes(fill=z)) +
  scale_x_continuous(expand=c(0,0), limits=c(91,305), breaks=c(91,152,213,274),
                     labels=c('Apr-1','Jun-1','Aug-1','Oct-1')) +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours = blue2green2red(60), limits=c(0,35),
                       breaks=seq(0,35,5), labels=seq(0,35,5),
                       guide= guide_colorbar(barwidth=1.5, barheight=15)) +
  labs(x = "", y = "", fill=expression(''*~degree*C*'')) +
  ggtitle(expression(Mendota~~+6*degree*C)) +
  mytheme + theme(legend.text.align = 1)

sunTemp0 <- ggplot(interp_temp_sun0, aes(x= x, y= y))+
  geom_raster(aes(fill=z)) +
  scale_x_continuous(expand=c(0,0), limits=c(91,305), breaks=c(91,152,213,274),
                     labels=c('Apr-1','Jun-1','Aug-1','Oct-1')) +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours = blue2green2red(60), limits=c(0,32),
                       breaks=seq(0,32,8), labels=seq(0,32,8)) +
  labs(x = "Date", y = "Depth (m)", fill=expression(''*~degree*C*'')) +
  ggtitle(expression(Sunapee~~+0*degree*C)) +
  mytheme + theme(legend.position='none')

sunTemp6 <- ggplot(interp_temp_sun6, aes(x= x, y= y))+
  geom_raster(aes(fill=z)) +
  scale_x_continuous(expand=c(0,0), limits=c(91,305), breaks=c(91,152,213,274),
                     labels=c('Apr-1','Jun-1','Aug-1','Oct-1')) +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours = blue2green2red(60), limits=c(0,32),
                       breaks=seq(0,32,8), labels=seq(0,32,8),
                       guide= guide_colorbar(barwidth=1.5, barheight=15)) +
  labs(x = "Date", y = "", fill=expression(''*~degree*C*'')) +
  ggtitle(expression(Sunapee~~+6*degree*C)) +
  mytheme + theme(legend.text.align = 1)

plot_grid(menTemp0, menTemp6, sunTemp0, sunTemp6, labels='AUTO', 
          nrow=2, ncol=2, rel_widths = c(1,1.2))

# Plotting DO ####
menDO0 <- ggplot(interp_do_men0, aes(x= x, y= y))+
  geom_raster(aes(fill=z)) +
  scale_x_continuous(expand=c(0,0), limits=c(91,305), breaks=c(91,152,213,274),
                     labels=c('Apr-1','Jun-1','Aug-1','Oct-1')) +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours = rev(blue2green2red(60)), limits=c(0,15),
                       breaks=seq(0,15,3), labels=seq(0,15,3))+
  labs(x = "", y = "Depth (m)", fill=expression('mg'~~L^-1)) +
  #geom_hline(yintercept = 20, lty=2, col='black') +
  ggtitle(expression(Mendota~~+0*degree*C)) +
  mytheme + theme(legend.position='none')

menDO6 <- ggplot(interp_do_men6, aes(x= x, y= y))+
  geom_raster(aes(fill=z)) +
  scale_x_continuous(expand=c(0,0), limits=c(91,305), breaks=c(91,152,213,274),
                     labels=c('Apr-1','Jun-1','Aug-1','Oct-1')) +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours = rev(blue2green2red(60)), limits=c(0,15),
                       breaks=seq(0,15,3), labels=seq(0,15,3),
                       guide= guide_colorbar(barwidth=1.5, barheight=15))+
  labs(x = "", y = "", fill=expression('mg'~~L^-1)) +
  #geom_hline(yintercept = 20, lty=2, col='black') +
  ggtitle(expression(Mendota~~+6*degree*C)) +
  mytheme + theme(legend.text.align = 1)

sunDO0 <- ggplot(interp_do_sun0, aes(x= x, y= y))+
  geom_raster(aes(fill=z)) +
  scale_x_continuous(expand=c(0,0), limits=c(91,305), breaks=c(91,152,213,274),
                     labels=c('Apr-1','Jun-1','Aug-1','Oct-1')) +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours = rev(blue2green2red(60)), limits=c(0,15),
                       breaks=seq(0,15,3), labels=seq(0,15,3))+
  labs(x = "Date", y = "Depth (m)", fill=expression('mg'~~L^-1)) +
  #geom_hline(yintercept = 28, lty=2, col='black') +
  ggtitle(expression(Sunapee~~+0*degree*C)) +
  mytheme + theme(legend.position='none')

sunDO6 <- ggplot(interp_do_sun6, aes(x= x, y= y))+
  geom_raster(aes(fill=z)) +
  scale_x_continuous(expand=c(0,0), limits=c(91,305), breaks=c(91,152,213,274),
                     labels=c('Apr-1','Jun-1','Aug-1','Oct-1')) +
  scale_y_reverse(expand=c(0,0)) +
  scale_fill_gradientn(colours = rev(blue2green2red(60)), limits=c(0,15),
                       breaks=seq(0,15,3), labels=seq(0,15,3),
                       guide= guide_colorbar(barwidth=1.5, barheight=15))+
  labs(x = "Date", y = "", fill=expression('mg'~~L^-1)) +
  #geom_hline(yintercept = 28, lty=2, col='black') +
  ggtitle(expression(Sunapee~~+6*degree*C)) +
  mytheme + theme(legend.text.align = 1)

plot_grid(menDO0, menDO6, sunDO0, sunDO6, labels='AUTO', 
          nrow=2, ncol=2, rel_widths = c(1,1.2))
