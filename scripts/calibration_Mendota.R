# Script for Mendota GLM calibration with ####
# calculation of goodness of fit metrics

# Load libraries, set directory and date ranges ####
#install.packages('pacman')
pacman::p_load(broom, GLMr,  glmtools,  hydroGOF,  lubridate,  tidyverse)
options(scipen=999)

sim_folder  <- ('./Mendota/GLM')

startDate = ymd('2003-11-08') # start date for model runs
endDate = ymd('2014-12-31') # end date for model runs

calStart = ymd('2004-04-01') # calibration start
calEnd = ymd('2010-12-31') # calibration end

calculate_GOF <- function(x, y, z){
  RMSE = round(rmse(x, y), 2)
  R2 = round((cor(x, y, method="pearson", use= 'na.or.complete'))^2, 2)
  NMAE = round((mae(x, y) * count(z)) / (count(z)* mean(y)),2)
  rho = round(cor(x, y, method="spearman", use= 'na.or.complete'),2)
  N = count(z)
  
  return(data.frame(N, R2, RMSE, rho, NMAE))
  
}

#### Run GLM model ####
nml_file <- (paste0(sim_folder, "/glm2.nml"))
nml <- read_nml(nml_file)  # Read nml file
run_glm(sim_folder,  verbose=TRUE)
SimFile <- file.path(sim_folder, 'output.nc')

#### GOF metrics: Lake stage ####
ObsStage <- read_csv(paste0(sim_folder, '/observed_data/field_stage.csv'))

ObsStageResampled <- left_join((ObsStage %>% na.omit %>% rename(ObsStage = stage)),
                               (get_surface_height(file = SimFile) %>% 
                                  rename(datetime = DateTime, ModStage = surface_height))) %>% na.omit %>%
  mutate(period = if_else((datetime >= calStart & datetime <= calEnd), "cal", 
                          if_else(datetime > calEnd, "val", "NA"))) %>% filter(period != "NA")

plot(ObsStage ~ datetime, data = ObsStageResampled, ylim = c(23,26))
points(ModStage ~ datetime, data = ObsStageResampled, col = 'red')

GOFmetrics <- ObsStageResampled %>% mutate(Metric = "Stage") %>% 
  group_by(Metric) %>% #group_by(Metric, period) %>% 
  do(calculate_GOF(x=.$ModStage, y= .$ObsStage, z = .))

#### GOF metrics: Manual temperatures ####
manTemp <- read_csv(paste0(sim_folder, '/observed_data/manual_temps.csv'))

manTempResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/manual_temps.csv', method = 'interp') %>%
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths<-seq(16,24,1)

for(i in 1:length(depths)){
  tempdf <- subset(manTempResampled, manTempResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_temp, type='b', col='red',
       ylab="temperature", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]),
       ylim=c(0,30))
  points(tempdf$DateTime,tempdf$Modeled_temp, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (manTempResampled %>% filter(Depth <= 4) %>% 
                           mutate(Metric = "Manual Temp Daily Max 0-4m") %>% 
                           group_by(Metric, DateTime) %>% 
                           summarise(MaxObs = max(Observed_temp), MaxMod = max(Modeled_temp)) %>% 
                           do(calculate_GOF(x=.$MaxMod, y= .$MaxObs, z = .))),
                        (manTempResampled %>% filter(Depth >= 16 & Depth <= 20) %>% 
                           mutate(Metric = "Manual Temp Daily Max 16-20m") %>% 
                           group_by(Metric, DateTime) %>% 
                           summarise(MaxObs = max(Observed_temp), MaxMod = max(Modeled_temp)) %>% 
                           do(calculate_GOF(x=.$MaxMod, y= .$MaxObs, z = .))))

# Thermocline Depth
compare_to_field(SimFile, './Mendota/GLM/observed_data/manual_temps.csv', nml_file, metric='thermo.depth',as_value=F,
                 na.rm=TRUE, precision='days',method='interp')

therm_depths <- compare_to_field(SimFile, './Mendota/GLM/observed_data/manual_temps.csv', metric="thermo.depth",as_value=T,
                 na.rm=TRUE, precision="days",method="interp")
plot(therm_depths$DateTime, therm_depths$mod, type = "b", ylim=c(0,30),main="obs=red,mod=black")
points(therm_depths$DateTime, therm_depths$obs, col='red', type='b')

#### GOF metrics: Buoy temperatures ####
buoyTemp <- read_csv(paste0(sim_folder, '/observed_data/buoy_temps.csv'))

buoyTempResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/buoy_temps.csv',  method = 'interp') %>% 
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths<-seq(1,20,1)

for(i in 1:length(depths)){
  tempdf <- subset(buoyTempResampled, buoyTempResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_temp, type='b', col='red',
       ylab="temperature", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]),
       ylim=c(0,30))
  points(tempdf$DateTime,tempdf$Modeled_temp, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (buoyTempResampled %>% filter(Depth <= 4) %>% 
                           mutate(Metric = "Buoy Daily Max. Temp 0-4m") %>%
                           group_by(Metric, DateTime) %>% 
                           summarise(MaxObs = max(Observed_temp), MaxMod = max(Modeled_temp)) %>% 
                           do(calculate_GOF(x=.$MaxMod, y= .$MaxObs, z = .))),
                        (buoyTempResampled %>% filter(Depth >= 16) %>% 
                           mutate(Metric = "Buoy Temp Daily Max 16-20m") %>% 
                           group_by(Metric, DateTime) %>% 
                           summarise(MaxObs = max(Observed_temp), MaxMod = max(Modeled_temp)) %>% 
                           do(calculate_GOF(x=.$MaxMod, y= .$MaxObs, z = .))))

#### GOF metrics: Manual DO ####
manDO <- read_csv(paste0(sim_folder, '/observed_data/manual_DO.csv'))

manDOResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/manual_DO.csv', var_name= "OXY_oxy", method= 'interp') %>%
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths<- seq(1,24, 1)

for(i in 1:length(depths)){
  tempdf <- subset(manDOResampled, manDOResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_OXY_oxy, type='b', col='red',
       ylab="Oxygen", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]))
  points(tempdf$DateTime,tempdf$Modeled_OXY_oxy, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (manDOResampled %>% filter(Depth <= 4) %>% 
                           mutate(Metric = "Manual DO Daily Mean 0-4m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_OXY_oxy), MeanMod = mean(Modeled_OXY_oxy)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))),
                        (manDOResampled %>% filter(Depth >= 16 & Depth <= 20) %>% 
                           mutate(Metric = "Manual DO Daily Mean 16-20m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_OXY_oxy), MeanMod = mean(Modeled_OXY_oxy)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))))

#### GOF metrics: Buoy DO, 0.5m only ####
buoyDO <- read_csv(paste0(sim_folder, '/observed_data/buoy_DO.csv'))

buoyDOResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/buoy_DO.csv', var_name= "OXY_oxy", method= 'interp') %>%
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

plot(buoyDOResampled$DateTime, buoyDOResampled$Observed_OXY_oxy, type='b', col='red',
     ylab="Oxygen", xlab="time", main="Obs=red,Mod=black,Depth=0.5m")
points(buoyDOResampled$DateTime,buoyDOResampled$Modeled_OXY_oxy, type="b", col='black')

GOFmetrics <- bind_rows(GOFmetrics,
                        (buoyDOResampled %>% mutate(Metric = "Buoy DO Daily Mean 0.5m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_OXY_oxy), MeanMod = mean(Modeled_OXY_oxy)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))))  

#### GOF metrics: Manual TN ####
manTN <- read_csv(paste0(sim_folder, '/observed_data/manual_TN.csv'))

TNResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/manual_TN.csv', var_name = "TOT_tn", method = 'interp') %>%
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths <- c(0,4,8,12,16,20,22)

for(i in 1:length(depths)){
  tempdf <- subset(TNResampled, TNResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_TOT_tn, type='b', col='red',
       ylab="TN", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]))
  points(tempdf$DateTime,tempdf$Modeled_TOT_tn, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (TNResampled %>% filter(Depth <= 4) %>% 
                           mutate(Metric = "Manual TN 0-4m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_TOT_tn), MeanMod = mean(Modeled_TOT_tn)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))),
                        (TNResampled %>% filter(Depth >= 16 & Depth <= 20) %>% 
                           mutate(Metric = "Manual TN 16-20m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_TOT_tn), MeanMod = mean(Modeled_TOT_tn)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))))

# Ammonium ####
manAMM_NIT <- read_csv(paste0(sim_folder, '/observed_data/manual_amm_nit.csv'))

AMMResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/manual_amm_nit.csv', 
                                  var_name = "NIT_amm", method = 'interp') %>%
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths <- c(0,4,8,12,16,20, 22)

for(i in 1:length(depths)){
  tempdf <- subset(AMMResampled, AMMResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_NIT_amm, type='b', col='red',
       ylab="Amm", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]))
  points(tempdf$DateTime,tempdf$Modeled_NIT_amm, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (AMMResampled %>% filter(Depth <= 4) %>% 
                           mutate(Metric = "Manual AMM 0-4m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_NIT_amm), MeanMod = mean(Modeled_NIT_amm)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))),
                        (AMMResampled %>% filter(Depth >= 16 & Depth <= 20) %>% 
                           mutate(Metric = "Manual AMM 16-20m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_NIT_amm), MeanMod = mean(Modeled_NIT_amm)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))))

# Nitrate ####
NITResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/manual_amm_nit.csv', 
                                  var_name = "NIT_nit", method = 'interp') %>%
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths <- c(0,4,8,12,16,20, 22)

for(i in 1:length(depths)){
  tempdf<-subset(NITResampled, NITResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_NIT_nit, type='b', col='red',
       ylab="Nitrate", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]))
  points(tempdf$DateTime,tempdf$Modeled_NIT_nit, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (NITResampled %>% filter(Depth <= 4) %>% 
                           mutate(Metric = "Manual NIT 0-4m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_NIT_nit), MeanMod = mean(Modeled_NIT_nit)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))),
                        (NITResampled %>% filter(Depth >= 16 & Depth <= 20) %>% 
                           mutate(Metric = "Manual NIT 16-20m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_NIT_nit), MeanMod = mean(Modeled_NIT_nit)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))))

#### GOF metrics: Manual TP ####
manTP <- read_csv(paste0(sim_folder, '/observed_data/manual_TP.csv'))

TPResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/manual_TP.csv', var_name = "TOT_tp", method = 'interp') %>% 
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths <- c(0,4,8,12,16,20, 22)

for(i in 1:length(depths)){
  tempdf<-subset(TPResampled, TPResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_TOT_tp, type='b', col='red',
       ylab="TP", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]),
       ylim=c(0,25))
  points(tempdf$DateTime,tempdf$Modeled_TOT_tp, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (TPResampled %>% filter(Depth <= 4) %>% 
                           mutate(Metric = "Manual TP 0-4m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_TOT_tp), MeanMod = mean(Modeled_TOT_tp)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))),
                        (TPResampled %>% filter(Depth >= 16 & Depth <= 20) %>% 
                           mutate(Metric = "Manual TP 16-20m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_TOT_tp), MeanMod = mean(Modeled_TOT_tp)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))))

# FRP ####
manFRP <- read_csv(paste0(sim_folder, '/observed_data/manual_frp.csv'))

FRPResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/manual_frp.csv', 
                                  var_name = "PHS_frp", method = 'interp') %>%
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths <- c(0,4,8,12,16,20, 22)

for(i in 1:length(depths)){
  tempdf<-subset(FRPResampled, FRPResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_PHS_frp, type='b', col='red',
       ylab="FRP", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]),
       ylim=c(0,20))
  points(tempdf$DateTime,tempdf$Modeled_PHS_frp, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (FRPResampled %>% filter(Depth <= 4) %>% 
                           mutate(Metric = "Manual FRP 0-4m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_PHS_frp), MeanMod = mean(Modeled_PHS_frp)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))),
                        (FRPResampled %>% filter(Depth >= 16 & Depth <= 20) %>% 
                           mutate(Metric = "Manual FRP 16-20m") %>% 
                           group_by(Metric, DateTime) %>%
                           summarise(MeanObs = mean(Observed_PHS_frp), MeanMod = mean(Modeled_PHS_frp)) %>% 
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))))

#### GOF metrics: Manual Chl-a ####
manChla <-  read_csv(paste0(sim_folder, '/observed_data/manual_chla.csv'))

ChlaResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/manual_chla.csv', var_name = "PHY_TCHLA", method = 'interp') %>% 
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

depths <- seq(0,12,2)

for(i in 1:length(depths)){
  tempdf<-subset(ChlaResampled, ChlaResampled$Depth==depths[i])
  plot(tempdf$DateTime, tempdf$Observed_PHY_TCHLA, type='b', col='red',
       ylab="Tot Chla", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]))
  points(tempdf$DateTime,tempdf$Modeled_PHY_TCHLA, type="b", col='black')
  
}

GOFmetrics <- bind_rows(GOFmetrics, 
                        (ChlaResampled %>% filter(Depth <= 2) %>% mutate(Metric = "Manual Chl-a 0-2m") %>% 
                           group_by(Metric, DateTime) %>% 
                           summarise(MeanObs = mean(Observed_PHY_TCHLA), MeanMod = mean(Modeled_PHY_TCHLA)) %>%
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))),
                        (ChlaResampled %>% # Summer (Jun-Aug) only 
                           mutate(Metric = "Manual Mean Summer Chl-a 0-2m",
                                   month = month(DateTime), year = year(DateTime)) %>%
                           filter(Depth <= 2, month > 5 & month <9) %>% group_by(Metric, year) %>%
                           summarise(MeanObs = mean(Observed_PHY_TCHLA), MeanMod = mean(Modeled_PHY_TCHLA))) %>%  
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .)))

#### GOF metrics: Buoy Chl-a ####
buoyChla <-  read_csv(paste0(sim_folder, '/observed_data/buoy_chla.csv'))

buoyChlaResampled <- resample_to_field(SimFile, './Mendota/GLM/observed_data/buoy_chla.csv', var_name = "PHY_TCHLA", method = 'interp') %>% 
  mutate(period = if_else((DateTime >= calStart & DateTime <= calEnd), "cal", 
                          if_else(DateTime > calEnd, "val", "NA"))) %>% filter(period != "NA")

plot(buoyChlaResampled$DateTime, buoyChlaResampled$Observed_PHY_TCHLA, type='b', col='red',
     ylab="Chla (ug/L)", xlab="time", main=paste0("Obs=red,Mod=black,Depth=",depths[i]))
points(buoyChlaResampled$DateTime,buoyChlaResampled$Modeled_PHY_TCHLA, type="b", col='black')

GOFmetrics <- bind_rows(GOFmetrics, 
                        (buoyChlaResampled %>% mutate(Metric = "Buoy Chl-a 0.5m") %>% 
                           group_by(Metric, DateTime) %>% 
                           summarise(MeanObs = mean(Observed_PHY_TCHLA), MeanMod = mean(Modeled_PHY_TCHLA)) %>%
                           do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .))),
                        (buoyChlaResampled %>% # Summer (Jun-Aug) only 
                           mutate(Metric = "Buoy Mean Summer Chl-a 0.5m",
                                  month = month(DateTime), year = year(DateTime)) %>%
                           filter(month > 5 & month <9) %>% group_by(Metric, year) %>%
                           summarise(MeanObs = mean(Observed_PHY_TCHLA), MeanMod = mean(Modeled_PHY_TCHLA))) %>%  
                          do(calculate_GOF(x=.$MeanMod, y= .$MeanObs, z = .)))

#### Write GOF table ####
GOFmetrics <- GOFmetrics %>% rename(NMAE = n.1) %>%
  write_csv(paste('./output/GOF_metrics_Mendota', format(Sys.Date(), "%Y%m%d"),'.csv', sep=""), append=F)

#### Heatmaps of variables ####
plot_temp(SimFile)
plot_var(SimFile, 'OXY_oxy')
plot_var(SimFile, 'TOT_tn')
plot_var(SimFile, 'NIT_amm')
plot_var(SimFile, 'NIT_nit')

plot_var(SimFile, 'TOT_tp')
plot_var(SimFile, 'PHS_frp')
plot_var(SimFile, 'PHS_frp_ads')

plot_var(SimFile, 'PHY_TCHLA')