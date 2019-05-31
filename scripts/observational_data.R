##### Munge all observational data into format needed for calibration ####
pacman::p_load(lubridate, readxl, tidyverse)

startDate = ymd('2003-11-08') # model start
endDate = ymd('2014-12-31')   # model end

# Mendota ####
sim_folder <- './Mendota/GLM'

#### Mendota Water temperatures: daily max. ####
# Manual:
manTemp <- file.path(sim_folder, "observed_data/raw/ntl29_v5.csv") %>%
  read_csv(.) %>% filter(sampledate >= startDate, sampledate <= endDate, lakeid=='ME') %>% 
  select(sampledate, depth, wtemp) %>%
  rename(DateTime = sampledate, Depth = depth, Temp = wtemp) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(Temp = max(Temp)) %>% na.omit %>% 
  write_csv(paste(sim_folder, './observed_data/manual_temps.csv', sep=""))

# Buoy (from hourly file):
buoyTemp <- file.path(sim_folder, "observed_data/raw/ntl130_2_v5.csv") %>%
  read_csv(., col_types = list(col_character(), col_number(), col_number(), col_number(), 
                               col_number(), col_number(), col_number(), col_factor())) %>% 
  mutate(DateTime = as.Date(sampledate),
         wtemp = ifelse(flag_wtemp %in% c('D', 'H'), NA, wtemp)) %>% 
  filter(DateTime >= startDate, DateTime <= endDate) %>% 
  select(DateTime, depth, wtemp, flag_wtemp) %>% 
  rename(Depth = depth, Temp = wtemp) %>% 
  filter(DateTime < as.Date("2011-08-08") | DateTime > as.Date("2011-08-18")) %>%
  filter(DateTime < as.Date("2010-11-23") | DateTime > as.Date("2010-12-02")) %>% 
  group_by(DateTime, Depth) %>% 
  summarise(Temp = max(Temp)) %>% na.omit %>% ungroup() %>% 
  mutate(DateTime = as.Date(DateTime)) %>% 
  write_csv(paste(sim_folder, '/observed_data/buoy_temps.csv', sep=""))

ggplot(buoyTemp, aes(x = DateTime, y = Temp, col = as.factor(Depth))) + 
  geom_point() + 
  facet_wrap(.~Depth)

#### Mendota DO: daily mean ####
# Manual
manDO <- file.path(sim_folder, "observed_data/raw/ntl29_v5.csv") %>%
  read_csv(.) %>% filter(sampledate >= startDate, sampledate <= endDate, lakeid=='ME') %>% 
  select(sampledate, depth, o2) %>%
  rename(DateTime = sampledate, Depth = depth, DO_mgL = o2) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(DO_mgL = mean(DO_mgL)) %>% na.omit %>% 
  mutate(OXY_oxy = DO_mgL * (1000/32)) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_DO.csv', sep=""))

# Buoy
#buoyDO <- read_csv('ntl129_3_v6.csv') %>% 
#  select(sampledate, do_raw, flag_do_raw) %>%  
#  filter(sampledate >= startDate, sampledate <= endDate) %>% 
#  filter(!is.na(do_raw), is.na(flag_do_raw)) %>% 
#  write_csv(paste(sim_folder, './observed_data/raw/buoy_do_noflags.csv')
buoyDO <- file.path(sim_folder, "observed_data/raw/buoy_do_noflags.csv") %>%
  read_csv(.) %>% 
  rename(DateTime = sampledate, DO_mgL = do_raw) %>% 
  mutate(Depth = 0.5, DateTime= as.Date(DateTime)) %>% 
  select(DateTime, Depth, DO_mgL) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(DO_mgL = mean(DO_mgL)) %>% 
  mutate(OXY_oxy = DO_mgL * (1000/32)) %>% #na.omit %>% 
  write_csv(paste(sim_folder, './observed_data/buoy_DO.csv', sep=""))

#### Mendota TN, TP ####
allChem <- file.path(sim_folder, 'observed_data/raw/ntl1_v5.csv') %>%
  read_csv(., col_types = cols(kjdl_n_sloh = "d", totpuf_sloh = "d")) %>% 
  filter(lakeid == "ME" & sampledate >= startDate & sampledate <= endDate) %>%
  select(sampledate, depth, totnuf, kjdl_n_sloh, totpuf, totpuf_sloh) %>% 
  mutate(totnuf = ifelse(is.na(totnuf), kjdl_n_sloh*1000, totnuf),
         totpuf = ifelse(is.na(totpuf), totpuf_sloh*1000, totpuf)) %>%
  mutate(DateTime= as.POSIXct(sampledate), Depth= depth, TN_mgL= totnuf*0.001, TP_mgL= totpuf*0.001) %>% 
  select(DateTime, Depth, TN_mgL, TP_mgL)

TN <- allChem %>% select(DateTime, Depth, TN_mgL) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(TN_mgL = mean(TN_mgL)) %>% 
  mutate(TOT_tn = TN_mgL*(1000/14)) %>% na.omit %>% 
  write_csv(paste(sim_folder, './observed_data/manual_TN.csv', sep=""))

TP <- allChem %>% select(DateTime, Depth, TP_mgL) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(TP_mgL = mean(TP_mgL)) %>% 
  mutate(TOT_tp = TP_mgL*(1000/30.97)) %>% na.omit %>% 
  write_csv(paste(sim_folder, './observed_data/manual_TP.csv', sep=""))

#### Mendota NIT, AMM, FRP from C.Snortheim and E.Read ####
# "CalDataManualCraig" from CNH Dropbox; units mg/L??
csPath <- file.path(sim_folder, 'observed_data/raw/CalDataManualCraig')
csNP <- dir(csPath, pattern = "*.txt") %>% #
  map_df(~ read_tsv(file.path(csPath, .))) %>% 
  gather(variable, value, frp_0:nit_22) %>%
  separate(col = variable, into = c('variable','depth'), sep= "_") %>%
  filter(!is.na(value)) %>% 
  mutate(variable = recode(variable, "frp"="PHS_frp", "amm"="NIT_amm", 
                           "nit"="NIT_nit")) %>% 
  spread(variable, value) %>% 
  mutate(Depth = as.double(depth), PHS_frp=PHS_frp/(30.97/1000), #mg/L to mmol/m3
         NIT_amm=NIT_amm/(14/1000), NIT_nit=NIT_nit/(14/1000)) %>% 
  select(DateTime, Depth, PHS_frp, NIT_amm, NIT_nit) 

# E. Read NTL-LTER data units mg/L??
erNP <- file.path(sim_folder, 'observed_data/raw/karaEnvDatalong_0.csv') %>% 
  read_csv(.) %>% filter(param %in% c('NH4','NO3NO2','SRP')) %>% 
  select(sampledate, param, value) %>% 
  mutate(DateTime = as.POSIXct(sampledate), value = as.double(value)) %>% 
  spread(param, value)%>%
  mutate(Depth = "0", Depth = as.double(Depth), PHS_frp=as.double(SRP/(30.97/1000)), #mg/L to mmol/m3
         NIT_amm=as.double(NH4/(14/1000)), NIT_nit=as.double(NO3NO2/(14/1000))) %>% 
  select(DateTime, Depth, PHS_frp, NIT_amm, NIT_nit) %>% 
  filter(DateTime <= '2009-01-01' )

solubleNP <- bind_rows(erNP, csNP)

solubleN <- solubleNP %>% select(DateTime, Depth, NIT_amm, NIT_nit) %>% 
  group_by(DateTime, Depth) %>% 
  summarize_all(mean) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_amm_nit.csv', sep=""))

solubleP <- solubleNP %>% select(DateTime, Depth, PHS_frp) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(PHS_frp = mean(PHS_frp)) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_frp.csv', sep=""))

#### Mendota carbon (DIC, DOC, POC) ####
# Manual (all mg/L; for initial conditions in glm2.nml)
manCarbon <- file.path(sim_folder, 'observed_data/raw/lake_weekly_carbon_ghg_v1.csv') %>%
  read_csv(.) %>% mutate(month = month(sampledate)) %>%
  filter(sample_site == "Deep Hole" & month == 11) %>% group_by(water_depth) %>%
  summarise(POC = mean(poc), DIC = mean(dic), DOC = mean(doc))

#### Mendota Chla ####
# Manual
manChla <- file.path(sim_folder, 'observed_data/raw/ntl38_v3.csv') %>%
  read_csv(.) %>% filter(lakeid == "ME" & sampledate >= startDate & sampledate <= endDate) %>%
  select(sampledate, depth_range_m, correct_chl_fluor) %>% # Chla is ug/L
  rename(DateTime = sampledate, Depth = depth_range_m, PHY_TCHLA = correct_chl_fluor) %>%
  filter(Depth %in% c(0,2,4,6,8,10,12)) %>%
  mutate(DateTime = as.POSIXct(DateTime), Depth = as.double(Depth)) %>%
  group_by(DateTime, Depth) %>% 
  summarize(PHY_TCHLA = mean(PHY_TCHLA)) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_chla.csv', sep=""))

# Buoy
buoychla <- file.path(sim_folder, "observed_data/raw/ntl129_1_v5.csv") %>%
  read_csv(.) %>% filter(sampledate >= startDate, sampledate <= endDate) %>% 
  select(sampledate, avg_chlor) %>% 
  rename(DateTime = sampledate, PHY_TCHLA = avg_chlor) %>%
  mutate(Depth = 0.5) %>% na.omit %>% 
  select(DateTime, Depth, PHY_TCHLA) %>% 
  write_csv(paste(sim_folder, './observed_data/buoy_chla.csv', sep=""))

#### Mendota stage ####
ObsStage <- file.path(sim_folder, 'observed_data/raw/usgs05428000.csv') %>%
  read_csv(., skip=29) %>% select(datetime, `155052_00065_00003`) %>%
  rename(stage = `155052_00065_00003`) %>%
  mutate(datetime = as.POSIXct(datetime, format= "%m/%d/%Y"), 
         stage = ((840 + stage) * 0.3048) - 234.5)  %>%
  filter(datetime >= startDate & datetime <= endDate) %>% 
  write_csv(paste(sim_folder, './observed_data/field_stage.csv', sep=""))

# Sunapee #####
sim_folder <- './Sunapee/GLM'

#### Sunapee Water temperatures: daily max. ####
# Manual:
manTemp <- file.path(sim_folder, "observed_data/raw/LMP_L1daily_temp_merge.csv") %>%
  read_csv(.) %>% filter(date >= startDate) %>% 
  rename_at(vars(starts_with("Temp_")), funs(sub("Temp_", "", .))) %>%
  rename_at(vars(ends_with("_degC")), funs(sub("m_degC","",.))) %>%
  rename_at(vars(contains("p")), funs(sub("p", ".",.))) %>% rename(DateTime = date) %>%
  gather(Depth, temp, "0":"33") %>% mutate(Depth = as.numeric(Depth)) %>% na.omit() %>% 
  filter(loc == "210", datasource !='L1 buoy data') %>% 
  select(DateTime, Depth, temp) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(Temp = max(temp)) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_temps.csv', sep=""))

# Buoy (from hourly means file):
buoyTemp <- file.path(sim_folder, "observed_data/raw/L1_buoy_temp_hourlymean.csv") %>%
  read_csv(.) %>% filter(date >= startDate) %>% 
  rename_at(vars(starts_with("Temp_")), funs(sub("Temp_", "", .))) %>%
  rename_at(vars(ends_with("_degC")), funs(sub("m_degC","",.))) %>%
  rename_at(vars(contains("p")), funs(sub("p", ".",.))) %>% rename(DateTime = date) %>%
  gather(Depth, temp, "0":"14") %>% mutate(Depth = as.numeric(Depth)) %>% na.omit() %>% 
  select(DateTime, Depth, temp) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(Temp = max(temp)) %>% 
  write_csv(paste(sim_folder, './observed_data/buoy_temps.csv', sep=""))

#### Sunapee DO: daily mean ####
# All Chemistry:
allChem <- file.path(sim_folder, "observed_data/raw/limnol_allsources_to2013.csv") %>%
  read_csv(., col_types=cols('D','f','f','d','f','f','f','d','f')) %>% 
  filter(date >= startDate) %>% rename(DateTime = date) %>% 
  mutate(Depth = as.numeric(depth.m))

# Manual:
manDO <- allChem %>% filter(loc == '210', variable == 'DO_mgL') %>% 
  rename(DO_mgL = value) %>% select(DateTime, Depth, DO_mgL) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(DO_mgL = mean(DO_mgL)) %>%
  mutate(OXY_oxy = DO_mgL * (1000/32)) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_DO.csv', sep=""))

# Buoy: 
buoyDO <- allChem %>% filter(loc == 'buoy', variable == 'DO_mgL') %>% 
  rename(DO_mgL = value) %>% select(DateTime, Depth, DO_mgL) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(DO_mgL = mean(DO_mgL)) %>%
  mutate(OXY_oxy = DO_mgL * (1000/32)) %>% 
  write_csv(paste(sim_folder, './observed_data/buoy_DO.csv', sep=""))

#### Sunapee TN: all available ####
lspaTN <- allChem %>% filter(variable %in% c('TKN_mgL', 'TN_mgL')) %>% 
  rename(TN_mgL = value) %>% mutate(Depth = as.numeric(Depth)) %>% 
  select(DateTime, Depth, loc, sitetype, TN_mgL) %>% 
  mutate(TOT_tn = TN_mgL * (1000/14))

cccTN <- file.path(sim_folder, 'observed_data/raw/All TN data edited_15Sept08.csv') %>% 
  read_csv(.) %>% filter(bag %in% c(0, 17)) %>% 
  mutate(loc = 'CCC', sitetype = 'littoral', Depth = 0, TN_mgL = tn/1000, TOT_tn = tn/14) %>% 
  select(DateTime, loc, sitetype, Depth, TN_mgL, TOT_tn)

TN <- bind_rows(lspaTN, cccTN) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(TN_mgL = mean(TN_mgL), TOT_tn = mean(TOT_tn)) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_TN.csv', sep=""))

#### Sunapee TP: (pelagic) ####
TP <- allChem %>% filter(variable %in% c('TP_mgL'), sitetype=='pelagic') %>% 
  rename(TP_mgL = value) %>% mutate(Depth = as.numeric(Depth)) %>% 
  select(DateTime, Depth, loc, TP_mgL) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(TP_mgL = mean(TP_mgL)) %>%
  mutate(TOT_tp = TP_mgL * (1000/30.97)) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_TP.csv', sep=""))

#### Sunapee NIT, AMM from CCC littoral (Burkehaven) ####
cccN <- file.path(sim_folder, 'observed_data/raw/All nitrate & ammonium data_CCC.xlsx') %>% 
  read_excel(.) %>%
  mutate(DateTime = date(DateTime), Depth = 0, NIT_amm = (AMM_mgL*1000/14), 
         NIT_nit = (NIT_mgL*1000/14)) %>% 
  select(DateTime, Depth, NIT_amm, NIT_nit) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(NIT_amm = mean(NIT_amm), NIT_nit=mean(NIT_nit)) %>% 
  write_csv(paste(sim_folder, './observed_data/manual_amm_nit.csv', sep=""))

# CSV for lake stage #
rawStage <- file.path(sim_folder, 'observed_data/raw/field_stage.csv') 

# CSV for met data # 
MetData <- read_csv("./Sunapee/GLM/met_hourly.csv")

# CSV for single inflow stream #
oneinflow <- read_csv("./Sunapee/GLM/oneInflow.csv") %>%
  mutate(year = year(time), month = month(time))

#### Sunapee Chla ####
# Manual
manChla <- allChem %>% filter(loc == '210', variable == 'chla_ugL') %>% 
  rename(PHY_TCHLA = value) %>% select(DateTime, Depth, PHY_TCHLA) %>% 
  group_by(DateTime, Depth) %>% 
  summarize(PHY_TCHLA = mean(PHY_TCHLA)) %>%
  write_csv(paste(sim_folder, './observed_data/manual_chla.csv', sep=""))

#### Sunapee stage ####
ObsStage <- file.path(sim_folder, 'observed_data/raw/field_stage.csv') %>%
  read_csv(.) %>% filter(datetime >= startDate & datetime <= endDate) %>%
  mutate(datetime = as.POSIXct(datetime)) %>% 
  write_csv(paste(sim_folder, './observed_data/field_stage.csv', sep=""))
