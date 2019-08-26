pacman::p_load(glmtools, tidyverse)

####### MENDOTA ######
sim_folder <- './Mendota/GLM/Mendota_GRAPLE/GRAPLE_20190418'

#### BASELINE ####
nc_file <- file.path(sim_folder, 'baseline.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"24") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"24") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"24") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,24,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"24") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"24") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"24") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"24") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"24") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- left_join(Vol, Temp) %>% left_join(., DO) %>% left_join(., TP) %>% 
  left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
  left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
  left_join(.,PON) %>% 
  mutate(Sim = "0")

#### PLUS 1 ####
nc_file <- file.path(sim_folder, 'plus1.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"24") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"24") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"24") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,24,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"24") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"24") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"24") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"24") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"24") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "1")))

#### PLUS 2 ####
nc_file <- file.path(sim_folder, 'plus2.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"24") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"24") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"24") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,24,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"24") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"24") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"24") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"24") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"24") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "2")))

#### PLUS 3 ####
nc_file <- file.path(sim_folder, 'plus3.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"24") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"24") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"24") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,24,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"24") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"24") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"24") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"24") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"24") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "3")))


#### PLUS 4 ####
nc_file <- file.path(sim_folder, 'plus4.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"24") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"24") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"24") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,24,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"24") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"24") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"24") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"24") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"24") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "4")))

#### PLUS 5 ####
nc_file <- file.path(sim_folder, 'plus5.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"24") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"24") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"24") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,24,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"24") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"24") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"24") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"24") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"24") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "5")))

#### PLUS 6 ####
nc_file <- file.path(sim_folder, 'plus6.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"24")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"24") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"24") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"24") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,24,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"24") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"24") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,24,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"24") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"24") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"24") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"24") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"24") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"24") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "6"))) %>%
  mutate(Lake = "Mendota")

write_csv(output, paste("./output/Mendota_11AprAll_", format(Sys.Date(), "%Y%m%d"),'.csv', sep=""), append=F)



####### SUNAPEE ######
sim_folder <- './Sunapee/GLM/Sunapee_GRAPLE/newInflow_20190820'

#### BASELINE ####
nc_file <- file.path(sim_folder, 'baseline.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"33") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"33") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"33") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,33,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"33") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"33") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"33") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"33") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"33") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- left_join(Vol, Temp) %>% left_join(., DO) %>% left_join(., TP) %>% 
  left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
  left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
  left_join(.,PON) %>% 
  mutate(Sim = "0")

#### PLUS 1 ####
nc_file <- file.path(sim_folder, 'plus1.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"33") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"33") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"33") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,33,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"33") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"33") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"33") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"33") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"33") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "1")))

#### PLUS 2 ####
nc_file <- file.path(sim_folder, 'plus2.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"33") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"33") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"33") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,33,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"33") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"33") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"33") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"33") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"33") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "2")))

#### PLUS 3 ####
nc_file <- file.path(sim_folder, 'plus3.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"33") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"33") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"33") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,33,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"33") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"33") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"33") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"33") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"33") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "3")))


#### PLUS 4 ####
nc_file <- file.path(sim_folder, 'plus4.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"33") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"33") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"33") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,33,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"33") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"33") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"33") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"33") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"33") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "4")))

#### PLUS 5 ####
nc_file <- file.path(sim_folder, 'plus5.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"33") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"33") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"33") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,33,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"33") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"33") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"33") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"33") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"33") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "5")))

#### PLUS 6 ####
nc_file <- file.path(sim_folder, 'plus6.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
Vol <- get_var(nc_file, "Tot_V")
Temp <- get_temp(file=nc_file,reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("temp_")), funs(sub("temp_", "", .))) %>%
  gather(depth, Temp, "0":"33")
DO <- get_var(file=nc_file, var_name="OXY_oxy", reference='surface', z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("OXY_oxy_")), funs(sub("OXY_oxy_", "", .))) %>%
  gather(depth, DO_mgL, "0":"33") %>%
  mutate(DO_mgL = DO_mgL * (32/1000)) 
TP <- get_var(file=nc_file, var_name="TOT_tp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tp_")), funs(sub("TOT_tp_", "", .))) %>%
  gather(depth, TP_mgL, "0":"33") %>%
  mutate(TP_mgL = TP_mgL * (30.97/1000))
FRP <- get_var(file=nc_file, var_name="PHS_frp", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("PHS_frp_")), funs(sub("PHS_frp_", "", .))) %>%
  gather(depth, FRP_mgL, "0":"33") %>%
  mutate(FRP_mgL = FRP_mgL * (30.97/1000))
FRP_ads <- get_var(file=nc_file, var_name="PHS_frp_ads", reference='surface',z_out=seq(0,33,1))%>%
  rename_at(vars(starts_with("PHS_frp_ads_")), funs(sub("PHS_frp_ads_", "", .))) %>%
  gather(depth, FRP_ads_mgL, "0":"33") %>%
  mutate(FRP_ads_mgL = FRP_ads_mgL * (30.97/1000)) 
DOP <- get_var(file=nc_file, var_name="OGM_dop", reference='surface',z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_dop_")), funs(sub("OGM_dop_", "", .))) %>%
  gather(depth, DOP_mgL, "0":"33") %>%
  mutate(DOP_mgL = DOP_mgL * (30.97/1000))
POP <- get_var(file=nc_file, var_name="OGM_pop", reference='surface',z_out=seq(0,33,1))  %>%
  rename_at(vars(starts_with("OGM_pop_")), funs(sub("OGM_pop_", "", .))) %>%
  gather(depth, POP_mgL, "0":"33") %>%
  mutate(POP_mgL = POP_mgL * (30.97/1000))
TN <- get_var(file=nc_file, var_name="TOT_tn", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("TOT_tn_")), funs(sub("TOT_tn_", "", .))) %>%
  gather(depth, TN_mgL, "0":"33") %>%
  mutate(TN_mgL = TN_mgL * (14/1000))
AMM <- get_var(file=nc_file, var_name="NIT_amm", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_amm_")), funs(sub("NIT_amm_", "", .))) %>%
  gather(depth, AMM_mgL, "0":"33") %>%
  mutate(AMM_mgL = AMM_mgL * (14/1000))
NIT <- get_var(file=nc_file, var_name="NIT_nit", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("NIT_nit_")), funs(sub("NIT_nit_", "", .))) %>%
  gather(depth, NIT_mgL, "0":"33") %>%
  mutate(NIT_mgL = NIT_mgL * (14/1000))
DON <- get_var(file=nc_file, var_name="OGM_don", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_don_")), funs(sub("OGM_don_", "", .))) %>%
  gather(depth, DON_mgL, "0":"33") %>%
  mutate(DON_mgL = DON_mgL * (14/1000))
PON <- get_var(file=nc_file, var_name="OGM_pon", reference='surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("OGM_pon_")), funs(sub("OGM_pon_", "", .))) %>%
  gather(depth, PON_mgL, "0":"33") %>%
  mutate(PON_mgL = PON_mgL * (14/1000))

output <- bind_rows(output, (left_join(Temp, DO) %>% left_join(., TP) %>% 
                               left_join(.,FRP) %>% left_join(.,FRP_ads) %>% left_join(.,DOP) %>% left_join(.,POP) %>%
                               left_join(.,TN) %>% left_join(.,AMM) %>% left_join(.,NIT) %>% left_join(.,DON) %>% 
                               left_join(.,PON) %>% 
                               mutate(Sim = "6"))) %>% 
  mutate(Lake = "Sunapee")

write_csv(output, paste("./output/Sunapee_20AugAll_", format(Sys.Date(), "%Y%m%d"),'.csv', sep=""), append=F)
