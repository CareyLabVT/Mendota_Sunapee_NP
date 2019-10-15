# Extract N-flux rates from GLM scenarios ####
pacman::p_load(glmtools, tidyverse)

####### MENDOTA ######
sim_folder <- './Mendota/GLM/Mendota_GRAPLE/GRAPLE_20190418'

#### BASELINE ####
nc_file <- file.path(sim_folder, 'baseline.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- left_join(vol, sedpflux) %>% 
  mutate(Sim = "0")

#### PLUS 1 ####
nc_file <- file.path(sim_folder, 'plus1.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "1")))

#### PLUS 2 ####
nc_file <- file.path(sim_folder, 'plus2.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "2")))

#### PLUS 3 ####
nc_file <- file.path(sim_folder, 'plus3.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "3")))

#### PLUS 4 ####
nc_file <- file.path(sim_folder, 'plus4.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "4")))

#### PLUS 5 ####
nc_file <- file.path(sim_folder, 'plus5.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "5")))

#### PLUS 6 ####
nc_file <- file.path(sim_folder, 'plus6.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "6"))) %>% 
  mutate(Lake = "Mendota")

write_csv(output, paste("./output/Mendota_psed_", format(Sys.Date(), "%Y%m%d"),'.csv', sep=""), append=F)

####### SUNAPEE ######
sim_folder <- './Sunapee/GLM/Sunapee_GRAPLE/S_GRAPLE_20190411'

#### BASELINE ####
nc_file <- file.path(sim_folder, 'baseline.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- left_join(vol, sedpflux) %>% 
  mutate(Sim = "0")

#### PLUS 1 ####
nc_file <- file.path(sim_folder, 'plus1.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "1")))

#### PLUS 2 ####
nc_file <- file.path(sim_folder, 'plus2.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "2")))

#### PLUS 3 ####
nc_file <- file.path(sim_folder, 'plus3.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "3")))

#### PLUS 4 ####
nc_file <- file.path(sim_folder, 'plus4.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "4")))

#### PLUS 5 ####
nc_file <- file.path(sim_folder, 'plus5.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "5")))

#### PLUS 6 ####
nc_file <- file.path(sim_folder, 'plus6.nc') #this defines the output.nc file 

# Extract variables of interest from GLM output
vol <- get_var(nc_file, "Tot_V")
sedpflux <- get_var(nc_file, var_name="PHS_sed_frp", ref = 'surface', z_out=c(1))

output <- bind_rows(output, (left_join(vol, sedpflux) %>% 
                               mutate(Sim = "6"))) %>% 
  mutate(Lake = "Sunapee")

write_csv(output, paste("./output/Sunapee_psed_", format(Sys.Date(), "%Y%m%d"),'.csv', sep=""), append=F)
