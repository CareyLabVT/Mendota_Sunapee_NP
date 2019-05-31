# Climate data for each lake from MACA ####
pacman::p_load(lubridate, tidyverse)

lakes <- bind_rows((bind_rows((read_csv('./climate_data/data_mad_historical.csv',skip=8) %>% 
                                 mutate(scenario = 'hist') %>%  
                                 rename(tmax_k = 'tasmax_GFDL-ESM2M_historical(K)')), 
                              (read_csv('./climate_data/data_mad_4.5.csv',skip=8) %>% 
                                 mutate(scenario = 'RCP4.5') %>%  
                                 rename(tmax_k = 'tasmax_GFDL-ESM2M_rcp45(K)'))) %>% 
                      bind_rows(., (read_csv('./climate_data/data_mad_8.5.csv',skip=8) %>% 
                                      mutate(scenario = 'RCP8.5') %>%  
                                      rename(tmax_k = 'tasmax_GFDL-ESM2M_rcp85(K)'))) %>% 
                      mutate(Lake = 'Mendota')), 
                   (bind_rows((read_csv('./climate_data/data_sun_historical.csv',skip=8) %>% 
                                 mutate(scenario = 'hist') %>%  
                                 rename(tmax_k = 'tasmax_GFDL-ESM2M_historical(K)')), 
                              (read_csv('./climate_data/data_sun_4.5.csv',skip=8) %>% 
                                 mutate(scenario = 'RCP4.5') %>%  
                                 rename(tmax_k = 'tasmax_GFDL-ESM2M_rcp45(K)'))) %>% 
                      bind_rows(., (read_csv('./climate_data/data_sun_8.5.csv',skip=8) %>% 
                                      mutate(scenario = 'RCP8.5') %>%  
                                      rename(tmax_k = 'tasmax_GFDL-ESM2M_rcp85(K)'))) %>% 
                      mutate(Lake = 'Sunapee'))) %>% 
  rename(Date = `yyyy-mm-dd`) %>% 
  mutate(tmax_C = tmax_k - 273.15, year = year(Date)) %>% 
  select(Lake, year, Date, scenario, tmax_C)

# Yearly mean temps for historical (1950-2005) ####
hist <- lakes %>% filter(scenario == 'hist')

hist_mean <- hist %>% 
  filter(year <=1980) %>% 
  group_by(Lake, year) %>% 
  summarize(yearly = mean(tmax_C))

ggplot(hist_mean, aes(x = year, y = yearly)) +
  geom_point() + geom_smooth(method='lm') + 
  facet_grid(.~Lake, scales='free_y')

lakes %>% filter(scenario == 'hist') %>% 
  mutate(month = month(Date)) %>% 
  filter(year <=1980) %>% 
  group_by(Lake, scenario) %>% 
  summarize(mean = mean(tmax_C))

# MACA projections ####
proj <- lakes %>% filter(scenario != 'hist')

proj_mean <- proj %>% 
  group_by(Lake, year, scenario) %>% 
  summarize(yearly = mean(tmax_C))

ggplot(proj_mean, aes(x = year, y = yearly, col=scenario)) +
  geom_point() + geom_smooth() + 
  facet_grid(.~Lake, scales='free_y') 

lakes %>% filter(scenario != 'hist') %>% 
  mutate(month = month(Date)) %>% 
  filter(year ==2099) %>% 
  group_by(Lake, scenario) %>% 
  summarize(mean = mean(tmax_C))
