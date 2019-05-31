# Pull water temps for MatLab thermocline calculation; save in MATLAB folders ####
pacman::p_load(tidyverse, glmtools)

matlab_path <- 'C:/Users/KJF/Dropbox/MATLAB/Lake-Analyzer-master'
Mendota_results <- './Mendota/GLM/Mendota_GRAPLE/GRAPLE_20190418'
Sunapee_results <- './Sunapee/GLM/Sunapee_GRAPLE/GRAPLE_20190411'

# Mendota ####
menbaseline <- file.path(Mendota_results, 'baseline.nc')
get_temp(menbaseline, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Mendota_baseline/Mendota.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

menplus1 <- file.path(Mendota_results, 'plus1.nc')
get_temp(menplus1, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Mendota_plus1/Mendota.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

menplus2 <- file.path(Mendota_results, 'plus2.nc')
get_temp(menplus2, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Mendota_plus2/Mendota.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

menplus3 <- file.path(Mendota_results, 'plus3.nc')
get_temp(menplus3, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Mendota_plus3/Mendota.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

menplus4 <- file.path(Mendota_results, 'plus4.nc')
get_temp(menplus4, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Mendota_plus4/Mendota.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

menplus5 <- file.path(Mendota_results, 'plus5.nc')
get_temp(menplus5, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Mendota_plus5/Mendota.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

menplus6 <- file.path(Mendota_results, 'plus6.nc')
get_temp(menplus6, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Mendota_plus6/Mendota.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

# Sunapee  ####
sunbaseline <- file.path(Sunapee_results, 'baseline.nc')
get_temp(sunbaseline, reference = 'surface', z_out=seq(0,33,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Sunapee_baseline/Sunapee.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

sunplus1 <- file.path(Sunapee_results, 'plus1.nc')
get_temp(sunplus1, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Sunapee_plus1/Sunapee.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

sunplus2 <- file.path(Sunapee_results, 'plus2.nc')
get_temp(sunplus2, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Sunapee_plus2/Sunapee.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

sunplus3 <- file.path(Sunapee_results, 'plus3.nc')
get_temp(sunplus3, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Sunapee_plus3/Sunapee.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

sunplus4 <- file.path(Sunapee_results, 'plus4.nc')
get_temp(sunplus4, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Sunapee_plus4/Sunapee.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

sunplus5 <- file.path(Sunapee_results, 'plus5.nc')
get_temp(sunplus5, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Sunapee_plus5/Sunapee.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")

sunplus6 <- file.path(Sunapee_results, 'plus6.nc')
get_temp(sunplus6, reference = 'surface', z_out=seq(0,24,1)) %>%
  rename_at(vars(starts_with("temp")), funs(sub("temp", "wtr", .))) %>%
  rename(dateTime = DateTime) %>%
  write.table(paste(matlab_path, './Sunapee_plus6/Sunapee.txt', sep=''),
              row.names=FALSE, quote=F, sep="\t")