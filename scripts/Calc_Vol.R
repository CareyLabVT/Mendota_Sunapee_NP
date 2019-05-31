pacman::p_load(glmtools, tidyverse)

##### MENDOTA #####
sim_folder  <-  ('./Mendota/GLM')
nml <- read_nml(paste0(sim_folder, "/glm2.nml"))
baseline_nc <- file.path('./Mendota/GLM/Mendota_GRAPLE/20190213_ValDay', 'baseline.nc')

surface_area = nml$morphometry$A[30]
volume <- get_var(baseline_nc, "Tot_V")
volume = median(volume$Tot_V)
h = nml$init_profiles$lake_depth

# Calculate lake as a cone
# V = (1/3)pi r^2 h --> r = sqrt(V/h/pi*3)
r = sqrt(volume / h / 3.14159*3)

# slant height (l): r2 + h2 = l2
l = sqrt((r^2) + (h^2))

# lateral surface area = pi * r * l
sed_area = 3.14159 * r * l  # 56,502,822 m2

sed_area / surface_area     # 1.417319

# Calculate Mendota hypo depth (20m +) ####
hypso <- as.data.frame(nml$morphometry$H) %>%
  bind_cols(as.data.frame(nml$morphometry$A)) %>% 
  rename(H = "nml$morphometry$H", A = "nml$morphometry$A") %>%
  mutate(D = 259.8 - H)

HypoVol_Mendota <- ((1/3)*(7777000 + 5957000 + sqrt(7777000 * 5957000)) * (20.9 - 20.1) + 
                    (1/3)*(5957000 + 4040000 + sqrt(5957000 * 4040000)) * (21.8 - 20.9) +
                    (1/3)*(4040000 + 2560000 + sqrt(4040000 * 2560000)) * (22.7 - 21.8) + 
                    (1/3)*(2560000 + 821000  + sqrt(2560000 * 821000))  * (23.6 - 22.7) + 
                    (1/3)*(821000  + 216000  + sqrt(821000 * 216000))   * (24.4 - 23.6) +
                    (1/3)*(216000  + 0       + sqrt(216000 * 0))        * (25.3 - 24.4))

HypoVol2_Mendota <- ((1/3)*(2560000 + 821000  + sqrt(2560000 * 821000))  * (23.6 - 22.7) + 
                     (1/3)*(821000  + 216000  + sqrt(821000 * 216000))   * (24.4 - 23.6) +
                     (1/3)*(216000  + 0       + sqrt(216000 * 0))        * (25.3 - 24.4))

# 14795913 volume m3 from 20.1 m to bottom
# 7777000 area m2 at 20.1 m depth

# 1902853 volume m3 from 22.7 m to bottom

# Denitrification conversion factor for mmol N/m3/d to mmol/m2/d
14795913 / 7777000 # 1.902522

#### SUNAPEE ####
sim_folder  <-  ('./Sunapee/GLM')
nml <- read_nml(paste0(sim_folder, "/glm2.nml"))
baseline_nc <- file.path('./Sunapee/GLM/Sunapee_GRAPLE/20190225', 'baseline.nc')

surface_area = nml$morphometry$A[30]
volume <- get_var(baseline_nc, "Tot_V")
volume = median(volume$Tot_V)
h = nml$init_profiles$lake_depth

# Calculate lake as a cone
# V = (1/3)pi r^2 h --> r = sqrt(V/h/pi*3)
r = sqrt(volume / h / 3.14159*3)

# slant height (l): r2 + h2 = l2
l = sqrt((r^2) + (h^2))

# lateral surface area = pi * r * l
sed_area = 3.14159 * r * l  # 16,879,430 m2

sed_area / surface_area     # 7.552616 (Sunapee is a deeper cone than Mendota)

# Calculate Sunapee hypo depth (20m +) ####
sim_folder  <-  ('./Sunapee/GLM')
nml <- read_nml(paste0(sim_folder, "/glm2.nml"))

hypso <- as.data.frame(nml$morphometry$H) %>%
  bind_cols(as.data.frame(nml$morphometry$A)) %>% 
  rename(H = "nml$morphometry$H", A = "nml$morphometry$A") %>%
  mutate(D = 333.943 - H) %>% filter(D >= 20)

HypoVol_Sunapee <- ((1/3)*(2.234912e+06 + 1.988527e+06 + sqrt(2.234912e+06 * 1.988527e+06)) * (20.5 - 20) +
                    (1/3)*(1.988527e+06 + 1.740821e+06 + sqrt(1.988527e+06 * 1.740821e+06)) * (21 - 20.5) + 
                    (1/3)*(1.740821e+06 + 1.505854e+06 + sqrt(1.740821e+06 * 1.505854e+06)) * (21.5 - 21) +
                    (1/3)*(1.505854e+06 + 1.283337e+06 + sqrt(1.505854e+06 * 1.283337e+06)) * (22 - 21.5) + 
                    (1/3)*(1.283337e+06 + 1.088627e+06 + sqrt(1.283337e+06 * 1.088627e+06)) * (22.5 - 22) + 
                    (1/3)*(1.088627e+06 + 9.257047e+05 + sqrt(1.088627e+06 * 9.257047e+05)) * (23 - 22.5) +
                    (1/3)*(9.257047e+05 + 7.974677e+05 + sqrt(9.257047e+05 * 7.974677e+05)) * (23.5 - 23) +
                    (1/3)*(7.974677e+05 + 6.769300e+05 + sqrt(7.974677e+05 * 6.769300e+05)) * (24 - 23.5) +
                    (1/3)*(6.769300e+05 + 5.759476e+05 + sqrt(6.769300e+05 * 5.759476e+05)) * (24.5 - 24) +
                    (1/3)*(5.759476e+05 + 4.835282e+05 + sqrt(5.759476e+05 * 4.835282e+05)) * (25 - 24.5) +
                    (1/3)*(4.835282e+05 + 4.012220e+05 + sqrt(4.835282e+05 * 4.012220e+05)) * (25.5 - 25) +
                    (1/3)*(4.012220e+05 + 3.271121e+05 + sqrt(4.012220e+05 * 3.271121e+05)) * (26 - 25.5) +
                    (1/3)*(3.271121e+05 + 2.691378e+05 + sqrt(3.271121e+05 * 2.691378e+05)) * (26.5 - 26) +
                    (1/3)*(2.691378e+05 + 2.148366e+05 + sqrt(2.691378e+05 * 2.148366e+05)) * (27 - 26.5) +
                    (1/3)*(2.148366e+05 + 1.685762e+05 + sqrt(2.148366e+05 * 1.685762e+05)) * (27.5 - 27) +
                    (1/3)*(1.685762e+05 + 1.339418e+05 + sqrt(1.685762e+05 * 1.339418e+05)) * (28 - 27.5) +
                    (1/3)*(1.339418e+05 + 1.050375e+05 + sqrt(1.339418e+05 * 1.050375e+05)) * (28.5 - 28) +
                    (1/3)*(1.050375e+05 + 7.903042e+04 + sqrt(1.050375e+05 * 7.903042e+04)) * (29 - 28.5) +
                    (1/3)*(7.903042e+04 + 6.273414e+04 + sqrt(7.903042e+04 * 6.273414e+04)) * (29.5 - 29) +
                    (1/3)*(6.273414e+04 + 5.185362e+04 + sqrt(6.273414e+04 * 5.185362e+04)) * (30 - 29.5) +
                    (1/3)*(5.185362e+04 + 4.155119e+04 + sqrt(5.185362e+04 * 4.155119e+04)) * (30.5 - 30) +
                    (1/3)*(4.155119e+04 + 3.320782e+04 + sqrt(4.155119e+04 * 3.320782e+04)) * (31 - 30.5) +
                    (1/3)*(3.320782e+04 + 2.520589e+04 + sqrt(3.320782e+04 * 2.520589e+04)) * (31.5 - 31) +
                    (1/3)*(2.520589e+04 + 7.254808e+03 + sqrt(2.520589e+04 * 7.254808e+03)) * (32 - 31.5) +
                    (1/3)*(7.254808e+03 + 1.588891e+02 + sqrt(7.254808e+03 * 1.588891e+02)) * (32.5 - 32) +
                    (1/3)*(1.588891e+02 + 6.423176e+01 + sqrt(1.588891e+02 * 6.423176e+01)) * (33 - 32.5) +
                    (1/3)*(6.423176e+01 + 3.887712e+01 + sqrt(6.423176e+01 * 3.887712e+01)) * (33.5 - 33) +
                    (1/3)*(3.887712e+01 + 1.690309e+01 + sqrt(3.887712e+01 * 1.690309e+01)) * (34 - 33.5) +
                    (1/3)*(1.690309e+01 + 1.000000e+00 + sqrt(1.690309e+01 * 1.000000e+00)) * (34.513 - 34))
                            

print(HypoVol_Sunapee)
# 7044181 volume m3 from 20 m to bottom
# 2.234912e+06 area at 20m
7044181/2.234912e+06 # 3.151883 multiplier

