# Pull in stability metrics from Matlab, save as csv ####
pacman::p_load(tidyverse)

# Load files from Matlab ####
matlab_path <- 'C:/Users/KJF/Dropbox/MATLAB/Lake-Analyzer-master'

stability <- full_join((file.path(matlab_path, 'Mendota_baseline/Mendota_results.txt') %>% 
                          read_delim(delim= "\t") %>%
                          mutate(Lake = "Mendota", Sim = "0")),
                       (file.path(matlab_path, 'Mendota_plus1/Mendota_results.txt') %>% 
                          read_delim(delim= "\t") %>%
                          mutate(Lake = "Mendota", Sim = "1"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_plus2/Mendota_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "2"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_plus3/Mendota_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "3"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_plus4/Mendota_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "4"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_plus5/Mendota_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "5"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_plus6/Mendota_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "6"))) %>%
  #Sunapee
  full_join(., (file.path(matlab_path, 'Sunapee_baseline/Sunapee_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "0"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_plus1/Sunapee_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "1"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_plus2/Sunapee_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "2"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_plus3/Sunapee_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "3"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_plus4/Sunapee_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "4"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_plus5/Sunapee_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "5"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_plus6/Sunapee_results.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "6"))) %>%
 write_csv('./output/stability_all.csv')
