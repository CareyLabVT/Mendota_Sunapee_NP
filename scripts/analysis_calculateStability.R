# Pull in stability metrics from Matlab, save as csv ####
pacman::p_load(tidyverse)

# Load files from Matlab ####
matlab_path <- './output/MATLAB'

stability <- full_join((file.path(matlab_path, 'Mendota_results_baseline.txt') %>% 
                          read_delim(delim= "\t") %>%
                          mutate(Lake = "Mendota", Sim = "0")),
                       (file.path(matlab_path, 'Mendota_results_plus1.txt') %>% 
                          read_delim(delim= "\t") %>%
                          mutate(Lake = "Mendota", Sim = "1"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_results_plus2.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "2"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_results_plus3.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "3"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_results_plus4.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "4"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_results_plus5.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "5"))) %>%
  full_join(., (file.path(matlab_path, 'Mendota_results_plus6.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Mendota", Sim = "6"))) %>%
  #Sunapee
  full_join(., (file.path(matlab_path, 'Sunapee_results_baseline.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "0"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_results_plus1.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "1"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_results_plus2.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "2"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_results_plus3.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "3"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_results_plus4.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "4"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_results_plus5.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "5"))) %>%
  full_join(., (file.path(matlab_path, 'Sunapee_results_plus6.txt') %>% 
                  read_delim(delim= "\t") %>%
                  mutate(Lake = "Sunapee", Sim = "6"))) %>%
 write_csv('./output/stability_all.csv')
