# Read Lacy's data in and write to CSV(s)
# QDR / anttime / 31 Mar 2020

library(tidyverse)
library(readxl)

thermaltol_raw <- read_xlsx('data/Thermal_tolerance_DF_HF communities.xlsx', sheet = 'Sheet1')

# Separate into individual measurements and summary stats
tt_indiv <- thermaltol_raw %>%
  select(species:CTmin)

rle(tt_indiv$species)$lengths

tt_indiv %>%
  group_by_at(vars(species:habitat_coverage)) %>%
  summarize(mean_CTmax = mean(CTmax),
            mean_CTmin = mean(CTmin)) # This corresponds to the data in the xlsx

tt_stats <- thermaltol_raw %>%
  select(-CTmax, -CTmin) %>%
  filter(!is.na(`Mean Ctmax`))

write_csv(tt_indiv, 'data/thermal_tolerance.csv')
write_csv(tt_stats, 'data/thermal_tolerance_summarystats.csv')

# Make species lookup table with species code

sp_lookup <- tt_indiv %>% 
  select(species) %>% 
  unique %>%
  mutate(sp = map_chr(strsplit(species, ' '), ~ paste(substr(., 1, 2), collapse = '') %>% tolower))

# fosu is duplicated

sp_lookup %>% filter(sp=='fosu')
tt_indiv %>% filter(genus == 'Formica') %>% select(region, species) %>% unique
# The one found in both places is Formica subsericea

sp_lookup %>% 
  filter(!species %in% 'Formica subintegra') %>%
  mutate(species = gsub('pennsylvannicus', 'pennsylvanicus', species)) %>%
  write_csv('data/species_lookup.csv')
