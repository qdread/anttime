# Script to load and wrangle data. Sourced in all Rmds.
# 26 June 2020

library(tidyverse)
library(ade4)
library(Ostats)
library(emdist)
library(circular)
library(brms)
library(brmstools)

theme_set(theme_minimal())

trt <- read_csv('data/chamber_treatments.csv')
dat <- read_csv('data/data_allmonths_allsites.csv')
spp <- read_csv('data/species_lookup.csv')

# Create a new column in treatment with the temp and chamber ID
# Order it by the temperature so the plots are in ascending temp order
trt <- trt %>%
  mutate(chamber_temp = paste0('chamber ', chamber, ': +', temperature, 'C')) %>%
  mutate(chamber_temp = factor(chamber_temp, 
                               levels = unique(chamber_temp)[order(site, temperature)]))

# Convert month to ordered factor and date to Date object and join with treatment
# Also correct for DST. We have no obs in March and Nov so we can just subtract 1 from Apr-Oct
# Take modulo 24 so that -1 is converted to 23.
dat <- dat %>%
  filter(!spp %in% c('none','unk'), !is.na(spp)) %>%
  mutate(month = factor(month, levels = month.name),
         date = as.Date(date, format = '%m/%d/%Y'),
         time = if_else(month %in% month.name[4:10], (time - 1) %% 24, time)) %>%
  left_join(trt)

# Additional data wrangling:
# Convert to long form with one row per individual ant observed.
dat_long <- dat %>%
  filter(!is.na(number)) %>%
  group_by(site, month, date, time,  chamber, temperature, chamber_temp) %>%
  group_modify(~ data.frame(sp = rep(.$spp, times = .$number)))

# Filter the data to show only species with at least 100 individuals
# Also define seasons
dat_common <- dat_long %>%
  filter(!is.na(temperature)) %>%
  mutate(season = if_else(month %in% month.name[4:9], 'summer', 'winter')) %>%
  group_by(sp) %>%
  filter(n() >= 100)

# Set color palette for all species
sp_descend <- dat_common %>% summarize(n=n()) %>% arrange(-n) %>% pull(sp)
fill_palette <- scale_fill_manual(values = 
                                    setNames(c(RColorBrewer::brewer.pal(9, 'Set1'), 'turquoise'), sp_descend))
color_palette <- scale_color_manual(values = 
                                      setNames(c(RColorBrewer::brewer.pal(9, 'Set1'), 'turquoise'), sp_descend))
