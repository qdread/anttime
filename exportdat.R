# Export some data for the O-stats package example

dat_exp <- dat_common %>%
  filter(site == 'Duke', chamber %in% c(2,3), season == 'summer')

dat_exp_common <- dat_exp %>%
  group_by(sp) %>%
  mutate(n = n()) %>%
  filter(n >= 100)

# Overlap plot

ggplot(dat_exp_common, aes(x = time, group = sp, fill = sp)) +
  geom_bar(stat = 'count', position = 'dodge') +
  facet_wrap(~ chamber)
# Lookin' good, dogg

# Get rid of the identifying information about Dook, and add some real species names
spp_lookup <- read_csv('data/species_lookup.csv')

dat_exp_common <- dat_exp_common %>%
  ungroup %>%
  left_join(spp_lookup) %>%
  select(species, chamber, time) %>%
  arrange(chamber, time, species)

write_csv(dat_exp_common %>% mutate(chamber  =chamber-1), '~/Documents/temp/ant_observation_times_by_chamber.csv')
