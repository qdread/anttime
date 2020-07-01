# Cool looking figures to show ant trends
# QDR 01 Jul 2020 -- later convert this to a Rmd.

# Setup -------------------------------------------------------------------

source('load_wrangle_data.R')
library(ggridges) # For ridgeline plot


# Define seasons
dat_common <- dat_common %>%
  mutate(season = if_else(month %in% month.name[4:9], 'summer', 'winter')) 

# Add dummy data so all panels will plot
dummy_data <- dat_common %>%
  group_by(site,season,chamber_temp,sp) %>%
  slice(1) %>%
  mutate(time = -1) %>% 
  ungroup

sp_season <- expand_grid(sp = sp_descend, season = c('summer','winter'))
dummy_data <- dat_common %>% 
  ungroup %>% 
  select(site, chamber_temp) %>% 
  unique %>%
  group_by(site, chamber_temp) %>% group_modify(~ data.frame(sp_season))


dat_common_toplot <- bind_rows(dat_common, dummy_data)


# Calc circ means and CI to superimpose -----------------------------------

# Make sure not to use the one with dummy data included.

summary_stats <- dat_common %>%
  group_by(site, sp, chamber, temperature, season, chamber_temp) %>%
  summarize(mean_time = mean(circular(time, units = 'hours', modulo = '2pi')),
            q05_time = quantile(circular(time, units = 'hours', modulo = '2pi'), probs = 0.05),
            q25_time = quantile(circular(time, units = 'hours', modulo = '2pi'), probs = 0.25),
            median_time = median(circular(time, units = 'hours', modulo = '2pi')),
            q75_time = quantile(circular(time, units = 'hours', modulo = '2pi'), probs = 0.75),
            q95_time = quantile(circular(time, units = 'hours', modulo = '2pi'), probs = 0.95)
            )

# Include code to split up the segment into multiple chunks if it goes across midnight.
# For the 25 to 75 quantile segment, this will be true if q25 > q75.

summary_stats <- summary_stats %>%
  mutate(seg1_start = if_else(q25_time > q75_time, 0, as.numeric(q25_time)),
         seg1_end = if_else(q25_time > q75_time, as.numeric(q75_time), as.numeric(q75_time)),
         seg2_start = if_else(q25_time > q75_time, as.numeric(q25_time), as.numeric(NA)),
         seg2_end = if_else(q25_time > q75_time, 23, as.numeric(NA)))

### Summary stats not split by season.
summary_stats_combineseasons <- dat_common %>%
  group_by(site, sp, chamber, temperature,  chamber_temp) %>%
  summarize(mean_time = mean(circular(time, units = 'hours', modulo = '2pi')),
            q05_time = quantile(circular(time, units = 'hours', modulo = '2pi'), probs = 0.05),
            q25_time = quantile(circular(time, units = 'hours', modulo = '2pi'), probs = 0.25),
            median_time = median(circular(time, units = 'hours', modulo = '2pi')),
            q75_time = quantile(circular(time, units = 'hours', modulo = '2pi'), probs = 0.75),
            q95_time = quantile(circular(time, units = 'hours', modulo = '2pi'), probs = 0.95)
  ) %>%
  mutate(seg1_start = if_else(q25_time > q75_time, 0, as.numeric(q25_time)),
         seg1_end = if_else(q25_time > q75_time, as.numeric(q75_time), as.numeric(q75_time)),
         seg2_start = if_else(q25_time > q75_time, as.numeric(q25_time), as.numeric(NA)),
         seg2_end = if_else(q25_time > q75_time, 23, as.numeric(NA)))



# Create stacked up histograms --------------------------------------------



temp_fill_palette <- scale_fill_distiller(palette = 'RdYlBu')

# Summer only
ggplot(dat_common_toplot %>% filter(site == 'Duke', sp == 'prim', season %in% 'summer'), 
       aes(x = time, y= chamber_temp, fill = temperature)) +
  geom_density_ridges(aes(y = chamber_temp), alpha = 0.7, color = 'gray50', from = 0, to = 23, bandwidth = 1) +
  geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = summary_stats %>% filter(site == 'Duke', sp == 'prim', season %in% 'summer'), size = 1.5) +
  geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = summary_stats %>% filter(site == 'Duke', sp == 'prim', season %in% 'summer'), size = 1.5) +
  geom_point(aes(x = median_time), data = summary_stats %>% filter(site == 'Duke', sp == 'prim', season %in% 'summer'), size = 2, fill = 'gray50', shape = 21, stroke = 2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  temp_fill_palette +
  coord_cartesian(clip = 'off') +
  ggtitle('prenolepis summer') +
  theme_ridges(grid = FALSE) +
  theme(legend.position = 'none', axis.title.y = element_blank())

# Winter only
ggplot(dat_common_toplot %>% filter(site == 'Duke', sp == 'prim', season %in% 'winter'), 
       aes(x = time, y= chamber_temp, fill = temperature)) +
  geom_density_ridges(aes(y = chamber_temp), alpha = 0.7, color = 'gray50', from = 0, to = 23, bandwidth = 1) +
  geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = summary_stats %>% filter(site == 'Duke', sp == 'prim', season %in% 'winter'), size = 1.5) +
  geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = summary_stats %>% filter(site == 'Duke', sp == 'prim', season %in% 'winter'), size = 1.5) +
  geom_point(aes(x = median_time), data = summary_stats %>% filter(site == 'Duke', sp == 'prim', season %in% 'winter'), size = 2, fill = 'gray50', shape = 21, stroke = 2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  temp_fill_palette +
  coord_cartesian(clip = 'off') +
  ggtitle('prenolepis winter') +
  theme_ridges(grid = FALSE) +
  theme(legend.position = 'none', axis.title.y = element_blank())

# Summer and winter on the same plot
ggplot(dat_common_toplot %>% filter(site == 'Duke', sp == 'prim'), 
       aes(x = time, y= chamber_temp, fill = temperature)) +
  geom_density_ridges(aes(y = chamber_temp), alpha = 0.7, color = 'gray50', from = 0, to = 23, bandwidth = 1) +
  geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = summary_stats %>% filter(site == 'Duke', sp == 'prim'), size = 1.5) +
  geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = summary_stats %>% filter(site == 'Duke', sp == 'prim'), size = 1.5) +
  geom_point(aes(x = median_time), data = summary_stats %>% filter(site == 'Duke', sp == 'prim'), size = 2, fill = 'gray50', shape = 21, stroke = 2) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  temp_fill_palette +
  facet_wrap(~ season) +
  coord_cartesian(clip = 'off') +
  theme_ridges(grid = FALSE) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(),
        strip.background = element_blank())



# Write a plot for each species -------------------------------------------

# Do by season 

two_season_plot <- function(site_to_plot, sp_to_plot) {
  plotdat <- dat_common_toplot %>% filter(site == site_to_plot, sp == sp_to_plot)
  plotstats <- summary_stats %>% filter(site == site_to_plot, sp == sp_to_plot)
  ggplot(plotdat, 
         aes(x = time, y= chamber_temp, fill = temperature)) +
    geom_density_ridges(aes(y = chamber_temp), alpha = 0.7, color = 'gray50', from = 0, to = 23, bandwidth = 1) +
    geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = plotstats, size = 1.5) +
    geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = plotstats, size = 1.5) +
    geom_point(aes(x = median_time), data = plotstats, size = 2, fill = 'gray50', shape = 21, stroke = 2) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    temp_fill_palette +
    facet_wrap(~ season) +
    coord_cartesian(clip = 'off') +
    theme_ridges(grid = FALSE) +
    theme(legend.position = 'none', 
          axis.title.y = element_blank(),
          strip.background = element_blank(),
          panel.background = element_rect(colour = 'black', size = 0.75, linetype = 1),
          panel.spacing = unit(2, 'lines'))
}

one_season_plot <- function(site_to_plot, sp_to_plot, season_to_plot) {
  plotdat <- dat_common_toplot %>% filter(site == site_to_plot, sp == sp_to_plot, season %in% season_to_plot)
  plotstats <- summary_stats %>% filter(site == site_to_plot, sp == sp_to_plot, season %in% season_to_plot)
  ggplot(plotdat, 
         aes(x = time, y= chamber_temp, fill = temperature)) +
    geom_density_ridges(aes(y = chamber_temp), alpha = 0.7, color = 'gray50', from = 0, to = 23, bandwidth = 1) +
    geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = plotstats, size = 1.5) +
    geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = plotstats, size = 1.5) +
    geom_point(aes(x = median_time), data = plotstats, size = 2, fill = 'gray50', shape = 21, stroke = 2) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    temp_fill_palette +
    coord_cartesian(clip = 'off') +
    theme_ridges(grid = FALSE) +
    theme(legend.position = 'none', 
          axis.title.y = element_blank())
}

combined_season_plot <- function(site_to_plot, sp_to_plot, season_to_plot) {
  plotdat <- dat_common_toplot %>% filter(site == site_to_plot, sp == sp_to_plot, season %in% season_to_plot)
  plotstats <- summary_stats_combineseasons %>% filter(site == site_to_plot, sp == sp_to_plot)
  ggplot(plotdat, 
         aes(x = time, y= chamber_temp, fill = temperature)) +
    geom_density_ridges(aes(y = chamber_temp), alpha = 0.7, color = 'gray50', from = 0, to = 23, bandwidth = 1) +
    geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = plotstats, size = 1.5) +
    geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = plotstats, size = 1.5) +
    geom_point(aes(x = median_time), data = plotstats, size = 2, fill = 'gray50', shape = 21, stroke = 2) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    temp_fill_palette +
    coord_cartesian(clip = 'off') +
    theme_ridges(grid = FALSE) +
    theme(legend.position = 'none', 
          axis.title.y = element_blank())
}

#duke_plots <- map(sp_descend, ~ two_season_plot("Duke", .))
harvard_spp <- c('cape', 'apru', 'fosu')

duke_summer_plots <- map(sp_descend, ~ one_season_plot("Duke", ., "summer"))
duke_winter_plots <- map(sp_descend, ~ one_season_plot("Duke", ., "winter"))
duke_bothseasons_plots <- map(sp_descend, ~ combined_season_plot("Duke", ., c("summer", "winter")))
harvard_summer_plots <- map(harvard_spp, ~ one_season_plot("Harvard", ., "summer"))

duke_names <- gsub(' ', '_', spp$species[match(sp_descend, spp$sp)])
harvard_names <- gsub(' ', '_', spp$species[match(harvard_spp, spp$sp)])

walk2(duke_names, duke_summer_plots, ~ ggsave(file.path('figs', paste0('Duke_summer_', .x, '.png')), .y, height = 7, width = 5))
walk2(duke_names, duke_winter_plots, ~ ggsave(file.path('figs', paste0('Duke_winter_', .x, '.png')), .y, height = 7, width = 5))
walk2(duke_names, duke_bothseasons_plots, ~ ggsave(file.path('figs', paste0('Duke_bothseasons_', .x, '.png')), .y, height = 7, width = 5))
walk2(harvard_names, harvard_summer_plots, ~ ggsave(file.path('figs', paste0('Harvard_summer_', .x, '.png')), .y, height = 7, width = 5))
     