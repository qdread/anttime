# Ridge plots with axis labels

set.seed(1)
fakedat <- data.frame(x = sample(1:10, size = 100, replace = TRUE),
                      g = factor(letters[1:4], letters[4:1]))

library(ggplot2)

ggplot(fakedat, aes(x = x, y = ..count..)) +
  stat_bin(geom = "col",  breaks = 0:11 - 0.5, fill = "gray70") +
  stat_bin(geom = "step", breaks = 0:13 - 1) +
  facet_grid(g ~ ., switch = "y") +
  theme_classic() +
  scale_y_continuous(position = "right") +
  scale_x_continuous(breaks = 0:4 * 3) +
  theme(strip.background    = element_blank(),
        axis.text.x         = element_text(size = 16),
        axis.line.x         = element_blank(),
        axis.ticks.x        = element_line(colour = "gray90"),
        axis.ticks.length.x = unit(30, "points"),
        strip.text.y.left   = element_text(angle = 0, size = 16),
        panel.grid.major.x  = element_line(colour = "gray90"))

site_to_plot = 'Duke'
sp_to_plot = 'apru'
season_to_plot = 'summer'

plotdat <- dat_common_toplot %>% 
  filter(site == site_to_plot, sp == sp_to_plot, season %in% season_to_plot) %>%
  mutate(chamber_temp = factor(chamber_temp, levels = rev(levels(chamber_temp))))
plotstats <- summary_stats_combineseasons %>% 
  filter(site == site_to_plot, sp == sp_to_plot) %>%
  mutate(chamber_temp = factor(chamber_temp, levels = rev(levels(chamber_temp))))
ggplot(plotdat, 
       aes(x = time, y= ..count.., fill = temperature)) +
  facet_grid(chamber_temp ~ ., switch = 'y', scales = 'free_y') +
  stat_bin(geom = "col",  breaks = 0:23 - 0.5) +
  stat_bin(geom = "step", breaks = 0:23 - 1) +
  geom_segment(aes(x = seg1_start, xend = seg1_end, y = 0, yend = 0), data = plotstats, size = 1.5) +
  geom_segment(aes(x = seg2_start, xend = seg2_end, y = 0, yend = 0), data = plotstats, size = 1.5) +
  geom_point(aes(x = median_time, y = 0), data = plotstats, size = 2, fill = 'gray50', shape = 21, stroke = 2) +
  scale_x_continuous(expand = c(0,0), limits=c(0,23), breaks  = c(0, 6, 12, 18)) +
  scale_y_continuous(position = 'right') +
  temp_fill_palette +
  coord_cartesian(clip = 'off') +
  theme_ridges(grid = FALSE) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(),
        strip.background    = element_blank(),
        axis.text.x         = element_text(size = 16),
        axis.line.x         = element_blank(),
        axis.ticks.x        = element_line(colour = "gray90"),
        axis.ticks.length.x = unit(30, "points"),
        strip.text.y.left   = element_text(angle = 0, size = 16),
        panel.grid.major.x  = element_line(colour = "gray90"))
