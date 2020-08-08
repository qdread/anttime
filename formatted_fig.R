# Publication formatted figures

# Must run all code in ant_temporal_analysis_v2.Rmd to generate data for figures.
# Plotting data object is called dat.

cond_eff_plot_with_pies <- function(fit, v, yvar, adj, size_factor, x_title, y_title) {
  yvarsym <- sym(yvar)
  pdat <- conditional_effects(fit)
  var_names <- c('colley_ranking', 'ctmax')
  p <- ggplot(pdat[[v]], aes_string(x = var_names[v])) +
    geom_line(aes(y = estimate__)) +
    geom_line(aes(y = lower__), linetype = 'dashed') +
    geom_line(aes( y = upper__), linetype = 'dashed') +
    geom_point(data = dat, aes(y = !!yvarsym), alpha = 0) +
    geom_text(data = dat, aes(y = !!yvarsym - adj, label = sp), color = 'gray50') +
    labs(x = x_title, y = y_title) +
    theme(legend.position = 'none')
  
  dat$xoffset <- size_factor * diff(range(dat[, var_names[v]])) 
  dat$yoffset <- size_factor * diff(range(dat[, yvar]))
  
  for (i in 1:nrow(dat)) {
    p <- p +
      annotation_custom(ggplotGrob(dat$plot[[i]]), 
                        xmin = as.numeric(dat[i, var_names[v]] - dat$xoffset[i]), 
                        xmax = as.numeric(dat[i, var_names[v]] + dat$xoffset[i]),
                        ymin = as.numeric(dat[i, yvar] - dat$yoffset[i]), 
                        ymax = as.numeric(dat[i, yvar] + dat$yoffset[i]))
  }
  return(p)
  
}


p1 <- cond_eff_plot_with_pies(mod_emd, 2, 'mantel_stat', adj = 0.01, size_factor = 0.04, parse(text = 'CT[max]~(degree*C)'), 'Foraging time responsiveness\nto increased temperature')
p2 <- cond_eff_plot_with_pies(mod_emd, 1, 'mantel_stat', adj = 0.01, size_factor = 0.04, 'Dominance score', 'Foraging time responsiveness\nto increased temperature')

p1 <- p1 + theme_bw() + theme(panel.grid = element_blank())
p2 <- p2 + theme_bw() + theme(panel.grid = element_blank(),
                              axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Save plots so session can be restarted
save(p1, p2, file = '~/Documents/temp/pieplots.RData')


# Restart session here ----------------------------------------------------

load('~/Documents/temp/pieplots.RData')
library(tidyverse)


# Interesting way to line up plots. Answer was given at 
# https://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot
library(grid)

png('figs/conditional_plot_with_pies_formatted.png', res = 400, units = 'in', height = 4, width = 6)
grid.newpage()
grid.draw(cbind(ggplotGrob(p1 + scale_x_continuous(limits = c(40.3, 46.3), expand = c(0,0), breaks = 41:46) +
                             annotate(geom = 'text', x = -Inf, y = Inf, label = 'a', hjust = -0.2, vjust = 1.2, size = 5)), 
                ggplotGrob(p2 + scale_x_continuous(limits = c(0.3, 0.92), expand = c(0,0), breaks = (4:9)/10) +
                             annotate(geom = 'text', x = -Inf, y = Inf, label = 'b', hjust = -0.2, vjust = 1.2, size = 5))))
dev.off()


# Four ridge plots on one fig ---------------------------------------------

# FIXME the error bars on the median points extend past the bin plot (ugly)
# FIXME the left and right sides aren't lined up
# FIXME the extreme values on panels b,c,and d are causing it to be very hard to see most of the data

dat_common_toplot <- dat_common_toplot %>%
  mutate(temp_label = paste0('+', temperature, '*degree*C'))

temp_label_table <- unique(dat_common_toplot %>% ungroup %>% filter(site == 'Duke', !is.na(temperature)) %>% select(chamber_temp, temp_label)) %>%
  arrange(chamber_temp)
y_labels <- temp_label_table$temp_label

combined_season_plot <- function(site_to_plot, sp_to_plot, season_to_plot) {
  plotdat <- dat_common_toplot %>% filter(site == site_to_plot, sp == sp_to_plot, season %in% season_to_plot)
  plotstats <- summary_stats_combineseasons %>% filter(site == site_to_plot, sp == sp_to_plot)
  ggplot(plotdat, 
         aes(x = time, y = chamber_temp, fill = temperature)) +
    geom_density_ridges(aes(y = chamber_temp), alpha = 0.7, color = 'gray50', bins = 24, stat = 'binline', scale = 2) +
    geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = plotstats, size = 1.5) +
    geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = plotstats, size = 1.5) +
    geom_point(aes(x = median_time), data = plotstats, size = 2, fill = 'gray50', shape = 21, stroke = 2) +
    geom_text(aes(label = paste('n = ', n)), x = 0, data = plotstats, hjust = 0, vjust = -1, fontface = 'italic') +
    scale_x_continuous(limits=c(0,23)) +
    scale_y_discrete(expand = c(0,0), labels = parse(text = y_labels)) +
    temp_fill_palette +
    coord_cartesian(clip = 'off') +
    theme_ridges(grid = FALSE) +
    theme(legend.position = 'none', 
          axis.title.y = element_blank())
}

species_long_names <- spp$species[match(sp_descend, spp$sp)]
duke_bothseasons_plots <- map2(sp_descend, species_long_names, ~ combined_season_plot("Duke", .x, c("summer", "winter")) + ggtitle(.y))

title_size <- 12

png('figs/four_panel_ridge_plots.png', res = 400, units = 'in', height = 8, width = 6)
grid.newpage()
grid.draw(rbind(
  cbind(ggplotGrob(duke_bothseasons_plots[[1]] +
                     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
                           plot.title = element_text(size = title_size)) +
                     annotate(geom = 'text', x = Inf, y = Inf, label = 'a', hjust = 0.2, vjust = 1.2, size = 5)), 
        ggplotGrob(duke_bothseasons_plots[[2]] + 
                     theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.text.y = element_blank(),
                           plot.title = element_text(size = title_size)) +
                     annotate(geom = 'text', x = Inf, y = Inf, label = 'b', hjust = 0.2, vjust = 1.2, size = 5))),
  cbind(ggplotGrob(duke_bothseasons_plots[[3]] +
                     theme(plot.title = element_text(size = title_size)) +
                     annotate(geom = 'text', x = Inf, y = Inf, label = 'c', hjust = 0.2, vjust = 1.2, size = 5)), 
        ggplotGrob(duke_bothseasons_plots[[5]] + 
                     theme(axis.text.y = element_blank(),
                           plot.title = element_text(size = title_size)) +
                     annotate(geom = 'text', x = Inf, y = Inf, label = 'd', hjust = 0.2, vjust = 1.2, size = 5)))))
dev.off()


# Faceted ridge plot ------------------------------------------------------

# Alternative to laboriously lining up the plots is the faceted ridge plot.

spp_to_plot <- sp_descend[c(1,2,3,5)]

plotdat <- dat_common_toplot %>% filter(site == 'Duke', sp %in% spp_to_plot)
plotstats <- summary_stats_combineseasons %>% filter(site == 'Duke', sp %in% spp_to_plot)
p_facetridge <- ggplot(plotdat, 
       aes(x = time, y = chamber_temp, fill = temperature)) +
  facet_wrap(~ sp, nrow = 2, labeller = labeller(sp = c(apru = 'Aphaenogaster rudis', cape = 'Camponotus pennsylvanicus', crli = 'Crematogaster lineolata', prim = 'Prenolepis imparis'))) +
  geom_density_ridges(aes(y = chamber_temp, height = stat(density)), alpha = 0.7, color = 'gray50', bins = 24, stat = 'binline', scale = 1) +
  geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = plotstats, size = 1.5) +
  geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = plotstats, size = 1.5) +
  geom_point(aes(x = median_time), data = plotstats, size = 2, fill = 'gray50', shape = 21, stroke = 2) +
  geom_text(aes(label = paste('n = ', n)), x = 0, data = plotstats, hjust = 0, vjust = -1, fontface = 'italic') +
  scale_x_continuous(limits=c(0,23), expand = c(0,0), name = 'time (h)') +
  scale_y_discrete(labels = parse(text = y_labels)) +
  temp_fill_palette +
  coord_cartesian(clip = 'off') +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(),
        strip.background = element_blank())



ggsave('figs/facet_ridgeplot.png', p_facetridge, dpi = 400, height = 8, width = 6)

# Manually calculate heights first
plotdat_sums <- plotdat %>% 
  group_by(sp, temperature, chamber_temp, temp_label) %>%
  group_modify(~ data.frame(time = 0:23, n = sapply(0:23, function(x) sum(.$time == x))))
p_facetridge_sums <- ggplot(plotdat_sums, 
                       aes(x = time, y = chamber_temp)) +
  facet_wrap(~ sp, nrow = 2, labeller = labeller(sp = c(apru = 'Aphaenogaster rudis', cape = 'Camponotus pennsylvanicus', crli = 'Crematogaster lineolata', prim = 'Prenolepis imparis'))) +
  geom_ridgeline(aes(y = chamber_temp, height = n), alpha = 0.7, color = 'gray50') +
  geom_segment(aes(x = seg1_start, xend = seg1_end, yend = chamber_temp), data = plotstats, size = 1.5) +
  geom_segment(aes(x = seg2_start, xend = seg2_end, yend = chamber_temp), data = plotstats, size = 1.5) +
  geom_point(aes(x = median_time), data = plotstats, size = 2, fill = 'gray50', shape = 21, stroke = 2) +
  geom_text(aes(label = paste('n = ', n)), x = 0, data = plotstats, hjust = 0, vjust = -1, fontface = 'italic') +
  scale_x_continuous(limits=c(0,23), name = 'time (h)') +
  scale_y_discrete(expand = c(0,0), labels = parse(text = y_labels)) +
  temp_fill_palette +
  coord_cartesian(xlim = c(0, 23), clip = 'off') +
  theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
  theme(legend.position = 'none', 
        axis.title.y = element_blank(),
        strip.background = element_blank())



# Histograms faceted ------------------------------------------------------

temp_label_ordered <- plotdat$temp_label[match(unique(plotdat$chamber_temp), plotdat$chamber_temp)]
chamber_temp_ordered <- plotdat$chamber_temp[match(unique(plotdat$chamber_temp), plotdat$chamber_temp)]

temp_label_named <- setNames(temp_label_ordered, chamber_temp_ordered)

plotdat <- plotdat %>%
  mutate(chamber_temp = factor(chamber_temp, levels = rev(levels(chamber_temp))))
plotstats <- plotstats %>%
  mutate(chamber_temp = factor(chamber_temp, levels = rev(levels(chamber_temp))))

histogram_plot <- function(sp_name, sp_title) {
stat_df <- plotstats %>% filter(sp == sp_name) %>%
  mutate(seg1_start = if_else(seg1_start == 0, -0.5, seg1_start),
         seg2_end = if_else(seg2_end == 23, 23.5, seg2_end))

temp_label_named <- gsub("*degree*", "°", temp_label_named, fixed = TRUE)

ggplot(plotdat %>% filter(sp == sp_name), aes(x = time, fill = temperature)) +
  facet_wrap(~ chamber_temp, ncol = 1, scales = 'free_y', strip.position = 'right', labeller = labeller(chamber_temp = temp_label_named)) +
  geom_histogram(bins = 24) +
  geom_segment(aes(x = seg1_start, xend = seg1_end, yend = 0, y = 0), data = stat_df, size = 1.5) +
  geom_segment(aes(x = seg2_start, xend = seg2_end, yend = 0, y = 0), data = stat_df, size = 1.5) +
  geom_point(aes(x = median_time, y = 0), data = stat_df, size = 2, fill = 'gray50', shape = 21, stroke = 2) +
  coord_cartesian(xlim = c(-0.5,23.5), clip = 'off') +
  temp_fill_palette +
  scale_x_continuous(name = "time (h)", expand = c(0, 0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(2), expand = expand_scale(mult = c(0, 0.1))) +
  theme_bw() +
  theme(legend.position = 'none', strip.background = element_blank(), strip.text.y.right = element_text(angle = 0),
        panel.grid = element_blank(), plot.title = element_text(face = 1)) +
  ggtitle(sp_title)

}

spp_long_names <- spp$species[match(spp_to_plot, spp$sp) ]

paste0('(', letters[1:4], ') ', spp_long_names)

histos <- map2(spp_to_plot, paste0('(', letters[1:4], ') ', spp_long_names), histogram_plot)
title_size <- 12
png('figs/four_panel_histogram_plots.png', res = 400, units = 'in', height = 8, width = 6)
grid.newpage()
grid.draw(rbind(
  cbind(ggplotGrob(histos[[1]] +
                     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
                           strip.text.y.right = element_blank(),
                           plot.title = element_text(size = title_size))), 
        ggplotGrob(histos[[2]] + 
                     theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(size = title_size)))),
  cbind(ggplotGrob(histos[[3]] +
                     theme(strip.text.y.right = element_blank(),
                           plot.title = element_text(size = title_size))), 
        ggplotGrob(histos[[4]] + 
                     theme(axis.title.y = element_blank(),
                           plot.title = element_text(size = title_size))))))
dev.off()