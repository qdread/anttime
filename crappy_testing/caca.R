cacadat <- mean_times %>% filter(sp == 'caca')

lm.circular(y = cacadat$median_time, x = cacadat$temperature, type = 'c-l', init = 1)

# Random intercept only, no random slope
duke_fit_randomint <- brm(median_time_radians ~ temperature + (1 | sp), 
                            family = von_mises(), 
                            prior = priors,
                            data = mean_times %>% filter(site == 'Duke'),
                            control = list(adapt_delta = 0.9),
                            chains = 2, iter = 7500, warmup = 5000, seed = 33333)



mean_times <- dat_common %>%
  group_by(site, sp, chamber, temperature, chamber_temp) %>%
  summarize(median_time = median(circular(time, units = 'hours', modulo = '2pi')))

faketime <- c(23,23,23,0,0,0,1,1,1,2,2,2)

fakecirc <- circular(faketime, units = 'hours', template = 'clock24', modulo = '2pi', zero = 0)

fakecircmed <- median(fakecirc) %>% as.numeric

fakecircmed * (2*pi/24) - pi

conversion.circular(fakecirc, units = 'radians', zero = pi) %>% as.numeric

as.numeric(median(fakecirc))

caca1 <- dat_common %>%
  filter(site=='Duke',sp=='caca',chamber==1) %>%
  pull(time)
