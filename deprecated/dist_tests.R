allchb <- dat_common %>% filter(site == 'Duke', sp == 'prim')

# all pairwise ks tests

all_tests <- list()
ks_p_vals <- matrix(NA, nrow = 12, ncol = 12)
ks_stats <- matrix(NA, nrow = 12, ncol = 12)

for (i in 1:11) {
  pairs_i <- list()
  for (j in (i+1):12) {
    test <-  ks.test(x = allchb$cos_time[allchb$chamber == i], 
                     y = allchb$cos_time[allchb$chamber == j])
    pairs_i[[length(pairs_i) + 1]] <- test
    
    ks_p_vals[i,j] <- test$p.value
    ks_stats[i,j] <- test$statistic
    
  }
  all_tests[[i]] <- pairs_i
}

# Chi-square test

chisq_p_vals <- matrix(NA, nrow = 12, ncol = 12)
chisq_stats <- matrix(NA, nrow = 12, ncol = 12)

for (i in 1:11) {
  for (j in (i+1):12) {
    # Sample down whichever is longer
    x <- allchb$cos_time[allchb$chamber == i]
    y <- allchb$cos_time[allchb$chamber == j]
    
    if (length(x) > length(y)) x <- sample(x, length(y)) else y <- sample(y, length(x))
    
    test <-  chisq.test(x, y, simulate.p.value = TRUE)

    chisq_p_vals[i,j] <- test$p.value
    chisq_stats[i,j] <- test$statistic
    
  }
}

# Watson two-sample test (circular data)

watson_crit_vals <- matrix(NA, nrow = 12, ncol = 12)
watson_stats <- matrix(NA, nrow = 12, ncol = 12)

# function to get critical value
get_crit_value <- function(wtest) {
  o <- capture.output(wtest)
  as.numeric(strsplit(o[5], ' ')[[1]][5])
}

for (i in 1:11) {
  pairs_i <- list()
  for (j in (i+1):12) {
    test <-  watson.two.test(x = circular(allchb$time[allchb$chamber == i], units = 'hours', template = 'clock24'),
                             y = circular(allchb$time[allchb$chamber == j], units = 'hours', template = 'clock24'),
                             alpha = 0.05)
    pairs_i[[length(pairs_i) + 1]] <- test
    
    watson_crit_vals[i,j] <- get_crit_value(test)
    watson_stats[i,j] <- test$statistic
    
  }
  all_tests[[i]] <- pairs_i
}

# Loop KS and Chisq test for all pairs of chambers and all species

test_fn <- function(x, y) {
  
  # KS test : no downsampling
  ks_result <- ks.test(x, y)
  
  # Watson two sample test
  watson_result
  
  # Chi-square test : requires downsampling
  if (length(x) > length(y)) {
    x_samp <- 0
    while (length(unique(x_samp)) < 2) x_samp <- sample(x, length(y)) 
    x <- x_samp
  } else  {
    y_samp <- 0
    while (length(unique(y_samp)) < 2) y_samp <- sample(y, length(x))
    y <- y_samp
  }
  chisq_result <- chisq.test(x, y, simulate.p.value = TRUE)
  
  data.frame(ks_statistic = ks_result$statistic,
             ks_p_val = ks_result$p.value,
             chisq_statistic = chisq_result$statistic,
             chisq_p_val = chisq_result$p.value,
             watson_statistic = )
  
}

# Looping function to run within each species
loop_fn <- function(dat) {
  chambers <- unique(dat$chamber)
  res <- list()
  for (i in 1:(length(chambers) - 1)) {
    for (j in (i+1):length(chambers)) {
      res[[length(res) + 1]] <- cbind(chamber1 = chambers[i], chamber2 = chambers[j],
                                      test_fn(x = dat$cos_time[dat$chamber == chambers[i]],
                                              y = dat$cos_time[dat$chamber == chambers[j]]))
    }
  }
  do.call(rbind, res)
}

# Filter where at least 5 individuals are in the chamber, there are at least 2 times observed
# This means that nyfa only has 1 qualifying chamber so it is removed from analysis

all_tests_by_species <- dat_common %>%
  filter(site == 'Duke') %>%
  group_by(sp, chamber) %>%
  filter(n() >= 5 & length(unique(time)) > 1) %>%
  ungroup %>%
  group_by(sp) %>%
  filter(length(unique(chamber)) > 1) %>%
  group_modify(~ loop_fn(.))

# connect with temperature
all_tests_by_species %>%
  mutate(chamber1_temp = trt$temperature[match(chamber1, trt$chamber)],
         chamber2_temp = trt$temperature[match(chamber2, trt$chamber)])

# Make a plot showing whether the chi-squared test shows they are different
# 15 of them are different according to it.

# Do some kind of "Tukey" letters to show this: assign a letter to each chamber based on the group it's in

library(multcompView)

all_tests_by_species %>%
  mutate(name = paste(chamber1, chamber2, sep = '-')) %>%
  group_by(sp) %>%
  group_map(~ multcompLetters(setNames(.$chisq_p_val, .$name)))

# This is fine except for apru which has a ridiculous number of letters needed to differentiate.



# Mantel test -------------------------------------------------------------

# We could correlate the overlap distance matrix with the temperature distance matrix
# Also try earth moving distance

library(ade4)
library(Ostats)
library(emdist)

# Create temperature distance matrix
duke_temp_dist <- dist(trt$temperature[trt$site == 'Duke'])
harvard_temp_dist <- dist(trt$temperature[trt$site == 'Harvard'])

# 1 - overlap would be the distance between two distributions. 
# Calculate this distance matrix for all species.
# Give an option for earth mover's distance also
# Edit April 20: convert all of this to circular.

# Circular code courtesy of Kjetil Halvorsen
# See https://stats.stackexchange.com/questions/461345/earth-movers-distance-implementation-for-circular-distributions
hourdist <- function(A, B) sum(pmin(  (A-B)%%24, (B-A)%%24 ) )  


all_pairs_asmatrix <- function(traits, sp, metric = c('overlap', 'emd')) {
  require(emdist)
  dat <- data.frame(traits=traits, sp=sp, stringsAsFactors = FALSE)
  dat <- dat[complete.cases(dat), ]
  abunds <- table(dat$sp)
  abunds <- abunds[abunds>1]
  spp <- names(abunds)
  dat <- dat[dat$sp %in% spp, ]
  traitlist <- split(dat$traits, dat$sp)
  nspp <- length(traitlist)
  
  distances <- matrix(NA, nrow = nspp, ncol = nspp)
  
  for (sp_a in 1:(nspp-1)) {
    for (sp_b in (sp_a+1):nspp) {
      a <- traitlist[[sp_a]]
      b <- traitlist[[sp_b]]
      density_a <- calc_weight(a)
      density_b <- calc_weight(b)
      if (metric[1] == 'emd') {
        distances[sp_a, sp_b] <- emd(density_a, density_b, dist = hourdist)       
      } 
      if (metric[1] == 'overlap') {
        distances[sp_a, sp_b] <- 
          1 - circular_overlap_24hour(a, b)[1]
      }
    }
  }
  
  dimnames(distances) <- list(spp, spp)
  distances
}



# Calculate the overlaps
overlap_mats <- dat_common %>%
  group_by(site, sp, chamber) %>%
  filter(n() >= 5 & length(unique(time)) > 1) %>%
  ungroup %>%
  group_by(site, sp) %>%
  filter(length(unique(chamber)) > 1) %>%
  nest %>%
  mutate(temp_dist = if_else(site == 'Duke', list(duke_temp_dist), list(harvard_temp_dist))) %>%
  mutate(mat = map(data, ~ all_pairs_asmatrix(traits = .$time, sp = .$chamber, metric = 'overlap')),
         mat_emd = map(data, ~ all_pairs_asmatrix(traits = .$time, sp = .$chamber, metric = 'emd')),
         temp_dist = map2(mat, temp_dist, ~ as.dist(as.matrix(.y)[as.numeric(row.names(.x)), as.numeric(row.names(.x))])),
         dist_overlap = map(mat, ~ as.dist(t(.))),
         dist_emd = map(mat_emd, ~ as.dist(t(.))))

set.seed(919)
overlap_mats <- overlap_mats %>%
  mutate(mantel_test_overlap = map2(temp_dist, dist_overlap, ~ mantel.rtest(.x, .y, nrepet = 9999)),
         mantel_test_emd = map2(temp_dist, dist_emd, ~ mantel.rtest(.x, .y, nrepet = 9999)),
         mantel_test_compare2 = map2(dist_emd, dist_overlap, ~ mantel.rtest(.x, .y, nrepet = 9999)))

# Extract p values (OMG p values)

overlap_mats <- overlap_mats %>%
  mutate_at(vars(starts_with('mantel_test')), list(stat = ~ map_dbl(., 'obs'), p_val = ~ map_dbl(., 'pvalue')))

# Show
overlap_mats %>%
  select(site, sp, contains('p_val'))

# What is the trend of mean activity time for Crematogaster with temp
# Take mean in circular distribution
mean_times <- dat_common %>%
  group_by(site, sp, chamber, temperature, chamber_temp) %>%
  summarize(mean_time = mean(circular(time, units = 'hours', modulo = '2pi')))

mean_times %>% 
  filter(site == 'Duke') %>%
  ggplot(aes(x = temperature, y = mean_time)) +
  facet_wrap(~ sp, scales = 'free_y') +
  geom_point() +
  geom_smooth(method = 'lm')

# Do a mixed model.
library(lme4)

# Random intercept and random slope model
duke_mean_model <- lmer(mean_time ~ temperature + (temperature | sp), data = mean_times %>% filter(site == 'Duke'))

summary(duke_mean_model)
fixef(duke_mean_model)
ranef(duke_mean_model)
coef(duke_mean_model)

# Do it Bayesian, silly
library(brms)
options(mc.cores = 2)
duke_mean_bayesfit <- brm(mean_time ~ temperature + (temperature | sp), data = mean_times %>% filter(site == 'Duke'),
                          chains = 2, iter = 2000, warmup = 1000)

summary(duke_mean_bayesfit)
fixef(duke_mean_bayesfit)
ranef(duke_mean_bayesfit)
coef(duke_mean_bayesfit)
bayes_R2(duke_mean_bayesfit)

library(brmstools)
panels(duke_mean_bayesfit, grouping = 'sp', xvar = 'temperature')

# Overall there is no effect of temperature. If we look at individual coefficient estimates the closest is A rudis going later, and P imparis going earlier. Both are only suggestive.


