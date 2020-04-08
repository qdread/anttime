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

# Loop KS and Chisq test for all pairs of chambers and all species

test_fn <- function(x, y) {
  
  # KS test : no downsampling
  ks_result <- ks.test(x, y)
  
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
             chisq_p_val = chisq_result$p.value)
  
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

# 1 - overlap would be the distance between two distributions. 
# Calculate this distance matrix for all species.
# Give an option for earth mover's distance also

all_pairs_asmatrix <- function(traits, sp, norm = TRUE, bw = NULL, n = NULL, metric = c('overlap', 'emd')) {
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
      if (metric[1] == 'emd') {
        distances[sp_a, sp_b] <- emd(
          with(density(traitlist[[sp_a]]), cbind(y, x)),
          with(density(traitlist[[sp_b]]), cbind(y, x))
        )
      } 
      if (metric[1] == 'overlap') {
        distances[sp_a, sp_b] <- 
          1 - pairwise_overlap(a = traitlist[[sp_a]], 
                               b = traitlist[[sp_b]], 
                               norm = norm, 
                               bw = bw, 
                               n = n)[1]
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
  mutate(mat = map(data, ~ all_pairs_asmatrix(traits = .$cos_time, sp = .$chamber, metric = 'overlap')),
         mat_emd = map(data, ~ all_pairs_asmatrix(traits = .$cos_time, sp = .$chamber, metric = 'emd')),
         temp_dist = map(mat, ~ as.dist(as.matrix(duke_temp_dist)[as.numeric(row.names(.)), as.numeric(row.names(.))])),
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

# What is the trend of mean activity time for Crematogaster with temp
# Take mean of cos time and back transform
mean_times <- dat_common %>%
  group_by(site, sp, chamber, temperature, chamber_temp) %>%
  summarize(mean_time = mean(circular(time, units = 'hours', modulo = '2pi')))

mean_times %>% 
  filter(site == 'Duke') %>%
  ggplot(aes(x = temperature, y = mean_time)) +
  facet_wrap(~ sp, scales = 'free_y') +
  geom_point()
