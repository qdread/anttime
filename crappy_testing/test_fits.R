source('load_wrangle_data.R')

# Distance between two hour distributions
hourdist <- function(A, B) sum(pmin(  (A-B)%%24, (B-A)%%24 ) )  

# Manually calculate density from the vector of hours
calc_weight <- function(x) { # a vector of hours
  tab <- table(factor(x,  levels=as.character(0:23)),
               useNA="ifany")
  
  dimnames(tab) <- NULL
  weights <- tab / sum(tab)
  
  mat <- cbind( weights=weights, points=0:23 )
  mat
}

# Calculate EMD or overlap distance for all pairs (function)
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


# Create temperature distance matrix
duke_temp_dist <- dist(trt$temperature[trt$site == 'Duke'])
harvard_temp_dist <- dist(trt$temperature[trt$site == 'Harvard'])

overlap_mats <- dat_common %>%
  group_by(site, sp, chamber) %>%
  filter(n() >= 5 & length(unique(time)) > 1) %>%
  ungroup %>%
  group_by(site, sp) %>%
  filter(length(unique(chamber)) > 1) %>%
  nest %>%
  mutate(temp_dist = if_else(site == 'Duke', 
                             list(duke_temp_dist), 
                             list(harvard_temp_dist))) %>%
  mutate(mat = map(data, 
                   ~ all_pairs_asmatrix(traits = .$time, 
                                        sp = .$chamber, 
                                        metric = 'overlap')),
         mat_emd = map(data, 
                       ~ all_pairs_asmatrix(traits = .$time, 
                                            sp = .$chamber, 
                                            metric = 'emd')),
         temp_dist = map2(mat, temp_dist, 
                          ~ as.dist(as.matrix(.y)[as.numeric(row.names(.x)), 
                                                  as.numeric(row.names(.x))])),
         dist_overlap = map(mat, ~ as.dist(t(.))),
         dist_emd = map(mat_emd, ~ as.dist(t(.))))

set.seed(919)

# Mantel tests for the two distance metrics for all species/site combos.
overlap_mats <- overlap_mats %>%
  mutate(mantel_test_overlap = 
           map2(temp_dist, dist_overlap, ~ mantel.rtest(.x, .y, nrepet = 9999)),
         mantel_test_emd = 
           map2(temp_dist, dist_emd, ~ mantel.rtest(.x, .y, nrepet = 9999))
  )

# Extract p values (OMG p values)
overlap_mats <- overlap_mats %>%
  mutate_at(vars(starts_with('mantel_test')), 
            list(stat = ~ map_dbl(., 'obs'), 
                 p_val = ~ map_dbl(., 'pvalue')))

# The actual stuff --------------------------------------------------------



mean_times <- dat_common %>%
  group_by(site, sp, chamber, temperature, chamber_temp) %>%
  summarize(median_time = median(circular(time, units = 'hours', template = 'clock24', modulo = '2pi', zero = 0)))

# Convert median times to radians, from -pi to +pi
mean_times <- mean_times %>%
  mutate(median_time_radians = as.numeric(median_time) * (2*pi/24) - pi)

options(mc.cores = 2)

# Tighten prior on intercept and SD so model converges
priors <- c(prior(student_t(3,0,3), class = Intercept),
            prior(student_t(3,5,3), class = sd),
            prior(lkj_corr_cholesky(0.5), class = L, group = sp))

duke_median_bayesfit <- brm(median_time_radians ~ temperature + (temperature | sp), 
                            family = von_mises(), 
                            prior = priors,
                            data = mean_times %>% filter(site == 'Duke'),
                            control = list(adapt_delta = 0.9),
                            chains = 2, iter = 7500, warmup = 5000, seed = 12345)
harvard_median_bayesfit <- brm(median_time_radians ~ temperature + (temperature | sp), 
                               family = von_mises(), 
                               prior = priors,
                               data = mean_times %>% filter(site == 'Harvard'),
                               chains = 2, iter = 7500, warmup = 5000, seed = 55555)