# Overlap of two densities of circular distributions
# Use ants to test

# dat_common should be loaded

library(circular)
library(sfsmisc)

# test with two time datasets

testdat <- dat_common %>%
  filter(site == 'Duke', chamber %in% c(2,3), sp == 'apru')

x1 <- testdat$time[testdat$chamber == 2]
x2 <- testdat$time[testdat$chamber == 3]

plot(density(x1, bw = 1))
plot(density(x2, bw = 1))

# Convert x1 and x2 to circular objects

x1c <- circular(x1, units = 'hours', template = 'clock24')
x2c <- circular(x2, units = 'hours', template = 'clock24')

plot(x1c, stack = TRUE, shrink = 2)
plot(x2c, stack = TRUE, shrink = 2)

bandwidth <- 12

plot(density(x1c, bw = bandwidth), shrink = 2)
plot(density(x2c, bw = bandwidth), shrink = 2)

x1d <- density(x1c, bw = bandwidth)
x2d <- density(x2c, bw = bandwidth)
w <- pmin(x1d$y, x2d$y)

total <- integrate.xy(x1d$x, x1d$y) + integrate.xy(x2d$x, x2d$y)
intersection <- integrate.xy(x1d$x, w)

( overlap <- 2 * intersection / total )

pairwise_overlap(x1, x2, bw = 1)
pairwise_overlap(x1, x2)

circular_overlap <- function(a, b, norm = TRUE, bw, n = NULL) {
  
  # clean input
  a <- as.numeric(na.omit(a))
  b <- as.numeric(na.omit(b))
  
  # convert input to circular
  acirc <- circular(a, units = 'hours', template = 'clock24')
  bcirc <- circular(a, units = 'hours', template = 'clock24')

  # generate kernel densities
  # add option to use user-defined n
  # Must specify bandwidth
  if (is.null(n)) n <- 512 # Default value if not given
  da <- density(a, bw=bw, n=n)
  db <- density(b, bw=bw, n=n)
  d <- data.frame(x=da$x, a=da$y, b=db$y)
  
  # If not normalized, multiply each density entry by the length of each vector
  if (!norm) {
    d$a <- d$a * length(a)
    d$b <- d$b * length(b)
  }
  
  # calculate intersection densities
  d$w <- pmin(d$a, d$b)
  
  # integrate areas under curves
  integral_a <- sfsmisc::integrate.xy(d$x, d$a)
  integral_b <- sfsmisc::integrate.xy(d$x, d$b)
  total <- integral_a + integral_b
  intersection <- sfsmisc::integrate.xy(d$x, d$w)
  
  # compute overlap coefficient
  overlap <- 2 * intersection / total
  overlap_a <- intersection / integral_a
  overlap_b <- intersection / integral_b
  
  return(c(overlap = overlap, overlap_a = overlap_a, overlap_b = overlap_b))
  
}

circular_overlap(x1, x2, bw = 0.3)

# Watson two sample test --------------------------------------------------

# This is the analog of the K-S test for circular data. Are the two distributions the same?
# It is implemented in circular::watson.two

watson.two.test(x = x1c, y = x2c)
# If test stat is greater than the critical value, we reject H0 that they are from the same distribution.
