# Distance or overlap
# Done as circular.

calc_weight <- function(x) { # a vector of hours
  tab <- table(factor(x,  levels=as.character(0:23)), 
               useNA="ifany")
  
  dimnames(tab) <- NULL
  mat <- cbind( weights=tab/sum(tab), points=0:23 )
  mat
}

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
      density_a <- calc_weight(sp_a)
      density_b <- calc_weight(sp_b)
      if (metric[1] == 'emd') {
        distances[sp_a, sp_b] <- emd(density_a, density_b, dist = 'hourdist')       
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

