# Plot heat maps

# Key to sort matrices by increasing temp.
duke_key <- with(trt[trt$site == 'Duke', ], chamber[order(temperature)])
harvard_key <- with(trt[trt$site == 'Harvard', ], chamber[order(temperature)])

duke_temp_sorted <- as.matrix(duke_temp_dist)[duke_key, duke_key]
duke_temp_sorted[lower.tri(duke_temp_sorted)] <- NA

rotate <- function(x) t(apply(x, 2, rev))

image(rotate(duke_temp_sorted), xaxt = 'none', yaxt = 'none', bty = 'n', col = hcl.colors(12, palette = 'viridis', rev = TRUE))
axis(3, at = seq(1,0,length.out=12), labels = duke_key, tick = FALSE)
axis(4, at = seq(1,0,length.out=12), labels = duke_key, tick = FALSE, las = 1)

heat_map <- function(mat, key, title) {
  if (class(mat) == 'matrix') mat <- pmin(mat,t(mat), na.rm = TRUE) else mat <- as.matrix(mat)
  mat_sorted <- mat[key, key]
  mat_sorted[lower.tri(mat_sorted, diag = TRUE)] <- NA
  
  rotate <- function(x) t(apply(x, 2, rev))
  
  image(rotate(mat_sorted), xaxt = 'none', yaxt = 'none', bty = 'n', col = hcl.colors(12, palette = 'viridis', rev = FALSE), main = title)
  axis(3, at = seq(0,1,length.out=12)[-1], labels = key[-1], tick = FALSE)
  axis(4, at = seq(1,0,length.out=12)[-12], labels = key[-12], tick = FALSE, las = 1)
}

par(pty = 's', mar = c(5.1, 4.1, 6, 2.1))
heat_map(duke_temp_dist, duke_key, 'temperature treatments')

heat_map(overlap_mats$mat_emd[[1]], duke_key)
heat_map(overlap_mats$mat_emd[[6]], duke_key)
