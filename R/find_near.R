#' find_near
#' @param p a vector of length 2, `[lon, lat]`
#' @export
find_near <- function(p, st) {
  loc <- st[, .(lon, lat)]
  loc %<>%
    check_matrix()
  dist <- rdist.earth(loc, p)
  i <- which.min(dist)
  s <- st[i, ] |>
    cbind(dist = dist[i])
  s
}
