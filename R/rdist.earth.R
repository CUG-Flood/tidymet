#' @title rdist.earth
#'
#' @param x1 2 columns matrix, [lat, lon]
#' @param x2 same as x1
#'
#' @examples
#' \dontrun{
#' loc <- st840[, .(lat, lon)] %>% as.matrix()
#' dist <- rdist.earth(loc)
#' }
#'
#' @export
rdist.earth <- function(x1, x2 = x1) {
  R <- 6378.388
  # coslat1 <- cos(x1[, 2]) sinlat1 <- sin(x1[, 2]) coslon1 <- cos(x1[, 1]) sinlon1 <- sin(x1[, 1])
  coslat1 <- cos((x1[, 1] * pi) / 180)
  sinlat1 <- sin((x1[, 1] * pi) / 180)
  coslon1 <- cos((x1[, 2] * pi) / 180)
  sinlon1 <- sin((x1[, 2] * pi) / 180)

  coslat2 <- cos((x2[, 1] * pi) / 180)
  sinlat2 <- sin((x2[, 1] * pi) / 180)
  coslon2 <- cos((x2[, 2] * pi) / 180)
  sinlon2 <- sin((x2[, 2] * pi) / 180)
  pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*% t(cbind(
    coslat2 * coslon2, coslat2 * sinlon2,
    sinlat2
  ))
  return(R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp)))
}

rdist.earth2 <- function(x1, x2 = NULL) {
  if (is.vector(x1)) {
    x1 %<>%
      t()
  }
  if (is.vector(x2)) {
    x2 %<>%
      t()
  }
  x1 %<>%
    check_matrix()
  x2 %<>%
    check_matrix()

  # miles = FALSE, R = NULL R = R %||% ifelse(miles, 3963.34, 6378.388)
  R <- 6378.388

  coslat1 <- cos((x1[, 2] * pi) / 180)
  sinlat1 <- sin((x1[, 2] * pi) / 180)
  coslon1 <- cos((x1[, 1] * pi) / 180)
  sinlon1 <- sin((x1[, 1] * pi) / 180)

  if (is.null(x2)) {
    pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*% t(cbind(coslat1 * coslon1, coslat1 *
      sinlon1, sinlat1))
  } else {
    coslat2 <- cos((x2[, 2] * pi) / 180)
    sinlat2 <- sin((x2[, 2] * pi) / 180)
    coslon2 <- cos((x2[, 1] * pi) / 180)
    sinlon2 <- sin((x2[, 1] * pi) / 180)
    pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*% t(cbind(coslat2 * coslon2, coslat2 *
      sinlon2, sinlat2))
  }
  dist <- R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp))

  if (nrow(dist) == 1 || ncol(dist) == 1) {
    dist <- c(dist)
  }
  dist
}

check_matrix <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  x
}

#' find_near
#' @param p a vector of length 2, `[lon, lat]`
#' @export
find_near <- function(p, st) {
  loc <- st[, .(lon, lat)]
  loc %<>%
    check_matrix()
  dist <- rdist.earth2(loc, p)
  i <- which.min(dist)
  s <- st[i, ] |>
    cbind(dist = dist[i])
  s
}
