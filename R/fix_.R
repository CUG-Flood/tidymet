#' cal_exceed
#'
#' @param x numeric or matrix
#' @param values Should be in descend order
#'
#' @export
cal_exceed <- function(x, values) {
  for (i in seq_along(values)) {
    value <- values[i]
    x[which(x >= value)] %<>%
      subtract(value)
  }
  return(x)
}

#' @export
#' @rdname cal_exceed
cal_below <- function(x, values) {
  for (i in seq_along(values)) {
    value <- values[i]
    x[which(x <= value)] %<>%
      subtract(value)
  }
  return(x)
}


#' fix_alt
#' For China meteorological data V3
#' @export
fix_alt <- function(d) {
  if ("alt" %in% colnames(d)) {
    d[alt > 100000L, alt := alt - 100000L]
  }

  d
}

fix_ET <- . %>%
  replace_value(value = 32700) %>%
  cal_exceed(values = 1000L)

fix_G <- . %>%
  cal_exceed(values = 10000L) %>%
  cal_below(values = -10000L)

fix_prcp <- . %>%
  replace_value(value = 32700, newval = 1L) %>%
  cal_exceed(values = c(32000L, 31000L, 30000L))

fix_WIN <- . %>%
  cal_exceed(values = 1000L)

fix_RH <- . %>%
  cal_exceed(values = 300L)

fix_P <- . %>%
  cal_exceed(values = 20000L)
