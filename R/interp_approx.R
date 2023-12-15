#' na_approx.dtime
#'
#' @param x dtime object
#' @inheritParams zoo::na.approx
#' @param ... ignored
#'
#' @importFrom zoo na.approx
#' @export
na_approx.dtime <- function(x, maxgap = 5, ...) {
  x$data <- as.integer(na.approx(x$data, maxgap = maxgap, na.rm = FALSE))
  x
}

#' interp_approx
#'
#' Linear interpolation with nearest values
#'
#' @inheritParams interp_linear
#' @inheritParams na_approx.dtime
#' @param verbose boolean whether print progrss message?
#' @param .parallel boolean
#'
#' @export
interp_approx <- function(
    xx, stationInfo, maxgap = 5, verbose = TRUE,
    .parallel = FALSE, ...) {
  I <- with(stationInfo, which(n_miss > 0 & gap_min <= maxgap)) # gap satisfied
  N <- length(I)
  step <- floor(N / 10)

  FUN <- ifelse(.parallel, `%dopar%`, `%do%`)
  xx[I] <- FUN(foreach(x = xx[I], i = icount()), {
    if (verbose) {
      runningId(i, step, N, "interp_approx")
    }
    na_approx.dtime(x, maxgap)
  })
  return(xx)
}
