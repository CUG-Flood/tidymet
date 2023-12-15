#' @export
dtime2mat <- function(xx, date_begin = NULL, date_end = NULL) {
  date_begin <- map(xx, "begin") %>%
    unlist() %>%
    min() %>%
    as.Date(origin)
  date_end <- map(xx, "end") %>%
    unlist() %>%
    max() %>%
    as.Date(origin)
  datenum <- date_begin:date_end
  dates <- as.Date(datenum, origin)

  sites <- names(xx)
  mat <- matrix(NA_integer_, nrow = length(dates), ncol = length(sites), dimnames = list(
    format(dates),
    sites
  ))
  temp <- foreach(x = xx, i = icount()) %do% {
    I <- match(x$begin:x$end, datenum)
    mat[I, i] <- x$data
    NULL
  }
  as.data.table(mat) %>%
    cbind(date = dates, .)
}
