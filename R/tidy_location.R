# 对经纬度、高程信息进行转换
#' @export
tidy_location <- function(x) {
  varnames <- colnames(x)
  x$lat %<>%
    deg2dec()
  if ("lon" %in% varnames) {
    x$lon %<>%
      deg2dec()
  }
  if ("long" %in% varnames) {
    x$long %<>%
      deg2dec()
  }

  if ("alt" %in% varnames && is.integer(x$alt)) {
    x$alt[x$alt >= 1e+05] <- x$alt[x$alt >= 1e+05] - 1e+05
    x$alt <- x$alt / 10
    # x$alt <- x$alt/10
  }
  x
}

#' @export
deg2dec <- function(x) {
  deg <- floor(x / 100)
  deg + (x - deg * 100) / 60
}

#' @export
dec2deg <- function(x) {
  int <- floor(x)
  other <- x - int
  int * 100 + round(other * 60)
}

#' @export
get_alt <- function(x) {
  x[x >= 1e+05] <- x[x >= 1e+05] - 1e+05
  x / 10
}
