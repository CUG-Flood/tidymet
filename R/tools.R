runningId <- function(i, step = 1, N, prefix = "") {
  perc <- ifelse(missing(N), "", sprintf(", %.1f%% ", i / N * 100))
  if (mod(i, step) == 0) {
    cat(sprintf("[%s] running%s %d ...\n", prefix, perc, i))
  }
}

#' @export
match2 <- function(x, y) {
  I <- match(x, y)
  I_x <- which.notna(I)
  I_y <- I[I_x]

  d <- data.table(x = x[I_x], y = y[I_y], I_x, I_y, grp = cumsum(c(TRUE, diff(I_y) != 1)))
  d
}

which.notna <- function(x) {
  which(!is.na(x))
}

rm_empty <- function(x) {
  if (is.list(x)) {
    x[sapply(x, length) > 0]
  } else {
    x[!is.na(x)]
  }
}

#' @export
replace_value <- function(x, value = 32766, newval = NA_integer_) {
  x[x == value] <- newval
  x
}

last <- function(x) {
  x[length(x)]
}

#' @importFrom crayon green red
ok <- function(...) {
  cat(green(...), sep = "\n")
}

warn <- function(...) {
  cat(red(...), sep = "\n")
}

fprintf <- function(fmt, ...) {
  cat(sprintf(fmt, ...))
}

#' Mode number
#' @export
mode <- function(x) {
  table(x) %>%
    sort(decreasing = TRUE) %>%
    .[1] %>%
    names() %>%
    as.numeric()
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
