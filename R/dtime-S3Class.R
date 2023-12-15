#' @export
begin.Date <- function(x, ...) {
  if (!inherits(x, "Date")) {
    return("x should be date class")
  }
  x[1]
}

#' @export
end.Date <- function(x, ...) {
  if (!inherits(x, "Date")) {
    return("x should be date class")
  }
  x[length(x)]
}

#' @export
begin <- function(x, ...) UseMethod("begin")

# ' @export # end <- function(x, ...) UseMethod('end')

# only used for daily data
#' @export
seq.dtime <- function(x, ...) as.Date(x$begin:x$end, origin = "1970-01-01")

# seq(x$begin, x$end, by = x$by)
#' @export
seq_Date <- function(x, ...) as.Date(begin.Date(x):end.Date(x), origin = "1970-01-01")

#' @export
print.dtime <- function(x, ...) {
  cat(paste0("site ", x$station, ":  "))
  cat(sprintf("begin at: %s, end at: %s, %d %ss\n", x$begin, x$end, x$datelen, x$by))
  # cat('data:\n') print(head(x$data))
}

print.missInfo <- function(x, ...) {
  ncol <- ncol(x$info)
  print(x$info[, 1:(ncol - 2)])
  cat("-----------------------------------------------------\n")
  print(x$info$detailedInfo[[1]])
}

#' dtime
#'
#' @param data numeric vector or matrix
#' @param station site id or name
#' @param begin begin date
#' @param end end date
#' @param by increment of the sequence
#'
#' @export
dtime <- function(data = NULL, station = NULL, begin = Sys.Date(), end = Sys.Date(), by = "day") {
  if (!inherits(begin, "Date")) {
    begin <- as.Date(begin)
  }
  if (!inherits(end, "Date")) {
    end <- as.Date(end)
  }
  if (begin > end) {
    return("begin date should be small or equal to end date!")
  }
  if (!(by %in% c("month", "day"))) {
    return("by only can choose 'day' or 'month'")
  }
  if (!is.null(data)) {
    if (by == "day") {
      datelen <- end - begin + 1
    }
    if (by == "month") {
      datelen <- length(seq(begin, end, by = by))
    }

    dims <- dim(data)
    n <- ifelse(is.null(dims), length(data), dims[1])
    if (datelen != n) {
      return("data length or nrow should be equal to date length!")
    }
  }
  structure(list(data = data, station = station, begin = begin, end = end, by = by, datelen = as.numeric(datelen)),
    class = "dtime"
  )
}

#' @importFrom stats window<- window end
#' @export
"window<-.dtime" <- function(x, begin, end, value) {
  Id <- match(begin:end, seq(x))
  if (length(value) != length(Id)) {
    error("value should be equal to begin to end date!")
  }
  Id_nona <- Id %>%
    {
      .[which(!is.na(.))]
    }
  # data <- x$data
  x$data[Id_nona] <- value[Id_nona] # even if Id_nona is blank, have no side effect
  return(x)
}

#' @export
window.dtime <- function(x, begin, end) {
  Id <- match(begin:end, seq(x))
  # if Id is all is.na then a na vector equal to x$data will be return
  x$data[Id] # quickly return
}

#' @export
plot.dtime <- function(x, ...) {
  plot(time = seq(x), value = x$data, ...)
}
