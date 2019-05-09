origin <- as.Date("1970-01-01") # origin of date

#' date_intersect
#'
#' Intersect of two date interval
#'
#' @param date_x date vector, [date_begin, date_end]
#' @param date_y same as date_x
#' 
#' @export
date_intersect <- function(date_x, date_y) {
    # int_x <- interval(x_begin, x_end)
    # int_y <- interval(y_begin, y_end)
    if (date_x[1] > date_y[2] || date_y[1] > date_x[2]){
        NULL
    } else {
        ubegin <- max(date_x[1], date_y[1])
        uend   <- min(date_x[2], date_y[2])
        # interval(ubegin, uend)
        as.Date(ubegin:uend, origin)
    }
}

#' @param int_x interval x
#' @param int_y interval y
#'
#' @rdname date_intersect
#' @importFrom lubridate int_start int_end int_overlaps interval
#' @export
int_intersect <- function(int_x, int_y){
    date_x <- c(int_start(int_x), int_end(int_x)) %>% as.Date()
    date_y <- c(int_start(int_y), int_end(int_y)) %>% as.Date()
    date_intersect(date_x, date_y)
}

#' @param x dtime object
#' @param y same as x
#' 
#' @rdname date_intersect
#' @export
dtime_intersect <- function(x, y){
    date_x <- c(x$begin, x$end)
    date_y <- c(y$begin, y$end)
    date_intersect(date_x, date_y)
}
