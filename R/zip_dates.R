#' zip_dates
#'
#' @param dates Date vector
#' @param by increment of the sequence, could be one of 'day', 'month' and 'year'.
#' @param format character string.
#' @param check_duplicate If true, duplicated formated dates by `format` will be
#' removed. If by = 'month' or 'year', suggest setting \code{check_duplicate} as 
#' TRUE.
#' 
#' @example man/examples/ex-zip_dates.R
#' 
#' @importFrom lubridate make_date ymd
#' @import data.table
#' @export
zip_dates <- function(dates, by = NULL, format=NULL,
    check_duplicate = FALSE)
{
    # 1.1 guess parmater `by`
    if (is.null(by)) {
        ndays <- table(year(dates)) %>% median()
        # If no duplicated dates, ndays = 1, 12, 365/366 are corresponding to
        # year, month, day respectively.
        if (ndays <= 5) {
            by <- "year"
        } else if (ndays <= 30) {
            by <- "month"
        } else {
            by <- "day"
        }
    }

    # 1.2 check format
    if (is.null(format)){
        formats <- c("%Y", "%Y%m", "%Y%m%d")
        I <- match(by, c("year", "month", "day"))
        format <- formats[I]
    }

    # 1.3 remove duplicated dates
    if (check_duplicate) {
        year  <- year(dates)
        month <- month(dates)
        day   <- day(dates)

        if (!grepl("%m", format)) month <- 1
        if (!grepl("%d", format)) day <- 1

        dates <- make_date(year, month, day) %>% unique()
    }

    len <- length(dates)
    date_begin <- dates[1]
    date_end   <- dates[len]
    dates_norm <- seq.Date(date_begin, date_end, by)

    # statistics
    n <- length(dates_norm)
    n_miss <- n - length(dates)
    perc_miss <- n_miss/n*100 # (%)

    # 1. zip left dates
    info <- match2(dates, dates_norm)
    str_left <- info[, str_func(x, format), .(grp)]$V1 %>% paste(collapse = ", ")
    str_miss <- ""

    # 2. zip missing dates
    if (n_miss > 0){
        info_miss <- match2(dates_norm[-info$I_y], dates_norm)
        str_miss <- info_miss[, str_func(x, format), .(grp)]$V1 %>% paste(collapse = ", ")
    }

    data.table(n_complete = n, n_left = n-n_miss, n_miss, perc_miss, 
        date_begin, date_end, str_left, str_miss)
}

# format x at here
str_func <- function(x, format = "%Y%m"){
    len <- length(x)
    if (len == 1){
        format(x, format)
    } else {
        paste(format(x[1], format), format(x[len], format), sep = "-")
    }
}
