#' @title missInfo
#' @name missInfo
#' 
#' @export
missInfo <- function(data, date, station, clipdata = FALSE, ...) UseMethod("missInfo")

#' @description detect one station n_miss infomation for single meteorological element
#' @details If all(is.na(x)) is true, then return a dtime begin and end date is 1951-01-01
#'
#' @param x one station meteorological one column matrix data. Rownames is daily time string
#' @param date corresponding date of x (support monthly or daily)
#' @param station character
#' @param clipdata If clipdata is true, NAs at head and tail will be trimed.
#' And trimed data will be return.
#' @param Info_detail If true detail n_miss information will be return, else
#' n_miss information is na.
#' @param Info_days If true n_miss seg days will be return
#' @param collapse missinfo info collapse
#'
#' @rdname missInfo
#' @export
missInfo.default <- function(x, date, station = "S1", clipdata = FALSE,
    Info_detail = TRUE, Info_days = TRUE, collapse = ", ") {
    n_miss <- NA
    n_complete = 0

    i_begin <- i_end <- 1
    gap_min <- gap_max <- 0
    perc_miss = 100
    info <- ""
    xtrim = NULL
    # if x is blank, dateBegin and dateEnd set to be '1951-01-01'
    date_begin <- as.Date("0000-01-01")
    date_end   <- date_begin
    run <- rle(!is.na(x))
    len <- run$lengths
    Id  <- c(0, cumsum(len))

    info <- list()
    info_dt <- list()

    I_havedata <- which(run$values)
    # If having data, not blank
    if (length(I_havedata) > 0) {
        I_blank <- which(!run$values)  #use for generate n_miss info

        # delete blank values in head and tail
        if (length(I_blank) > 0) {
            if (I_blank[length(I_blank)] > I_havedata[length(I_havedata)])
                I_blank <- I_blank[-length(I_blank)]
            if (length(I_blank) > 0 && I_blank[1] == 1)
                I_blank <- I_blank[-1]
        }

        # collect n_miss information into `info` variable
        if (length(I_blank) > 0) {
            gap_max <- max(len[I_blank])
            gap_min <- min(len[I_blank])

            if (Info_detail) {
                for (i in seq_along(I_blank)) {
                    imiss_begin <- Id[I_blank[i]] + 1
                    imiss_end   <- Id[I_blank[i] + 1]
                    n_miss <- imiss_end - imiss_begin + 1

                    time_begin.miss <- date[imiss_begin]
                    time_end.miss   <- date[imiss_end]

                    str_miss <- if (n_miss == 0) {
                        format(date[imiss_begin], "%Y%m%d")
                    } else {
                        paste0(format.Date(date[imiss_begin], "%Y%m%d"), "-",
                               format.Date(date[imiss_end], "%Y%m%d"))
                    }
                    info_dt[[i]] <- data.table(date_begin = time_begin.miss, date_end = time_end.miss, n_miss, str_miss)
                    info[[i]] <- ifelse(Info_days, sprintf("%s, \t%ddays", str_miss, n_miss), str_miss)
                }
                info <- paste(unlist(info), collapse = collapse)
                info_dt %<>% do.call(rbind, .) #%>% cbind(ngrp = 1:nrow(.), .)
              }  #endif Info_detail
        }  #endif length(I_blank) > 0
        i_begin    <- Id[I_havedata[1]] + 1
        i_end      <- Id[I_havedata[length(I_havedata)] + 1]
        date_begin <- date[i_begin]
        date_end   <- date[i_end]

        n_complete <- i_end - i_begin + 1
        n_miss     <- sum(len[I_blank])  #if I_blank is null, zero will be return
        perc_miss  <- round(n_miss/n_complete * 100, 2)
    }

    info_miss <- data.table(date_begin, date_end, n_complete, n_miss, perc_miss,
        gap_min, gap_max, info = info, detailedInfo = I(list(info_dt)))
    if (clipdata)
        xtrim <- dtime(data = x[i_begin:i_end], station = station, date_begin, date_end)
    obj <- list(info = info_miss, xtrim = xtrim)
    class(obj) = "missInfo"
    obj
}

#' @rdname missInfo
#' @export
missInfo.dtime <- function(x, ...) {
    missInfo.default(x$data, date = seq(x), x$station, ...)
}
