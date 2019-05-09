#' na_hisavg.dtime
#'
#' using history average y_his replace missing values
#'
#' @param x dtime class
#'
#' @export
na_hisavg.dtime <- function(x, ...) {
    Id <- which(is.na(x$data))  #Modified Dongdong Kong, 2016-04-23
    if (length(Id) > 0) {
        doy <- yday(seq(x))
        # 366 should be caution
        tmp <- aggregate(x$data, list(doy = doy), mean, na.rm = T)$x
        tmp <- as.integer(tmp)
        x$data[Id] <- tmp[doy[Id]]
    }
    x
}

#' interp_hisavg
#'
#' @inheritParams interp_linear
#'
#' @export
interp_hisavg <- function(xx, stationInfo, ...) {
    I <- which(stationInfo$n_miss > 0)

    xx[I] <- foreach(i = I, x = xx[I]) %do% {
        na_hisavg.dtime(x)
    }
    xx
}
