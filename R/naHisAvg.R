#' @title naHisAvgSingle
#' @description using history average ti replace missing values
#' @param x dtime class
#' @export
naHisAvg.dtime <- function(x, ...){
    Id <- which(is.na(x$data))#Modified Dongdong Kong, 2016-04-23
    if (length(Id) > 0){
        doy <- lubridate::yday(seq(x))
        #366 should be caution
        tmp <- aggregate(x$data, list(doy = doy), mean, na.rm = T)$x
        tmp <- as.integer(tmp)
        x$data[Id] <- tmp[doy[Id]]
    }
    x
}
