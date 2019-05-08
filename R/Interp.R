# data interpolation by na.approx, history mean and linear regress

#' @importFrom zoo na.approx
#' @export
naApprox.dtime <- function(x, maxgap = 5, ...) {
    x$data <- as.integer(na.approx(x$data, maxgap = maxgap, na.rm = F))
    x
}

# put origin naHisAvg and naAprrox single
#' @export
naInterp <- function(xx, stationInfo, maxgap = 5, .progress = "text", 
    .parallel = FALSE, .FUN = naApprox.dtime, ...)
{
  # fun <- naHisAvg.dtime; .FUN <- deparse(substitute(fun))
  .FUN <- deparse(substitute(.FUN))
  fun <- match.fun(.FUN)

  # Id <- which(stationInfo$maxgap <= maxgap), Modified By Dongdong Kong, 2016-04-23
  # Id <- which(stationInfo$missing > 0), Modified By Dongdong Kong, 2016-12-15
  Id <- with(stationInfo, which(missing > 0 & mingap <= maxgap))

  if (.parallel) .progress = "none"
  progress <- create_progress_bar(.progress)
  progress$init(length(Id)); on.exit(progress$term())

  xx[Id] <- if (.parallel){
    foreach(x = xx[Id], j = icount(), .export = .FUN) %dopar% fun(x, maxgap)
  }else{
    foreach(x = xx[Id], j = icount()) %do%{
        progress$step()
        fun(x, maxgap)
    }
  }
  return(xx)#quickly return
}


#' Interp
#' 
#' Main funtion of interpolation, combined with na.approx, linear 
#' and hisavg interpolations
#' 
#' @param data_in matrix data with dim(date*station)
#' @param smax maximum searching radius of linear interpolation (unit:km), default is 100km.
#' @param nmax maximum number stations used in linear interpolation, default is 10.
#' @param maxgap maximum gap used in naApprox interpolation
#' @param sinkfile file that used to save linear interpolation information
#' 
#' @export
Interp <- function(xlist, stationInfo, dist, nmax = 10, maxgap = 5,
                   sinkfile = NULL, .parallel = T, ...){
    # .progress = "text"
    #-----------------------------------------------
    cat("[1.1]. runing na.approx interpolate ...\n")
    xx_naapprox <- naInterp(xlist, stationInfo, maxgap, .parallel = .parallel, .FUN = naApprox.dtime, ...)
    cat("[1.2]. detect missing info after na.approx interpolate ...\n")
    Info.naApprox <- MissInfo.list(xx_naapprox, .parallel = .parallel, detailInfo = F)$stationInfo
    try(plot(stationInfo$missing - Info.naApprox$missing,
           xlab ="std", ylab = "Number of Interped values", main = "naApprox Interpolation"))

    #-----------------------------------------------
    cat("[2.1] runing linear interpolate ...\n")
    xx <- linearInterp(xx_naapprox, Info.naApprox, dist, nmax, sinkfile = sinkfile, ...)
    xx_linear <- xx$xtrim
    cat("[2.2] detect missing info after first lm interpolate ...\n")
    Info.linear <- MissInfo.list(xx_linear, .parallel = .parallel, detailInfo = F)$stationInfo
    try(plot(Info.naApprox$missing - Info.linear$missing,
           xlab ="std", ylab = "Number of Interped values", main = "Linear Interpolation"))

    #-----------------------------------------------
    cat("[3.1] history average interpolate...\n")
    xx_hisavg <- naInterp(xx_linear, Info.linear, .parallel = .parallel, .FUN = naHisAvg.dtime, ...)
    cat("[3.2] detect missing info after hisavg interpolate ...\n")
    Info.hisavg <- MissInfo.list(xx_hisavg, .parallel = .parallel, detailInfo = T)$stationInfo
    try(plot(Info.linear$missing - Info.hisavg$missing,
           xlab ="std", ylab = "Number of Interped values", main = "Hisavg Interpolation"))

    # return every step detected missing info
    stationInfo_list <- list(origin = stationInfo, naApprox = Info.naApprox, linear = Info.linear,
                           hisavg = Info.hisavg)
    list(stationInfo = stationInfo_list, x = xx_hisavg)#quickly return
}
