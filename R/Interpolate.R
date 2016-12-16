# data interpolation by na.approx, history mean and linear regress
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
  x#quickly return
}

#' @export
naApprox.dtime <- function(x, maxgap = 5, ...) {x$data <- as.integer(zoo::na.approx(x$data, maxgap = maxgap, na.rm = F)); x}

# put origin naHisAvg and naAprrox single
#' @export
naInterp <- function(xx, stationInfo, maxgap = 5, .progress = "text", .parallel = FALSE, .FUN = naApprox.dtime, ...){
  # fun <- naHisAvg.dtime; .FUN <- deparse(substitute(fun))
  .FUN <- deparse(substitute(.FUN)); fun <- match.fun(.FUN)

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

#' @title summary_lm
#' @description summary_lm only used for .lm.fit returned objects. It's faster 200 times than traditional lm()
#' @export
summary_lm <- function(obj, ...){
  z <- obj
  p <- z$rank

  Qr <- z$qr
  n <- nrow(Qr)
  rdf <- n - p#df.residual

  r <- z$residuals
  # f <- z$fitted.values
  # mss <- sum((f - mean(f))^2)
  rss <- sum(r^2)
  resvar <- rss/rdf

  p1 <- 1L:p
  R <- chol2inv(Qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- z$coefficients[z$pivot[p1]]
  tval <- est/se
  # ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
  # ans$residuals <- r
  coefficients <- cbind(est, se, tval, 2 * pt(abs(tval),
                                              rdf, lower.tail = FALSE))
  dimnames(coefficients) <- list(names(z$coefficients)[z$pivot[p1]],
                                 c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  return(coefficients)
  # df.int <- ifelse(attr(z$terms, "intercept"), 1L, 0L)
  # ans$r.squared <- mss/(mss + rss)
  # ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - df.int)/rdf)
  # ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, numdf = p - df.int, dendf = rdf)
  # ans
}

#' @title linearInterp
#' @param x: dtime class whose data have missing values
#' @param StationInfo: single one stationInfo returned by Missinfo function
#' @param dist: Station distance from each other dim = (n * n)
#' @export
linearInterp <- function(xtrim, stationInfo, dist, nmax = 10, sinkfile = NULL){
  # Id_needInterp <- which(stationInfo$missing > 0)
  Id <- with(stationInfo, which(missing > 0))
  if (!is.null(sinkfile)) sink(sinkfile)

  # profvis::profvis({
  for (i in Id) {#Id_needInterp
    # print(i)
    x <- xtrim[[i]]
    xtime <- seq(x) #time_day will not alter with interpolate

    cat(sprintf('[%03d] =============================\n', i))
    print(x)
    cat(sprintf('missing :\t%s\n', stationInfo[i, "info"]))

    dist.tmp <- dist[[i]]; nd = nrow(dist.tmp)
    if (nd == 0) next
    if (nd > nmax) dist.tmp <- dist.tmp[1:nmax, ]#only the first nmax element left
    Id_std <- dist.tmp$Id
    d <- dist.tmp$d

    cat("------------------------------------\n")
    cat("nearest stations used to interpolate ...\n")
    print(cbind(stationInfo[Id_std, 1:6], dist = d))
    cat("------------------------------------\n")

    for (j in seq_along(Id_std)){
      # print(j)
      y = xtrim[[Id_std[j]]]; if (class(y) != "dtime") next
      station <- y$station
      cat(sprintf("\t%02dth: station %s ", j, station))

      if ((x$begin > y$end | y$begin > x$end)){
        cat("have no intersection date! [failed]\n")
      }else{
        ubegin <- max(x$begin, y$begin)
        uend <- min(x$end, y$end)#repair error, Dongdong Kong, 2016-04-23
        utime <- as.Date(ubegin:uend, origin="1970-01-01")
        # xNa.Date <- xtime[which(is.na(x$data))]
        # if ((end(xNa.Date) < ubegin) | (begin(xNa.Date) > uend)) next#if have no overlap then exit this station
        Idx <- fmatch(utime, xtime); x_trim <- x$data[Idx]
        y_trim <- y$data[fmatch(utime, seq(y))]
        xtrimNa.Id <- which(is.na(x_trim))

        # modified 20161021, if y can fix x's partial missing value then use it.
        if (length(which(!is.na(y_trim) & is.na(x_trim))) == 0) {
          cat("have missing values in the corresponding missing date! [failed]\n")
        }else{
          idn <- !(is.na(y_trim) | is.na(x_trim))#remove nan values
          xt <- x_trim[idn]
          yt <- y_trim[idn]
          #trim NA values if have no intersection, at least have 5 points to lm
          if (length(xt) < 5) { cat("linear regression points less 5 points. [failed]\n"); next}
          # lm_fit <- lm(xt ~ yt)
          # ss <- summary.lm(lm_fit)
          lm_fit <- .lm.fit(cbind(1, yt), xt) #if rank = 1 only indicate only intercept
          ss <- summary_lm(lm_fit)
          ## if lm coefficient significant
          if (lm_fit$rank < 2 || is.na(ss[2, 4]) || (ss[2, 4] > 0.05)){
            # if (ss[2, 4] > 0.05){
            cat("linear regression is not significant! [failed]\n")
          }else{
            x$data[Idx[xtrimNa.Id]] <- as.integer(cbind(1, yt = y_trim[xtrimNa.Id]) %*% lm_fit$coefficients)
            # x$data[Idx[xtrimNa.Id]] <- as.integer(predict(lm_fit, data.frame(yt = y_trim[xtrimNa.Id])))
            xtrim[[i]] <- x #update x values, use while will be better
            left_missing <- length(which(is.na(x$data)))
            ninterp <- length(which(!is.na(x$data[Idx[xtrimNa.Id]])))#interp value nums

            cat(sprintf("interped %s ~ %s %d NA values, %d left. [success]\n", ubegin, uend, ninterp, left_missing))
            if (left_missing == 0) break#breaks ith stations' interpolation
          }#endif: if linear regression is significant
        }#endif：If overlaps have missing values
      }#enfif: If date have ovelaps
    }#end Id_std for loop

  }#endfor which(stationInfo$missing > 0)
  # })#profvis

  if (!is.null(sinkfile)) sink(NULL)#解除sink
  list(xtrim = xtrim)#quickly return
}

#' @title Interp
#' @description main funtion of interpolation, combined with na.approx, linear, hisavg interpolations
#' @param data_in matrix data with dim(date*station)
#' @param smax maximum searching radius of linear interpolation (unit:km), default is 100km.
#' @param nmax maximum number stations used in linear interpolation, default is 10.
#' @param maxgap maximum gap used in naApprox interpolation
#' @param sinkfile file that used to save linear interpolation information
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
