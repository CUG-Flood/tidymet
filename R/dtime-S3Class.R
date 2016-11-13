# library(fields)
#' @export
begin.Date <- function(x, ...) {
  if (!is(x, "Date"))
    return("x should be date class")
  x[1]#quickly return begin date
}

#' @export
end.Date <- function(x, ...){
  if (!is(x, "Date"))
    return("x should be date class")
  x[length(x)]#quickly return end date
}

#' @export
begin <- function(x, ...) UseMethod("begin")

#' @export
end <- function(x, ...) UseMethod("end")

#' @export
seq.dtime <- function(x, ...) as.Date(x$begin:x$end)#only used for daily data
#seq(x$begin, x$end, by = x$by)
#' @export
seq_Date <- function(x, ...) as.Date(begin.Date(x):end.Date(x))#quickly return, just for daily
# seq <- function(x, ...) UseMethod("seq")
#' @export
print.dtime <- function(x, ...){
  cat(paste0("stationId", x$station, ":  "))
  cat(sprintf("begin at: %s, end at: %s, %d %ss\n", x$begin, x$end, x$datelen, x$by))
  #cat("data:\n")
  #print(head(x$data))
}

#' @export
dtime <- function(data = NULL, station = NULL, begin = Sys.Date(), end = Sys.Date(), by = "day"){
  if (!is(begin, "Date")) begin <- as.Date(begin)
  if (!is(end, "Date")) end <- as.Date(end)
  if (begin > end)
    return("begin date should be small or equal to end date!")
  if (!(by %in% c("month", "day")))
    return("by only can choose 'day' or 'month'")
  if (!is.null(data)){
    if (by == "day")   datelen <- end - begin + 1
    if (by == "month") datelen <- length(seq(begin, end, by = by))

    dims <- dim(data)
    n <- ifelse(is.null(dims), length(data), dims[1])
    if (datelen != n)
      return("data length or nrow should be equal to date length!")
  }
  structure(list(data = data, station = station, begin = begin, end = end, by = by, datelen = as.numeric(datelen)),
            class = "dtime")
}

#' @export
"window<-.dtime" <- function(x, begin, end, value)
{
  Id <- match(begin:end, seq(x))
  if (length(value) != length(Id)) error("value should be equal to begin to end date!")
  Id_nona <- Id %>% {.[which(!is.na(.))]}
  # data <- x$data
  x$data[Id_nona] <- value[Id_nona]#even if Id_nona is blank, have no side effect
  return(x)
}

#' @export
window.dtime <- function(x, begin, end){
  Id <- match(begin:end, seq(x))
  # if Id is all is.na then a na vector equal to x$data will be return
  x$data[Id]#quickly return
}

#' @title MissInfo.Single
#' @description detect one station missing infomation for single meteorological element
#' @details if all(is.na(x)) is true, then return a dtime begin and end date is 1951-01-01
#' @param x: one station meteorological one column matrix data. Rownames is daily time string
#' @param clipdata: return trim head and tail blank data
#' @param detailInfo If true detail missing information will be return, else missing information is na.
#' @param daysInfo If true missing seg days will be return
#' @param collapse missinfo info collapse
#' @export
MissInfoSingle <- function(x, station, time_day, clipdata = FALSE,
                           detailInfo = TRUE, daysInfo = TRUE, collapse = ", ")
{
  missing <- Inf; ndata = 0; beginPoint = 1; endPoint = 1; maxgap = 0;#maxgap continue Missing
  DateBegin <- "1951-01-01"#for all is na x
  DateEnd <- "1951-01-01"
  run <- rle(!is.na(x)); len <- run$lengths
  Id <- c(0, cumsum(len))

  Id_havedata <- which(run$values)
  # If having data, not blank
  if (length(Id_havedata) > 0){
    Id_nodata <- which(!run$values)#use for generate missing info

    if (length(Id_nodata) > 0){
      #delete blank values in head and tail
      if (Id_nodata[length(Id_nodata)] > Id_havedata[length(Id_havedata)]) Id_nodata <- Id_nodata[-length(Id_nodata)]
      if (length(Id_nodata) > 0 && Id_nodata[1] == 1) Id_nodata <- Id_nodata[-1]
    }
    if (detailInfo){
      # collect missing information into `info` variable
      if (length(Id_nodata) > 0){
        maxgap <- max(len[Id_nodata])
        info <- list()
        for(i in seq_along(Id_nodata)){
          beginP <- Id[Id_nodata[i]] + 1; endP <- Id[Id_nodata[i] + 1]
          if (beginP == endP){
            info[[i]] <- ifelse(daysInfo, paste0(format(time_day[beginP], "%Y%m%d"), " \t1day"),
                                format(time_day[beginP], "%Y%m%d"))
          }else{
            info[[i]] <- ifelse(daysInfo,
                                sprintf("%s, \t%ddays", paste0(format(time_day[beginP], "%Y%m%d"), "-", format(time_day[endP], "%Y%m%d")),
                                        as.numeric(time_day[endP] - time_day[beginP]) + 1),
                                paste0(format(time_day[beginP], "%Y%m%d"), "-", format(time_day[endP], "%Y%m%d")))
          }
        }
        info <- paste(unlist(info), collapse = collapse)# info <- do.call(rbind, info)
      }else{
        info = ""
      }
    }else{
      info <- NA
    }
    missing <- sum(len[Id_nodata])#if Id_nodata is null, zero will be return
    beginPoint <- Id[Id_havedata[1]] + 1; endPoint <- Id[Id_havedata[length(Id_havedata)] + 1]
    # data begin date and end date is generating
    DateBegin <- as.character.Date(time_day[beginPoint]); DateEnd <- as.character.Date(time_day[endPoint])
    ndata <- endPoint - beginPoint + 1
    info.single <- data.frame(DateBegin, DateEnd, missing, missingPerc = round(missing/ndata*100, 2), maxgap, ndata, info, stringsAsFactors = F)
  }else{
    info.single <- data.frame(DateBegin, DateEnd, missing, missingPerc = 100, maxgap, ndata, info = "", stringsAsFactors = F)
  }
  if (clipdata){
    xtrim <- dtime(data = x[beginPoint:endPoint], station = station, DateBegin, DateEnd)#construct dtime class
    list(xtrim = xtrim, info = info.single)#quickly return
  }else{
    info.single#quickly return
  }
}

#' @title MissInfo
#' @description detect all station missing infomation for single meteorological element
#' @param data matrix data with row dimemsion is date and col is stations
#' @param time_day data's responding daily date
#' @param stationId stations number id or names
#' @param clipdata if clipdata is true, x_trim will be return
MissInfo.matrix <- function(data, time_day, stationId, clipdata = FALSE, progress = TRUE, ...){
  # time_day <- as.Date(rownames(data)), change into parameter, speed up
  # stationId <- colnames(data)
  # rownames(data) <- NULL
  StationInfo <- list()
  if (progress) pb <- txtProgressBar(max = ncol(data), style = 3)
  for(i in 1:ncol(data)){
    if (progress) setTxtProgressBar(pb, i)
    StationInfo[[i]] <- MissInfoSingle(data[, i], stationId[i], time_day, clipdata, ...)
  }
  if (clipdata){
    stationInfo <- lapply(StationInfo, function(x) x$info) %>% do.call(rbind, .) %>% cbind(stationId, ., stringsAsFactors = F)
    xtrim <- lapply(StationInfo, function(x) x$xtrim) %>% set_names(stationId)
    list(stationInfo = stationInfo, xtrim = xtrim)
  }else{
    stationInfo <- do.call(rbind.data.frame, StationInfo) %>%
      cbind.data.frame(stationId, ., stringsAsFactors = F)
    stationInfo#quickly return
  }
}

#' @export
MissInfo.list <- function(data, clipdata = FALSE,  progress = TRUE, ...){
  stationId <- names(data)
  N <- length(stationId)

  StationInfo <- list()
  if (progress) pb <- txtProgressBar(max = N, style = 3)
  for(i in 1:N){
    if (progress) setTxtProgressBar(pb, i)
    x <- data[[i]]
    StationInfo[[i]] <- MissInfoSingle(x$data, stationId[i], time_day = seq(x), clipdata, ...)
  }
  if (progress) cat("\n")
  if (clipdata){
    stationInfo <- lapply(StationInfo, function(x) x$info) %>% do.call(rbind, .) %>% cbind(stationId, ., stringsAsFactors = F)
    xtrim <- lapply(StationInfo, function(x) x$xtrim) %>% set_names(stationId)
    list(stationInfo = stationInfo, xtrim = xtrim)
  }else{
    stationInfo <- do.call(rbind, StationInfo) %>% cbind(stationId, ., stringsAsFactors = F)
    stationInfo#quickly return
  }
}
MissInfo <- function(data, clipdata = FALSE, ...) UseMethod("MissInfo")

# data interpolation by na.approx, history mean and linear regress
#' @title naHisAvgSingle
#' @description using history average ti replace missing values
#' @param x dtime class
#' @export
naHisAvgSingle <- function(x){
  Id <- which(is.na(x$data))#Modified Dongdong Kong, 2016-04-23
  if (length(Id) > 0){
  	time_day <- seq(x)
	tmp <- aggregate(x$data, list(doy = format(time_day, "%m-%d")), mean, na.rm = T)
	tmp$x <- round(tmp$x, 1)
	x$data[Id] <- tmp$x[match(format(time_day[Id], "%m-%d"), tmp$doy)]
  }
  x#quickly return
}

#' @export
naHisAvg <- function(xx, stationInfo, ...){
  Id <- which(stationInfo[, "missing"] > 0)
  pb <- txtProgressBar(max = max(length(Id), 1), style = 3)
  for(i in seq_along(Id)){
    xx[[Id[i]]] <- naHisAvgSingle(xx[[Id[i]]])
    setTxtProgressBar(pb, i)
  }
  xx#quickly return
}

naApproxSingle <- function(x, maxgap = 5, ...) {x$data <- round(na.approx(x$data, maxgap = maxgap), 1); x}
naApprox <- function(xx, stationInfo, maxgap = 5){
  # Id <- which(stationInfo$maxgap <= maxgap), Modified By Dongdong Kong, 2016-04-23
  Id <- which(stationInfo[, "missing"] > 0)

  pb <- txtProgressBar(max = length(Id), style = 3)
  for(i in seq_along(Id)){
    xx[[Id[i]]] <- naApproxSingle(xx[[Id[i]]])
    setTxtProgressBar(pb, i)
  }
  xx#quickly return
}

#' @title linearInterp
#' @param x: dtime class whose data have missing values
#' @param StationInfo: single one stationInfo returned by Missinfo function
#' @param dist: Station distance from each other dim = (n * n)
#' @export
linearInterp <- function(xtrim, stationInfo, dist, nmax = 10, sinkfile = NULL){
  Id_needInterp <- which(stationInfo[, "missing"] > 0)
  InterpInfo <- list()
  if (!is.null(sinkfile)) sink(sinkfile)
  for (i in Id_needInterp) {
    x <- xtrim[[i]]
    xtime <- seq(x)#time_day will not alter with interpolate
    nmissing <- length(which(is.na(x$data)))
    cat(sprintf('[%03d] =============================\n', i))
    print(x)
    cat(sprintf('missing :\t%s\n', stationInfo[i, "info"]))

    if (nmissing == 0){
      cat("\tna.approx interpolate end\n")
    }else{
      dist.tmp <- dist[i, ]
      ## find nearest met station Id
      id <- order(dist.tmp, na.last = NA)
      if (length(id) > nmax) id <- id[1:nmax]#only the first nmax element left
      cat("------------------------------------\n")
      cat("nearest stations used to interpolate ...\n")
      print(cbind(stationInfo[id, 1:6], dist = dist.tmp[id]))
      cat("------------------------------------\n")
      interpi.info <- list(numeric(length(id)))
      for (j in seq_along(id)){
        y <- xtrim[[id[j]]]
        station <- y$station
        # if x and y full data have no date intersection, then next
        if ((x$begin > y$end | y$begin > x$end)){
          # cat(sprintf("\t%02dth: failed! station %s having no intersection date!\n", j, station))
        }else{
          ubegin <- max(x$begin, y$begin)
          uend <- min(x$end, y$end)#repair error, Dongdong Kong, 2016-04-23
          utime <- as.Date(ubegin:uend)#seq(ubegin, uend, by = "day")

          xNa.Date <- xtime[which(is.na(x$data))]
          if ((end(xNa.Date) < ubegin) | (begin(xNa.Date) > uend)) {
            # cat(sprintf("\t%02dth: failed! station%s date have no overlap with missing values!\n", j, station))
            next#if have no overlap then exit this station
          }
          ## modified 20161021
          Idx <- match(utime, xtime)
          Idy <- match(utime, seq(y))
          x_trim <- x$data[Idx]
          y_trim <- y$data[Idy]
          xtrimNa.Id <- which(is.na(x_trim))

          # modified 20161021, if y can fix x's partial missing value then use it.
          if (length(which(!is.na(y_trim) & is.na(x_trim))) == 0) {
            cat(sprintf("\t%02dth: failed! station%s also have missing values in the corresponding missing date!\n", j, station))
          }else{
            # lm_fit <- lm(x ~ y, data = data.frame(x = xx_trim, y = yy_trim))
            idn <- !(is.na(y_trim) | is.na(x_trim))#remove nan values
            xt <- x_trim[idn]
            yt <- y_trim[idn]
            lm_fit <- lm(xt ~ yt)
            ss <- summary.lm(lm_fit)
            ## if lm coefficient significant
            if (ss$coefficients[2, 4] > 0.05){
              # cat(sprintf("\t%02dth: failed! station%s linear regression is not significant!\n", j, station))
            }else{
              x$data[Idx[xtrimNa.Id]] <- round(predict(lm_fit, data.frame(yt = y_trim[xtrimNa.Id])), 1)
              xtrim[[i]] <- x #update x values, use while will be better
              left_missing <- length(which(is.na(x$data)))
              ninterp <- length(which(!is.na(x$data[Idx[xtrimNa.Id]])))#interp value nums

              # ubegin, uend, xtrimNa.Date, Interp stations, Interp nums, left missing values nums
              interpi.info[[i]] <- data.frame(UseStation = station, ubegin, uend, ninterp, left_missing)
              # cat(sprintf("\t%02dth: success! station%s interp %s ~ %s totally %d missing values, %d missing remaining\n",
              # j, station, begin(xtrimNa.Date), end(xtrimNa.Date), ninterp, left_missing))
              if (left_missing == 0){
                #cat(sprintf('[%03d] ============== end ============\n', i))
                break#break can exit for loop ignore many If layers
              }
            }#endif: if linear regression is significant
          }#endif：If overlaps have missing values
        }#enfif: If date have ovelaps
      }#endfor interpolate station
      InterpInfo[[i]] <- interpi.info
    }#endif stationInfo[i, "missing"] > 0
  }
  if (!is.null(sinkfile)) sink(NULL)#解除sink
  list(xtrim = xtrim, InterpInfo = InterpInfo)#quickly return
}
#' @title Interp
#' @description main funtion of interpolation, combined with na.approx, linear, hisavg interpolations
#' @param data_in matrix data with dim(date*station)
#' @param smax maximum searching radius of linear interpolation (unit:km), default is 100km.
#' @param nmax maximum number stations used in linear interpolation, default is 10.
#' @param maxgap maximum gap used in naApprox interpolation
#' @param sinkfile file that used to save linear interpolation information
#' @export
Interp <- function(data_in, dist, smax = 100, nmax = 10, maxgap = 5, time_day, stationId,
  sinkfile1 = NULL, sinkfile2 = NULL, ...){
  cat("[00]. detect original data missing information ...\n")
  Info <- MissInfo.matrix(data_in, clipdata = T, time_day, stationId, ...)
  stationInfo <- Info$stationInfo
  xtrim <- Info$xtrim
  progress <- FALSE#add parameter
  dist[(dist > smax) | (dist < 0.001)] <- NA

  cat("\n[1.1]. runing na.approx interpolate ...\n")
  ## 1 using naapprox to interpolate missing values which gap less than maxgap
  xx_naapprox <- naApprox(xtrim, stationInfo, maxgap)
  cat("\n[1.2]. detect missing info after na.approx interpolate ...\n")
  stationInfo_naapprox <- MissInfo.list(xx_naapprox, progress = progress, ...)

  ## 2 after naApprox, using linear Interpolation (it's significant if p < 0.05 in our interpolation)
  cat("[2.1_1] runing linear lm interpolate ...\n")
  xx <- linearInterp(xx_naapprox, stationInfo_naapprox, dist = dist, nmax, sinkfile = sinkfile1)
  xx_linear <- xx$xtrim
  cat("[2.1_2] detect missing info after first lm interpolate ...\n")
  stationInfo_linear <- MissInfo.list(xx_linear, progress = progress, ...)

  ## 2.2 try linear interpolation again
  # cat("Running : second linear interpolation\n")
  cat("[2.2_1] runing second linear lm interpolate ...\n")
  xx <- linearInterp(xx_linear, stationInfo_linear, dist, nmax, sinkfile = sinkfile2)
  xx_linear <- xx$xtrim
  cat("[2.2_2] detect missing info after second lm interpolate ...\n")
  stationInfo_linear2 <- MissInfo.list(xx_linear, progress = progress, ...)

  cat("[3.1] history average interpolate...\n")
  # save(xx_linear, stationInfo_linear2, file = "InterpBug.rda")#debug
  xx_hisavg <- naHisAvg(xx_linear, stationInfo_linear2)
  cat("\n[3.2] detect missing info after hisavg interpolate ...\n")
  stationInfo_hisavg <- MissInfo.list(xx_hisavg, progress = progress, ...)

  # return every step detected missing info
  stationInfo_list <- list(origin = stationInfo, naApprox = stationInfo_naapprox, linear = stationInfo_linear,
                           hisavg = stationInfo_hisavg)
  list(stationInfo = stationInfo_list, x = xx_hisavg)#quickly return
}
