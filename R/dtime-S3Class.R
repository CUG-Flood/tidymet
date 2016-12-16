#' @title rdist.earth
#' @export
rdist.earth <- function (x1, x2 = NULL)
{
  R <- 6378.388
  # coslat1 <- cos(x1[, 2])
  # sinlat1 <- sin(x1[, 2])
  # coslon1 <- cos(x1[, 1])
  # sinlon1 <- sin(x1[, 1])
  coslat1 <- cos((x1[, 2] * pi)/180)
  sinlat1 <- sin((x1[, 2] * pi)/180)
  coslon1 <- cos((x1[, 1] * pi)/180)
  sinlon1 <- sin((x1[, 1] * pi)/180)

  coslat2 <- cos((x2[, 2] * pi)/180)
  sinlat2 <- sin((x2[, 2] * pi)/180)
  coslon2 <- cos((x2[, 1] * pi)/180)
  sinlon2 <- sin((x2[, 1] * pi)/180)
  pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) %*%
    t(cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2))
  return(R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp)))
}


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

# ' @export
# # end <- function(x, ...) UseMethod("end")

#' @export
seq.dtime <- function(x, ...) as.Date(x$begin:x$end, origin = "1970-01-01")#only used for daily data
#seq(x$begin, x$end, by = x$by)
#' @export
seq_Date <- function(x, ...) as.Date(begin.Date(x):end.Date(x), origin = "1970-01-01")#quickly return, just for daily
# seq <- function(x, ...) UseMethod("seq")
#' @export
print.dtime <- function(x, ...){
  cat(paste0("stationId ", x$station, ":  "))
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

#' @export
plot.dtime <- function(x, ...){
  plot(time = seq(x), value = x$data, ...)
}

#' @title MissInfo.default
#' @description detect one station missing infomation for single meteorological element
#' @details if all(is.na(x)) is true, then return a dtime begin and end date is 1951-01-01
#' @param x one station meteorological one column matrix data. Rownames is daily time string
#' @param clipdata return trim head and tail blank data
#' @param detailInfo If true detail missing information will be return, else missing information is na.
#' @param daysInfo If true missing seg days will be return
#' @param collapse missinfo info collapse
#' @export
MissInfo.default <- function(x, station, time_day, clipdata = FALSE,
                           detailInfo = TRUE, daysInfo = TRUE, collapse = ", ")
{
  missing <- NA; ndata = 0; beginPoint = 1; endPoint = 1; mingap <- maxgap <- 0;#maxgap continue Missing
  missingPerc = 100
  info <- ""; xtrim = NULL
  #if x is blank, dateBegin and dateEnd set to be "1951-01-01"
  DateBegin <- "1951-01-01"
  DateEnd <- "1951-01-01"
  run <- rle(!is.na(x)); len <- run$lengths
  Id <- c(0, cumsum(len))

  Id_havedata <- which(run$values)
  # If having data, not blank
  if (length(Id_havedata) > 0){
    Id_nodata <- which(!run$values)#use for generate missing info

    #delete blank values in head and tail
    if (length(Id_nodata) > 0){
      if (Id_nodata[length(Id_nodata)] > Id_havedata[length(Id_havedata)]) Id_nodata <- Id_nodata[-length(Id_nodata)]
      if (length(Id_nodata) > 0 && Id_nodata[1] == 1) Id_nodata <- Id_nodata[-1]
    }

    # collect missing information into `info` variable
    if (length(Id_nodata) > 0){
      maxgap <- max(len[Id_nodata])
      mingap <- min(len[Id_nodata])

      if (detailInfo){
        info <- list()
        for(i in seq_along(Id_nodata)){
          beginP <- Id[Id_nodata[i]] + 1; endP <- Id[Id_nodata[i] + 1]
          if (beginP == endP){
            infoi <- format(time_day[beginP], "%Y%m%d")
            info[[i]] <- ifelse(daysInfo, paste0(infoi, " \t1day"), infoi)
          }else{
            infoi <- paste0(format.Date(time_day[beginP], "%Y%m%d"), "-", format.Date(time_day[endP], "%Y%m%d"))
            info[[i]] <- ifelse(daysInfo, sprintf("%s, \t%ddays", infoi, endP - beginP + 1), infoi)
          }
        }
        info <- paste(unlist(info), collapse = collapse)# info <- do.call(rbind, info)
      }#endif detailInfo
    }#endif length(Id_nodata) > 0
    beginPoint <- Id[Id_havedata[1]] + 1; endPoint <- Id[Id_havedata[length(Id_havedata)] + 1]
    DateBegin <- as.character.Date(time_day[beginPoint]); DateEnd <- as.character.Date(time_day[endPoint])
    ndata <- endPoint - beginPoint + 1
    missing <- sum(len[Id_nodata])#if Id_nodata is null, zero will be return
    missingPerc = round(missing/ndata*100, 2)
  }
  info <- data.frame(DateBegin, DateEnd, missing, missingPerc, mingap, maxgap, ndata, info, stringsAsFactors = F)

  if (clipdata) xtrim <- dtime(data = x[beginPoint:endPoint], station = station, DateBegin, DateEnd)#construct dtime class
  return(list(info = info, xtrim = xtrim))#quickly return
}

#' @title MissInfo.dtime
#' @export
MissInfo.dtime <- function(x, ...) MissInfo.default(x$data, x$station, time_day = seq(x), ...)
#' @title MissInfo
#' @description detect all station missing infomation for single meteorological element
#' @param data matrix data with row dimemsion is date and col is stations
#' @param time_day data's responding daily date
#' @param stationId stations number id or names
#' @param clipdata if clipdata is true, NAs at head and tail will be trimed. And trimed data will be return
#' @export
MissInfo.matrix <- function(data,
                            time_day = as.Date(rownames(data)),
                            stationId = colnames(data),
                            clipdata = FALSE,
                            progress = TRUE,
                            ...) {
	# time_day change into parameter, speed up
	# rownames(data) <- NULL
  if (.parallel) .progress = "none"
  progress <- create_progress_bar(.progress)
  progress$init(n); on.exit(progress$term())

	stationInfo <- if (.parallel){
	  foreach(x = data, station = stationId) %dopar% MissInfo::MissInfo.default(x, station, time_day, clipdata, ...)
	}else{
	  foreach(x = data, station = stationId) %do%{
	    progress$step()
	    MissInfo::MissInfo.default(x, station, time_day, clipdata, ...)
	  }
	}
	xtrim <- if (clipdata) lapply(stationInfo, `[[`, 'xtrim') %>% set_names(stationId) else NULL
	Info <- lapply(stationInfo, `[[`, 'info') %>% do.call(rbind, .) %>% cbind(stationId, ., stringsAsFactors = F)
	return(list(stationInfo = stationInfo, xtrim = xtrim))
}

#' @title MissInfo.list
#' @export
MissInfo.list <- function(xlist, clipdata = FALSE, .progress = "text", .parallel = FALSE, ...){
  stationId <- names(xlist)
  if (.parallel) .progress = "none"
  progress <- create_progress_bar(.progress)
  progress$init(n); on.exit(progress$term())

  stationInfo <- if (.parallel){
    foreach(x = xlist, i = icount(), .inorder = FALSE) %dopar%
      MissInfo::MissInfo.default(x$data, x$station, time_day = seq(x), clipdata, ...)
  }else{
    foreach(x = xlist, i = icount()) %do%{
      progress$step()
      MissInfo::MissInfo.default(x$data, x$station, time_day = seq(x), clipdata, ...)
    }
  }
  xtrim <- if (clipdata) lapply(stationInfo, `[[`, 'xtrim') %>% set_names(stationId) else NULL
  Info <- lapply(stationInfo, `[[`, 'info') %>% do.call(rbind, .) %>% cbind(stationId, ., stringsAsFactors = F)
  return(list(stationInfo = Info, xtrim = xtrim))
}

#' @export
MissInfo <- function(data, clipdata = FALSE, ...) UseMethod("MissInfo")

