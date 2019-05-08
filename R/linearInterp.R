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
  for (i in Id) {
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