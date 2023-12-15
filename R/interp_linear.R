#' interp_linear
#'
#' @param xx list of dtime objects which have missing values
#' @param stationInfo stationInfo object returned by Missinfo function
#' @param dist Station distance from each other dim = (n * n)
#' @param smax maximum searching radius (km) of linear interpolation, default is 100km.
#' @param nmax maximum number stations used in linear interpolation, default is 10.
#' @param sinkfile file that used to save linear interpolation information
#' @param ... ignored
#'
#' @export
interp_linear <- function(xx, stationInfo, dist, smax = 100, nmax = 10, sinkfile = NULL, ...) {
  dist[dist > smax | (dist < 0.001)] <- NA

  I_needInterp <- which(stationInfo$n_miss > 0)
  interp_info <- list()
  if (!is.null(sinkfile)) {
    sink(sinkfile)
    on.exit(sink(NULL))
  }

  for (i in I_needInterp) {
    x <- xx[[i]]
    xtime <- seq(x) # time_day will not alter with interpolate
    nmissing <- length(which(is.na(x$data)))
    cat(sprintf("[%03d] =============================\n", i))
    print(x)
    cat(sprintf("missing :\t%s\n", stationInfo[i, "info"]))

    if (nmissing == 0) {
      cat("\tna.approx interpolate end\n")
    } else {
      dist.tmp <- dist[i, ]
      ## find nearest met station Id
      id <- order(dist.tmp, na.last = NA)
      if (length(id) > nmax) {
        id <- id[1:nmax]
      } # only the first nmax element left

      cat("------------------------------------\n")
      cat("nearest stations used to interpolate: \n")
      print(cbind(stationInfo[id, 1:6], dist = dist.tmp[id]))
      cat("------------------------------------\n")

      interp_infoI <- vector("list", length(id))
      for (j in seq_along(id)) {
        y <- xx[[id[j]]]
        station <- y$station
        utime <- dtime_intersect(x, y)

        # if x and y has no overlap
        if (length(utime) == 0) {
          # cat(sprintf('\t%02dth: failed! station %s has no overlap!\n', j, station))
        } else {
          ubegin <- min(utime)
          uend <- max(utime)

          date_miss <- xtime[which(is.na(x$data))]

          if (min(date_miss) > uend || ubegin > max(date_miss)) {
            # cat(sprintf('\t%02dth: failed! no overlap with site [%s] !\n', j, station))
            next
          }

          Idx <- match(utime, xtime)
          Idy <- match(utime, seq(y))
          x_trim <- x$data[Idx]
          y_trim <- y$data[Idy]
          Ix_miss <- which(is.na(x_trim))
          # modified 20161021, if y can fix x's partial missing value also use it.
          if (length(which(!is.na(y_trim) & is.na(x_trim))) == 0) {
            cat(sprintf("\t%02dth: failed! site %s: corresponding dates also missing!\n", j, station))
          } else {
            # yt -> predict xt, xt is response variable
            idn <- !(is.na(y_trim) | is.na(x_trim)) # remove nan values
            xt <- x_trim[idn]
            yt <- y_trim[idn]

            # lm_fit <- lm(xt ~ yt) ss <- summary.lm(lm_fit)$coefficients
            lm_fit2 <- .lm.fit(cbind(1, yt), xt)
            ss2 <- summary_lm(lm_fit2)

            ## if lm coefficient significant
            if (ss2[2, 4] > 0.05) {
              # cat(sprintf('\t%02dth: failed!  站点%s线性拟合不能通过显著性检验!\n', j,
              # station))
            } else {
              ypred2 <- cbind(1, y_trim[Ix_miss]) %*% lm_fit2$coefficients %>%
                as.numeric()
              # ypred <- predict(lm_fit, data.frame(yt = y_trim[Ix_miss]))
              x$data[Idx[Ix_miss]] <- round(ypred2, 1)
              xx[[i]] <- x # update x values, use while will be better
              n_missLeft <- length(which(is.na(x$data)))
              n_interp <- length(which(!is.na(x$data[Idx[Ix_miss]])))
              # interp value nums # ubegin, uend, xxNa.Date, 插值站点, 插值个数，剩余空值个数
              interp_infoI[[j]] <- data.frame(
                site = station, ubegin, uend, n_interp, n_missLeft,
                dist = dist.tmp[id][j]
              )
              # cat(sprintf('\t%02dth: success! 站点%s插值%s ~ %s共%d个值，余留%d空值\n', j,
              # station, begin(date_miss), end(date_miss), n_interp, n_missLeft))
              if (n_missLeft == 0) {
                # cat(sprintf('[%03d] ============== end ============\n', i))
                break # break可以忽略多重if跳出for loop
              }
            } # endif: 线性拟合是否显著
          } # endif：重合部分，插值站点是否含有空值
        } # enfif: 日期是否有重合
      } # endfor interpolate station
      interp_infoI <- do.call(rbind, rm_empty(interp_infoI))
      print(interp_infoI)
      interp_info[[i]] <- interp_infoI
    } # endif stationInfo[i, 'missing'] > 0
  }

  list(xtrim = xx, interp_info = interp_info)
}
