#' The distance of locations from the center
#'
#' @export
distToCentralPeriod <- function(lon, lat, n_period) {
  P <- cbind(lon, lat) %>%
    deg2dec()
  i <- which.max(n_period) #

  if (nrow(P) > 1) {
    dist <- rdist.earth(P[i, , drop = FALSE], P[, , drop = FALSE])[1, ]
    round(dist, 2)
  } else {
    0
  }
}

#' location matched score
#'
#' @return
#' - `3`: all of `lon`, `lat` and `alt` matched
#' - `2`: two of `lon`, `lat` and `alt` matched
#' - `1`: one of `lon`, `lat` and `alt` matched
locMatchedScore <- function(x, y) {
  (x$lon == y$lon) + (x$lat == y$lat) + (abs(x$alt - y$alt) <= 0.005)
}



#' Revise meteorological station's location error
#'
#' @param d A data.frame with the columns at least of `dist`, `lon`, `lat`, `alt`
#' and `n_period`, `QC`
#' @param vars_rm variables not shown in the console
#' @param score_min if distance score lower than `score_min`, will not fix
#'
#' @return
#' - sites with not outliers, QC = '';
#' - sites with outliers, for outliers, QC equals:
#'   score = 0: unfixed
#'   score = 1: marginal quality fixed
#'   score = 2: good quality fixed
#'
#' @import glue
#' @importFrom crayon green
#' @export
revise_locaiton <- function(d, prefix = "", dist_max = 100, score_min = 2, vars_rm = c(
                              "date_begin", "date_end",
                              "n_all"
                            ), verbose = 2) {
  vars <- setdiff(colnames(d), vars_rm)
  n <- length(d$dist)
  # grps = cumsum(1, diff(d$dist >= dist_max) != 0)
  inds <- which(d$dist >= dist_max)
  grps <- if (length(inds) == 1) {
    1
  } else {
    cumsum(c(1, diff(inds) != 1))
  }
  # how many continuous periods?
  ngrp <- max(grps)

  # dist > `dist_max` is regarded as outlier which will be replaced with the nearest good location
  num <- 0
  for (i in 1:ngrp) {
    ind <- inds[grps == i]

    i_prev <- ind[1] - 1
    i_next <- ind[length(ind)] + 1
    if (i_prev < 1) {
      i_prev <- NULL
    }
    if (i_next > n) {
      i_next <- NULL
    }
    ind_inspect <- c(i_prev, ind, i_next)
    i_candinate <- c(i_prev, i_next)

    for (j in ind) {
      tag <- d$tag[ind[j]]
      dist <- d$dist[j]
      scores <- locMatchedScore(d[i_candinate, ], d[j, ])
      i_opt <- i_candinate[which.max(scores) %>%
        last()]
      score <- max(scores)
      # if (dist >= 500), we have to fix it
      is_good <- score >= 2 || (dist > 500 && score >= 1) # confidential fix
      is_print <- verbose == 1 || (verbose >= 2 && !is_good)
      if (is_print) {
        num <- num + 1
        if (num == 1) {
          ok(glue("[{prefix}] ======================================="))
        }
        cat(glue("    [tag={tag}] -------------------------------------------"), "\n")
        print(d[ind_inspect, ..vars])
        fprintf("---\n")
      }

      is_low_score <- FALSE
      if (is_good) {
        d$lat[j] <- d$lat[i_opt]
        d$lon[j] <- d$lon[i_opt]
        d$alt[j] <- d$alt[i_opt]
        d$QC[j] <- "good"
      } else {
        # warn(glue('[w]: location matched score={score}'))
        if (score >= score_min) {
          d$lat[j] <- d$lat[i_opt]
          d$lon[j] <- d$lon[i_opt]
          d$alt[j] <- d$alt[i_opt]
          d$QC[j] <- "marginal"
        } else {
          d$QC[j] <- ifelse(score != 0, "suspicious", "unfixed")
          warn(glue("[w]: tag={tag} {d$QC[j]}, low location matched score ({score})"))
          is_low_score <- TRUE
        }
      }
      d[, dist := distToCentralPeriod(lon, lat, n_period)]
      if (is_print && !is_low_score) {
        print(d[ind_inspect, ..vars])
      }
    }
  }
  d
  # listk(d, status)
}


#' @param info site moving info returned by [get_moveInfo()]
#' @param dist_max If the site moving distance beyond the `dist_max`, it will
#' be regarded as outlier and will be fixed by [revise_location()].
#'
#' @details
#' Continuous moving location outliers are also considered at here.
#'
#' @examples
#' d <- st_moveInfo[site == 50772]
#' revise_locaiton_multi(d)
#'
#' @rdname revise_locaiton
#' @export
revise_locaiton_multi <- function(info, dist_max = 50) {
  ## revise record error greater than one month
  info <- info[n_period >= 28, ] %>%
    get_moveInfo()

  info$QC <- ""
  sites <- info[dist >= dist_max]$site %>%
    unique()
  info_bad <- info[site %in% sites, ]

  temp <- foreach(sitename = sites, i = icount()) %do% {
    d <- info[site == sitename, ]
    # d$QC = 'raw'
    prefix <- sprintf("%02dth:%s", i, sitename)
    r <- revise_locaiton(d, prefix, dist_max)
    # r$status
    r
  }
  # %>% reorder_name(c('site', 'moveTimes', 'tag', 'lon', 'lat', 'alt', 'dist', 'n_period', 'QC'))
  df <- do.call(rbind, temp)

  d_fixed <- df[QC %in% c("good", "margin"), .N, .(site)] # %>% nrow()
  d_unsure <- df[QC %in% c("suspicious"), .N, .(site)] # %>% nrow()
  d_unfixed <- df[QC %in% c("unfixed"), .N, .(site)]
  sites_bad <- d_unsure$site
  sites_unfixed <- setdiff(d_unfixed$site, d_unsure$site)
  # n_fixed = length(sites) - length(sites_bad)
  ok(sprintf(
    "[info] %d sites fixed, %d sites unfixed, %s sites not sure", nrow(d_fixed), length(sites_unfixed),
    length(sites_bad)
  ))

  info_final <- rbind(info[!(site %in% sites)], df)
  get_moveInfo(info_final) # retidy moveinfo and delete duplicated TPs
  # info_final sites_bad = sites[which(unlist(temp) == 'bad')] sites_bad = c('51058', '52378',
  # '52607', '52884', '53730', '54287')
}
