#' The distance of locations from the first-time location
#' @export 
get_dist <- function(lon, lat) {
    P = cbind(lon, lat) %>% deg2dec()
    if (nrow(P) > 1) {
        rdist.earth(P[1,,drop = FALSE], P[-1,,drop = FALSE])[1, ] %>% c(0, .)
    } else 0
}

#' Mode number
#' @export 
mode <- function(x) {
    table(x) %>% sort(decreasing = TRUE) %>% .[1] %>% names() %>% as.numeric()
}

#' @export 
reflag <- function(d) {
    d$tag = d[, lon^2 + lat^2 + alt^2] %>% {c(1, abs(diff(.)) > 0.1)} %>% cumsum()
    # I_fix = which(table(flag) > 1)
    d[, `:=`(
        # tag = tag[1],
        period_date_begin = min(period_date_begin),
        period_date_end = max(period_date_end),
        n_period = sum(n_period)
    ), .(tag)]
    # for(i in I_fix) {
    #     ind = flag == i
    #     d[ind, `:=`(tag = tag[1],
    #                 period_date_begin = min(period_date_begin),
    #                 period_date_end = max(period_date_end),
    #                 n_period = sum(n_period))]
    # }
    d <- d[!duplicated(tag), ] %>% .[,`:=`(moveTimes = .N, tag = 1:.N)]
    # print(d)
    d
    # d
}


fix_wrong_location <- function(d) {
    I_bad = mark_outlier(d$dist, 2) %>% which.na()
    I_lat_bad = max_diff(d$lat) %>% mark_outlier(2) %>% which.na()
    I_lon_bad = max_diff(d$lon) %>% mark_outlier(2) %>% which.na()

    if (length(I_bad) > 0) {
        lat0 = mode(d$lat)
        lon0 = mode(d$lon)

        if (all.equal(I_bad, I_lat_bad) == TRUE) {
            d[I_lat_bad, lat := lat0]
        }
        if (all.equal(I_bad, I_lon_bad) == TRUE) {
            d[I_lon_bad, lon := lon0]
        }
    }
    d
}
