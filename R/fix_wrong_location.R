#' @export
reflag <- function(d) {
  d[, tag := (lon^2 + lat^2 + alt^2) %>%
    {
      c(1, abs(diff(.)) > 0.1)
    } %>% cumsum(), .(site)]
  # I_fix = which(table(flag) > 1)
  d[, `:=`(
    # tag = tag[1],
    period_date_begin = min(period_date_begin),
    period_date_end = max(period_date_end),
    n_period = sum(n_period)
  ), .(site, tag)]

  # remove dist
  d2 <- unique(d[, 1:12]) %>%
    .[, `:=`(
      moveTimes = .N, tag = 1:.N,
      dist = distToCentralPeriod(lon, lat, n_period)
    ), .(site)]
  d2
}


fix_wrong_location <- function(d) {
  I_bad <- mark_outlier(d$dist, 2) %>% which.na()
  I_lat_bad <- max_diff(d$lat) %>%
    mark_outlier(2) %>%
    which.na()
  I_lon_bad <- max_diff(d$lon) %>%
    mark_outlier(2) %>%
    which.na()

  if (length(I_bad) > 0) {
    lat0 <- mode(d$lat)
    lon0 <- mode(d$lon)

    if (all.equal(I_bad, I_lat_bad) == TRUE) {
      d[I_lat_bad, lat := lat0]
    }
    if (all.equal(I_bad, I_lon_bad) == TRUE) {
      d[I_lon_bad, lon := lon0]
    }
  }
  d
}

get_sitenames <- function(sites) {
  names <- foreach(sitename = sites, i = icount(), .combine = c) %do% {
    runningId(i, 10)
    tryCatch(
      {
        url <- glue::glue("https://weather.cma.cn/api/autocomplete?q={sitename}")
        l <- GET(url) %>% content()
        l$data[[1]]
      },
      error = function(e) {
        message(sprintf("%s", e$message))
        NA_character_
      }
    )
  }
  names
}

max_diff <- function(x) {
  # c(0, diff(x)) %>% abs()
  x2 <- x - x[1]
  x2
}
