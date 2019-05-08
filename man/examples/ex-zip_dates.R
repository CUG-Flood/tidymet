library(lubridate)
data("station_HisPos")
d <- station_HisPos[site == 50136, ]
dates <- d[, make_date(year, month, day)]

s1 <- zip_dates(dates)
s2 <- zip_dates(dates, by = "year", check_duplicate = TRUE)
