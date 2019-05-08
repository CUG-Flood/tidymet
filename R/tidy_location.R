# 对经纬度、高程信息进行转换
#' @export
tidy_location <- function(x){
    x$lat <- (x$lat - floor(x$lat/100)*100)/60 + floor(x$lat/100)
    x$long <- (x$long - floor(x$long/100)*100)/60 + floor(x$long/100)
    x$alt[x$alt >= 1e5] <- x$alt[x$alt >= 1e5] - 1e5
    # x$alt <- x$alt/10
    x
}
