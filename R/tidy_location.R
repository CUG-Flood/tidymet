# 对经纬度、高程信息进行转换
#' @export
tidy_location <- function(x){
    varnames <- colnames(x)
    x$lat %<>% dec2deg()
    if ("lon" %in% varnames) x$lon %<>% dec2deg()    
    if ("long" %in% varnames) x$long %<>% dec2deg()    
    
    if ("alt" %in% varnames && (class(x$alt) == "integer")) {
        x$alt[x$alt >= 1e5] <- x$alt[x$alt >= 1e5] - 1e5
        x$alt <- x$alt/10
        # x$alt <- x$alt/10
    }
    x
}

# x: 11242
dec2deg <- function(x){
    deg <- floor(x/100)
    deg + (x - deg*100)/60
}
