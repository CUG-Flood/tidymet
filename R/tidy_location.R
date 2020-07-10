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

#' @export
deg2dec <- function(x){
    deg <- floor(x/100)
    deg + (x - deg*100)/60
}

#' @export
dec2deg <- function(x) {
    int <- floor(x)
    other <- x - int
    int * 100 + round(other * 60)
}

#' @export 
get_alt <- function(x) {
    x[x >= 100000] <- x[x >= 100000] - 100000 
    x/10
}
