#' fix_alt
#' For China meteorological data V3
#' @export
fix_alt <- function(d){
    if ("alt" %in% colnames(d)) {
        d[alt > 1e5L, alt := alt - 1e5L]    
    }
    d
}

#' @export
rm_duplicate <- function(d) {
    I <- {!duplicated(d[, 1:4])} %>% which()
    d_pos <- d[I, ] %>% fix_alt()
    d_pos[order(site), ]
}

#' @export
replace_value <- function(x, value = 32766, newval = NA_integer_) {
    x[x == value] <- newval
    x
}

#' flag_realvalue
#'
#' @param x numeric or matrix
#' @param values Should be in descend order
flag_realvalue <- function(x, values){
    for (i in seq_along(values)){
        value <- values[i]
        x[which(x >= value)] %<>% subtract(value)
    }
    return(x)
}
