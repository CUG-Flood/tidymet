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

