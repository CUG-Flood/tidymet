#' @param mat matrix data with row dimemsion is date and col is stations
#'
#' @rdname missInfo
#' @example man/examples/ex-missInfo.R
#'
#' @import iterators
#' @export
missInfo.matrix <- function(mat,
    date = as.Date(rownames(mat)),
    site = colnames(mat),
    clipdata = FALSE,
    verbose = TRUE,
    .parallel = FALSE,
    ...)
{
    N <- ncol(mat); step <- floor(N/10)
    FUN <- ifelse(.parallel, `%dopar%`, `%do%`)

    res <- FUN(foreach(x = mat, station = site, i = icount()), {
        if (verbose) runningId(i, step, N, "missInfo.matrix")
        missInfo.default(x[, 1], date, station, clipdata, ...)
    }) %>% set_names(site)

    xtrim <- if (clipdata) { map(res, "xtrim") } else NULL
    info <- map(res, "info") %>% do.call(rbind, .) %>%
        cbind(site, ., stringsAsFactors = F)
    return(list(info = info, xtrim = xtrim))
}

#' @inheritParams interp_main
#'
#' @rdname missInfo
#' @export
missInfo.data.frame <- function(
    df,
    clipdata = FALSE,
    verbose = TRUE,
    .parallel = FALSE,
    ...)
{
    date <- df$date
    site <- colnames(df)[-1]
    mat  <- df[, -1] %>% as.matrix()
    missInfo.matrix(mat, clipdata = clipdata, date, site, verbose = verbose)
}

#' @param lst list of dtime object
#'
#' @rdname missInfo
#' @export
missInfo.list <- function(lst, clipdata = FALSE,
    verbose = TRUE,
    .parallel = FALSE, ...)
{
    site <- names(lst)
    N <- length(site); step <- floor(N/10)

    FUN <- ifelse(.parallel, `%dopar%`, `%do%`)
    res <- FUN(foreach(x = lst, station = site, i = icount()), {
        if (verbose) runningId(i, step, N, "missInfo.list")
        missInfo.default(x$data, date = seq(x), x$station, clipdata, ...)
    }) %>% set_names(site)

    xtrim <- if (clipdata) { map(res, "xtrim") } else NULL
    info <- map(res, "info") %>% do.call(rbind, .) %>%
        cbind(site, ., stringsAsFactors = F)
    return(list(info = info, xtrim = xtrim))
}
