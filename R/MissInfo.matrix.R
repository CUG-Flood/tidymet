#' @param mat matrix data with row dimemsion is date and col is stations
#'
#' @rdname MissInfo
#' @example man/examples/ex-MissInfo.R
#'
#' @import iterators
#' @export
MissInfo.matrix <- function(mat,
    time_day = as.Date(rownames(mat)),
    site = colnames(mat),
    clipdata = FALSE,
    verbose = TRUE,
    .parallel = FALSE,
    ...)
{
    N <- ncol(mat); step <- floor(N/10)
    FUN <- ifelse(.parallel, `%dopar%`, `%do%`)

    res <- FUN(foreach(x = mat, station = site, i = icount()), {
        if (verbose) runningId(i, step, N, "MissInfo.matrix")
        MissInfo.default(x[, 1], station, time_day, clipdata, ...)
    }) %>% set_names(site)

    xtrim <- if (clipdata) { map(res, "xtrim") } else NULL
    info <- map(res, "info") %>% do.call(rbind, .) %>%
        cbind(site, ., stringsAsFactors = F)
    return(list(info = info, xtrim = xtrim))
}

#' @inheritParams interp_main
#'
#' @rdname MissInfo
#' @export
MissInfo.data.frame <- function(
    df,
    clipdata = FALSE,
    verbose = TRUE,
    .parallel = FALSE,
    ...)
{
    time_day <- df$date
    site <- colnames(df)[-1]
    mat  <- df[, -1] %>% as.matrix()
    MissInfo.matrix(mat, clipdata = clipdata, time_day, site, verbose = verbose)
}

#' @param lst list of dtime object
#'
#' @rdname MissInfo
#' @export
MissInfo.list <- function(lst, clipdata = FALSE,
    verbose = TRUE,
    .parallel = FALSE, ...)
{
    site <- names(lst)
    N <- length(site); step <- floor(N/10)

    FUN <- ifelse(.parallel, `%dopar%`, `%do%`)
    res <- FUN(foreach(x = lst, station = site, i = icount()), {
        if (verbose) runningId(i, step, N, "MissInfo.list")
        MissInfo.default(x$data, x$station, time_day = seq(x), clipdata, ...)
    }) %>% set_names(site)

    xtrim <- if (clipdata) { map(res, "xtrim") } else NULL
    info <- map(res, "info") %>% do.call(rbind, .) %>%
        cbind(site, ., stringsAsFactors = F)
    return(list(info = info, xtrim = xtrim))
}
