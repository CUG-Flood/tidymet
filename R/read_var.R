#' read files of China Meteorological data V3
#' 
#' @param indir Directory of txt files
#' @param varname meteorological variable names
#' @param drop Numeric, which variable to be droped
#' @param limits Numeric, If not null, only read the first \code{limits} files.
#' 
#' @export 
read_var <- function(indir, varname = NULL, drop = NULL, limits = NULL){
    files <- dir(indir, '*.txt|TXT', full.names = T)
    if (!is.null(limits)) files <- files[1:limits]

    df <- llply(files, fread, drop = drop, .progress = "text") %>%
        do.call(rbind, .)
    if (!is.null(varname)) {
        names <- c('site', 'lat', 'long', 'alt', 'year', 'month', 'day',
                   varname, paste0('QC_', varname))
        if (!is.null(drop)) names <- names[-drop]
        colnames(df) <- names
        df[, `:=`(date = make_date(year, month, day), 
            year = NULL, month = NULL, day = NULL)]
    }
    reorder_name(df, c("site", "date"))
}


reorder_name <- function(
    d,
    headvars = c("site", "date", "year", "month", "day", "doy", "d8", "d16"),
    tailvars = "")
{
    names <- names(d)
    headvars %<>% intersect(names)
    tailvars %<>% intersect(names)
    varnames <- c(headvars, setdiff(names, union(headvars, tailvars)), tailvars)

    if (is.data.table(d)) {
        d[, varnames, with = F]
    } else if (is.data.frame(d)) {
        d[, varnames]
    } else if (is.list(d)){
        d[varnames]
    } else{
        stop("Unknown data type!")
    }
}
