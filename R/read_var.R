#' read files of China Meteorological data V3
#'
#' @param indir Directory of txt files
#' @param varname Character vector, meteorological variable names
#' @param var_left Character vector, The part of left variables among varname
#' @param var_drop Character vector, droped variables
#' @param limits Numeric, If not null, only read the first \code{limits} files.
#' @inheritParams qc_mask
#' 
#' @note This function is only designed for China Meteorological data V3.
#' @export
read_var <- function(indir, varname = NULL, var_left, var_drop = NULL, 
    limits = NULL, QCmin = NULL)
{
    files <- dir(indir, '*.txt|TXT', full.names = T)
    if (!is.null(limits)) files <- files[1:limits]

    names_head  <- c('site', 'lat', 'lon', 'alt', 'year', 'month', 'day')
    names_var   <- varname
    names_varQC <- paste0('QC_', varname)
    names       <- c(names_head, names_var, names_varQC)

    var_drop <- setdiff(varname, var_left) %>% union(var_drop) # removed variables
    var_drop <- intersect(names, c(var_drop, paste0('QC_', var_drop)))

    if (length(var_drop) > 0) {
        drop <- match(var_drop, names)
    } else {
        drop <- NULL
    }

    df <- llply(files, fread, drop = drop, .progress = "text") %>%
        do.call(rbind, .)

    if (!is.null(varname)) {
        if (!is.null(drop)) {
            names      %<>% setdiff(var_drop)
            names_head %<>% setdiff(var_drop)
            names_var  %<>% setdiff(var_drop)
            names_varQC%<>% setdiff(var_drop)
        }

        colnames(df) <- names
        df[, `:=`(date = make_date(year, month, day),
            year = NULL, month = NULL, day = NULL)]

        names_head <- setdiff(c('site', 'date','lat', 'lon', 'alt'), var_drop)
        mat_var   <- df[, ..names_var]
        mat_varQC <- df[, ..names_varQC]
        mat_var[mat_var == 32766] <- NA_integer_ # na value
        
        # mask data with QC > QCmin
        if (!is.null(QCmin)) {
            mat_var[mat_varQC > QCmin] <- NA_integer_
        }
        df <- data.table(df[, ..names_head], mat_var, mat_varQC)
    }
    df
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
