#' read meteorological data
#'
#' This function is for original txt file from .
#'
#' @param file txt file path. The txt file is from http://data.cma.cn/. The
#' first 7 columns should be the data of `c("site", "lat", "lon", "alt", "year",
#' "month", "day")`.
#' @param ... other parameters to [fread()]
#'
#' @examples
#' lst_varnames = list(
#'     EVP = c("EVP_sm", "EVP_bg"),
#'     PRE = c("Prcp_20-08", "Prcp_02-20", "Prcp_20-20"),
#'     RHU = c("RH_avg", "RH_min"),
#'     WIN = c("WIN_Avg","WIN_S_Max", "WIN_D_S_Max", "WIN_INST_Max", "WIN_D_INST_Max"),
#'     SSD = "SSD",
#'     GST = c("TG_avg", "TG_max", "TG_min"),
#'     TEM = c("Tair_avg", "Tair_max", "Tair_min"),
#'     PRS = c("Pa_avg", "Pa_max", "Pa_min")
#' )
#' @export
read_mete <- function(file, lst_varnames, ...) {
    var <- guess_variable(file)
    varnames <- lst_varnames[[var]]
    varnames <- c(varnames, paste0("QC.", varnames)) # add qc
    df <- fread(file, ...)

    vars_common <- c("site", "lat", "lon", "alt", "year", "month", "day")
    colnames(df) <- c(vars_common, varnames)
    df[df == 32766] <- NA_integer_
    df %>%
        dplyr::mutate(date = make_date(year, month, day)) %>%
        # dplyr::select(-year, -month, -day) %>%
        .[, -(2:7)] %>%
        reorder_name(c("site", "date"))
}

#' @rdname read_mete
#' @import data.table
#' @export
write_mete <- function(df, prefix = "", date_end = NULL, overwrite = FALSE) {
    mkdir(dirname(prefix))
    sites = df$site %>% unique() %>% sort()
    # if (is.null(date_end)) {
    #     l <- split(df, df$site)
    # } else {
    #     l <- split(df[date <= date_end], df[date <= date_end, site])
    # }
    temp <- foreach(SITE = sites, i = icount()) %do% {
        runningId(i, 20)
        outfile <- glue("{prefix}{SITE}.csv")
        if (!file.exists(outfile) || overwrite) {
            d = df[site == SITE]
            if (!is.null(date_end)) d = d[date <= date_end]
            # site <- d$site[1]
            fwrite(d, outfile)
        }
    }
    invisible()
}

#' @rdname read_mete
#' @export
guess_variable <- function(file) str_extract(basename(file), "(?<=_DAY_).*(?= )")

#' @rdname read_mete
#' @export
not_select_QC <- . %>% dplyr::select(!starts_with("QC."))
