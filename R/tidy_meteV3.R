#' handle of meteorological flags
#' 
#' @param df data.frame
#' 
#' @examples
#' data('mete840')
#' df1 <- tidy_meteV3(mete840)
#' df2 <- tidy_meteV3(mete840[, 1:10])
#' @export
tidy_meteV3 <- function(df){
    varnames <- colnames(df)
    # fix alt
    df[alt > 1e5L, alt := alt - 1e5L] 
    df[, alt := alt/10] # avoid conflict with 32766
    
    df[df == 32766] <- NA

    # modified according to reference
    tidy_FUN <- function(vars, FUN){
        vars <- intersect(vars, varnames)
        if (length(vars) > 0) {
            df[, (vars) := lapply(.SD, FUN), .SDcols = vars]
        }
    }

    fix_ET  <- . %>% replace_value(value = 32700) %>% flag_realvalue(values = 1000)
    tidy_FUN(c("ET_big", "ET_sml"), fix_ET)
    
    # G temp
    vars_G <- c("GTmax", "GTmin", "GTavg")
    fix_G <- . %>% flag_realvalue(values = 10000)
    tidy_FUN(vars_G, fix_G)

    # precipitation
    fix_prcp <- . %>% replace_value(value = 32700, newval = 0.1) %>%
      flag_realvalue(values = c(32000, 31000, 30000))
    tidy_FUN("prcp20_20", fix_prcp)

    # WIND
    fix_WIN <- . %>% flag_realvalue(values = 1000)
    tidy_FUN(c("WINavg", "WINmax"), fix_WIN)

    # Pressure
    vars_P <- c("Pmax", "Pmin", "Pavg")
    fix_P <- . %>% flag_realvalue(values = 20000)
    tidy_FUN(vars_P, fix_P)
    
    # RH
    fix_RH <- . %>% flag_realvalue(values = 300)
    tidy_FUN(vars = c("RHmin", "RHavg"), fix_RH)
    
    df
}
