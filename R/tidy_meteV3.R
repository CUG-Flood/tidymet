#' handle of meteorological flags
#'
#' @param df data.frame
#'
#' @examples
#' data("mete840")
#' df1 <- tidy_meteV3(mete840)
#' df2 <- tidy_meteV3(mete840[, 1:10])
#' @export
tidy_meteV3 <- function(df) {
  varnames <- colnames(df)
  # fix alt
  if ("alt" %in% varnames) {
    df[alt > 100000L, alt := alt - 100000L]
    df[, alt := alt / 10] # avoid conflict with 32766
  }

  df[df == 32766] <- NA_integer_

  # modified according to reference
  replaceExceed <- function(vars, FUN) {
    vars <- intersect(vars, varnames)
    if (length(vars) > 0) {
      df[, (vars) := lapply(.SD, FUN), .SDcols = vars]
    }
  }

  replaceExceed(c("ET_big", "ET_sml"), fix_ET)

  # G temp
  vars_G <- c("GTmax", "GTmin", "GTavg")
  replaceExceed(vars_G, fix_G)

  # precipitation
  replaceExceed(c("prcp20_08", "prcp08_20", "prcp20_20"), fix_prcp)

  # WIND
  replaceExceed(c("WINavg", "WINmax", "WINext"), fix_WIN)

  # Pressure
  vars_P <- c("Pmax", "Pmin", "Pavg")
  fix_P <- . %>%
    cal_exceed(values = 20000L)
  replaceExceed(vars_P, fix_P)

  # RH
  replaceExceed(vars = c("RHmin", "RHavg"), fix_RH)

  df
}


#' @export
tidy_mete2000 <- function(df) {
  varnames <- colnames(df)
  # modified according to reference
  replaceExceed <- function(vars, FUN) {
    vars <- intersect(vars, varnames)
    if (length(vars) > 0) {
      df[, (vars) := lapply(.SD, FUN), .SDcols = vars]
    } else {
      stop("invalid")
    }
  }

  fprintf("[1]. running ET ...\n")
  replaceExceed(c("EVP_sm"), fix_ET) # ET_bg has 999 is normal

  types <- c("avg", "max", "min")
  fprintf("[2]. running TG ...\n")
  # G temp
  vars_TG <- paste0("TG_", types[-1])
  replaceExceed(vars_TG, fix_G)

  # precipitation
  fprintf("[3]. running Prcp ...\n")
  vars_prcp <- paste0("Prcp_", c("20-08", "02-20", "20-20"))
  # vars_prcp = c('prcp20_08', 'prcp08_20', 'prcp20_20')
  replaceExceed(vars_prcp, fix_prcp)

  # WIND
  fprintf("[4]. running Wind ...\n")
  replaceExceed(c("WIN_S_Max", "WIN_INST_Max"), fix_WIN) # 'WIN_Avg',

  # Pressure
  fprintf("[5]. running Pa ...\n")
  vars_P <- paste0("Pa_", types)
  replaceExceed(vars_P, fix_P)

  # RH
  fprintf("[6]. running RH ...\n")
  replaceExceed(vars = c("RH_min"), fix_RH)

  df
}
