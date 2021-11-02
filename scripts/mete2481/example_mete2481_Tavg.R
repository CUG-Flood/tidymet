#! /usr/bin/Rscript
library(plyr)
library(glue)
library(foreach)
library(iterators)
library(lubridate)
library(missInfo)
library(purrr)
library(matrixStats)
# source("test/process.R")

# ------------------------------------------------------------------------------
dir_root <- "N:/DATA/China/2400climate data" %>% path.mnt()
varnames <- c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")
# merge_mete2000_txts(dir_root, is_save = TRUE)
vars_common = c("site", "lat", "lon", "alt", "year", "month", "day")

# InitCluster(14, kill = FALSE)

# 2019-2020年数据异常，观测数据不是完整的一整月的数据，
# df = lst$TEM
# file_met = "OUTPUT/mete2481_Tavg_daily (195101-202003).rda"
# if (!file.exists(file_met)) {
    files = dir(dir_root, "*.csv", full.names = TRUE) %>% set_names(varnames)
    I_sel = 1:13
    read_data <- function(file) {
        fread(file, select = I_sel) %>% set_colnames(vars_common[I_sel])
    }
    # lst <- map(files[7], read_data)
    df <- read_data(files[7])
    obs_types <- c("avg", "max", "min")
    prefix    <- "T"
    varnames  <- c(paste0(prefix, obs_types), paste0("QC_", prefix, obs_types))
    colnames(df)[8:13] <- varnames
    df[df == 32766] = NA_integer_
    df[, date := make_date(year, month, day)]
    # 337 unchanged: lon^2 + lat^2

    # process(df, varname = "Tavg", overwrite = FALSE)
    ## In the second time, input data will be cached, df can be NULL
    process(df = NULL, varname = "Tavg", overwrite = FALSE)
# }

{
    st = df[, .(site, lat, lon, alt, date)] %>%
        st_loc_shuffle()
}
