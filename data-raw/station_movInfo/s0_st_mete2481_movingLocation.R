#! /usr/bin/Rscript
library(plyr)
library(glue)
library(foreach)
library(iterators)
library(lubridate)
library(missInfo)
library(purrr)
library(matrixStats)

read_data <- function(file, I_sel = 1:13) {
    fread(file, select = I_sel) %>% set_colnames(vars_common[I_sel])
}


# dir_root <- path.mnt("N:/DATA/China/2400climate data")
varnames <- c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")
# merge_mete2000_txts(dir_root, is_save = TRUE)
vars_common <- c("site", "lat", "lon", "alt", "year", "month", "day")

files = dir(dir_root, "*.csv", full.names = TRUE) %>% set_names(varnames)

# lst <- map(files[7], read_data)
df <- read_data(files[7])
obs_types <- c("avg", "max", "min")
prefix    <- "T"
varnames  <- c(paste0(prefix, obs_types), paste0("QC_", prefix, obs_types))
colnames(df)[8:13] <- varnames
df[df == 32766] = NA_integer_
df[, date := make_date(year, month, day)]

# 2019以后，站点位置错误较多，不予采用
# 经纬度重新洗牌，避免出现114°60'这种现象
st <- df[, .(site, lat, lon, alt, date)] %>% st_loc_shuffle()

st_full <- st[date <= as.Date("2018-12-31")]
st_moveInfo <- get_moveInfo(st_full)
fwrite(st_moveInfo, "data-raw/mete2481_站点变迁记录-(195101-201812).csv")
# st_moveInfo <- revise_locaiton_multi(st_moveInfo_raw, dist_max = 50)

st_moveInfo_raw = fread("data-raw/mete2481_站点变迁记录-(195101-201812).csv")
st_met2481 = st_moveInfo_raw %>% dt_ddply(.(site), ~ top_n(., 1, n_period) %>% last())
# use_data(st_met2481)

# st_moveInfo_raw <- get_moveInfo(st_full)
st_moveInfo <- revise_locaiton_multi(st_moveInfo_raw, dist_max = 50)
