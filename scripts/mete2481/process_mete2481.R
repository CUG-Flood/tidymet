#! /usr/bin/Rscript
# 1. 处理特殊值flags
library(plyr)
library(glue)
library(foreach)
library(iterators)
library(lubridate)
library(missInfo)
library(purrr)
library(matrixStats)
# source("test/process.R")

# obs_types <- c("avg", "max", "min")
# ------------------------------------------------------------------------------
dir_root <- "N:/DATA/China/2400climate data" %>% path.mnt()
varnames <- c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")
# merge_mete2000_txts(dir_root, is_save = TRUE)

lst_varnames = list(
    EVP = c("EVP_sm", "EVP_bg"),
    PRE = c("Prcp_20-08", "Prcp_02-20", "Prcp_20-20"),
    RHU = c("RH_avg", "RH_min"),
    WIN = c("WIN_Avg","WIN_S_Max", "WIN_D_S_Max", "WIN_INST_Max", "WIN_D_INST_Max"),
    SSD = "SSD",
    GST = c("TG_avg", "TG_max", "TG_min"),
    TEM = c("Tair_avg", "Tair_max", "Tair_min"),
    PRS = c("Pa_avg", "Pa_max", "Pa_min")
)

read_mete(files[2], nrow = 1e5)
lst = llply(files, read_mete, lst_varnames, .progress = "text")
df = reduce(lst, merge, by = c("site", "date"), all.x = TRUE)
# 54016857
tailvars = df %>% dplyr::select(starts_with("QC.")) %>% colnames()
df %<>% reorder_name(tailvars = tailvars)

# df %>% select(!starts_with("QC."))

library(Ipaper)
outdir = path.mnt("/mnt/h/China_Latest_Meteorological_Data/2400climate\ data")
outfile = glue("{outdir}/ChinaMeteDaily_SURF_CLI_CHN_MUL_DAY-[195101,202003]_rawfile.csv")

fwrite(df, outfile)

## 1. 代表性站点 ---------------------------------------------------------------
outfile = "Z:/DATA/China/2400climate data/ChinaMeteDaily_SURF_CLI_CHN_MUL_DAY_[195101,202003]_rawfile.csv"
df = fread(outfile)

# df = fread(files[1], nrows = 1e5)
# 北京、武汉、广州
d = df[site %in% c(54511, 57494, 59287, 58449)] %>% 
    .[year(date) <= 2019]
# write_mete(d, "rawfile_")
tidy_mete2000(d)
write_mete(d %>% not_select_QC(), "processed_", date_end = "2019-12-31")

fwrite(df, glue("{outdir}/ChinaMeteDaily_SURF_CLI_CHN_MUL_DAY_[195101,202003]_processed.csv"))

## 2. 代表性站点 ---------------------------------------------------------------
tidy_mete2000(df)
# %>% not_select_QC()
write_mete(df, "OUTPUT/ChinaMetDaily_st2481_[195101,202003]/", date_end = NULL, overwrite = TRUE)
