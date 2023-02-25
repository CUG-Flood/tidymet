#! /usr/bin/Rscript
# 1. 处理特殊值flags
library(Ipaper)
# library(plyr)
# library(glue)
# library(foreach)
# library(iterators)
library(lubridate)
library(missInfo)
library(purrr)
library(matrixStats)
# source("test/process.R")

# obs_types <- c("avg", "max", "min")
# ------------------------------------------------------------------------------
dir_root <- "Z:/DATA/China/2400climate data" %>% path.mnt()
dir_root <- "Z:/DATA/China/2400climate data/202001-202103气象数据"

varnames <- c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")
# merge_mete2000_txts(dir_root, is_save = TRUE)

lst_varnames <- list(
  EVP = c("EVP_sm", "EVP_bg"),
  PRE = c("Prcp_20-08", "Prcp_02-20", "Prcp_20-20"),
  RHU = c("RH_avg", "RH_min"),
  WIN = c("WIN_Avg", "WIN_S_Max", "WIN_D_S_Max", "WIN_INST_Max", "WIN_D_INST_Max"),
  SSD = "SSD",
  GST = c("TG_avg", "TG_max", "TG_min"),
  TEM = c("Tair_avg", "Tair_max", "Tair_min"),
  PRS = c("Pa_avg", "Pa_max", "Pa_min")
)
