suppressMessages({
    library(tidyverse)
    library(lubridate)
    library(magrittr)
    library(plyr)
    library(data.table)
    library(Ipaper)
    library(foreach)
    library(iterators)
})

dir_root <- "../rawdata-mete/mete840"
dir_root <- "E:/SciData/rawdata-mete/mete840"

# basename(dirs)
# > [1] "EVP" "GST" "PRE" "PRS" "RHU" "SSD" "TEM" "WIN"

# ET   | 0.1mm  | c("ET_sml", "ET_big")
# GT   | 0.1deg | c('GTavg', 'GTmax', 'GTmin')
# prcp | 0.1mm  | c('prcp20_08', 'prcp08_20', 'prcp20_20')
# P    | 0.1hPa | c('Pavg', 'Pmax', 'Pmin')
# RH   | 1%     | c('RHavg', 'RHmin')
# SSD  | 0.1h   | 'SSD'
# Tair | 0.1deg | c('Tavg', 'Tmax', 'Tmin')
# WIN  | 0.1m/s | c('WINavg', 'WINmax', 'WINmax_dir', 'WINext', 'WINext_dir')
varnames_all <- list(
    c("ET_sml", "ET_big"),
    c('GTavg', 'GTmax', 'GTmin'),
    c('prcp20_08', 'prcp08_20', 'prcp20_20'),
    c('Pavg', 'Pmax', 'Pmin'),
    c('RHavg', 'RHmin'),
    'SSD',
    c('Tavg', 'Tmax', 'Tmin'),
    c('WINavg', 'WINmax', 'WINmax_dir', 'WINext', 'WINext_dir'))

varnames_sel <- list(
  c("ET_sml", "ET_big"),
  c('GTavg', 'GTmax', 'GTmin'),
  'prcp20_20',
  c('Pavg', 'Pmax', 'Pmin'),
  c('RHavg', 'RHmin'),
  'SSD',
  c('Tavg', 'Tmax', 'Tmin'),
  c('WINavg', 'WINmax'))
