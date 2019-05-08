library(ncdf4)
library(floodmap)
library(magrittr)
library(ncdf4)
library(lubridate)
library(stringr)

rm(list = ls())
gc()
source('R/mainfunc_transformat.R')

## global variables ---------------------------------------------------------
years <- 1951:2014

yearId <- matrix(FALSE, nrow = 31, ncol = 12)
days <- days_in_month(as.Date(sprintf("%d-%02d-01", 2001, 1:12)))
for (i in 1:12) yearId[seq(days[i]), i] <- TRUE
Id <- list()
for (i in seq_along(years)){
  tmp <- yearId
  if (leap_year(years[i])) tmp[29, 2] <- TRUE
  Id[[i]] <- as.numeric(tmp)
}
Id <- which(unlist(Id) == 1)
date_daily <- seq(as.Date("1951-01-01"), as.Date("2014-12-31"), by = "day")
ndays <- length(Id)
## ---------------------------------------------------------------------------
## same as matlab, R also treat column as priority
file <- "E:/GitHub/surface_met2474_daily.nc"

## get nc file data
info <- nc_open(file)
varsname <- names(info$var)
nvars <- length(varsname)

lat <- ncvar_get(info, "lat")
lon <- ncvar_get(info, "lon")
alt <- ncvar_get(info, "alt")
stations <- data.frame(station = paste0("v", seq_along(lat)), lat, lon, alt)
write.table(stations, file = "met2474_stations.txt", row.names = F, sep = "\t", quote = F)
nstd <- 2474#station number
## 以Tavg为例进行处理
x_new <- list()
for (i in 1:(nvars)){
  fprintf("[%d] %s\n", i, varsname[i])
  x <- ncvar_get(info, varsname[i])
  if (i <= nvars - 3){
    x <- aperm(x, 4:1)
    xx <- matrix(NA, nrow  = ndays, ncol = nstd)
    for (j in 1:nstd)
      xx[, j] <- as.numeric(x[,,,j])[Id]
    x_new[[i]] <- xx
  }else{
    x_new[[i]] <- x
  }
}
x_new <- c(x_new, list(as.numeric(date_daily)))
names(x_new) <- c(varsname, "time")
gc()

# ## detect missing information
# colnames(xx) <- paste0("v", 1:nstd)
# info <- MissInfo.matrix(xx, date_daily, showInfo = TRUE,infodays = TRUE, collapse = "\r\n", progress = FALSE)
# 
# length(which(info$missingPerc < 10))/nstd*100
# length(which(info$missingPerc < 10 & info$ndata > 365*30))
# 
# openxlsx::write.xlsx(info, "met2474_Tavg_missingInfo_kong.xlsx")

