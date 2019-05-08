library(ncdf4)
library(floodmap)
library(magrittr)
library(data.table)
library(lubridate)

rm(list = ls())
source('R/mainfunc_transformat.R')

file_nc <- "met2474_kong.nc"
file_stations <- "met2474_stations.txt"

info <- nc_open(file_nc)
#ignore last three: lat, lon, alt variable
varnames <- names(info$var); nvars <- length(varnames) - 3; varnames <- varnames[1:nvars]
time <- info$dim$time$vals %>% as.Date(origin = "1970-01-01")
stations <- read.table(file_stations, header = T, stringsAsFactors = F)
stationId <- stations$station
# stations <- fread(file_stations, header = T, sep = "\t")
nstd <- nrow(stations)
dist <- rdist.earth(stations[, 3:2], miles = F)#long, lat
outdir <- "met2474/"
## 数据气象值特殊标记已经进行了处理
# Tavg <- ncvar_get(info, "T2M")
# Tavg_min <- apply(Tavg, 2, min, na.rm = T)
# Tavg_max <- apply(Tavg, 2, max, na.rm = T)
# 
# prcp_min <- apply(prcp, 2, min, na.rm = T)
# prcp_max <- apply(prcp, 2, max, na.rm = T)
Interp_Parallel <- function(var, dist, outdir){
  info <- nc_open("met2474_kong.nc")
  xx <- ncvar_get(info, var)
  varname <- gsub("-", "_", var)
  sinkfile1 <- paste0("met2474/log/", var, "_1.txt")
  sinkfile2 <- paste0("met2474/log/", var, "_2.txt")
  eval(parse(text =  sprintf("%s <- Interp(xx, dist, smax = 100, nmax = 10, maxgap = 5, 
                       time, stationId, sinkfile1, sinkfile2)", varname)))
  eval(parse(text = sprintf("save(%s, file = '%smet2474_Interp_%s.rda')", varname, outdir, var)))
  eval(parse(text = sprintf("rm('%s')", varname)))
  gc(); gc()
  # return(X)
}
# ignore data have been dealed
varnames <- varnames[!(varnames %in% gsub("met2474_Interp_|.rda","",dir(outdir, pattern = "*.rda")))]
# profvis::profvis({ X <- Interp(xx, dist, smax = 100, nmax = 10, maxgap = 5, time, stationId) })

cl <- makeCluster(7, type = "SOCK", outfile = "log.txt")
tmp <- clusterEvalQ(cl, {library(ncdf4);source("R/MissingInfo/dtime-S3Class.R", encoding = "utf-8")})
clusterExport(cl, c("time", "stationId"), envir = globalenv())#quickly return

tm <- snow.time(result <- parLapply(cl, varnames, Interp_Parallel, dist, outdir))
plot(tm)
stopCluster(cl); gc()