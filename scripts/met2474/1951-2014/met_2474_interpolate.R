source('test/main_pkgs.R')
library(ncdf4)

file_nc <- "met2474_kong.nc"
file_stations <- "met2474_stations.txt"

info <- nc_open(file_nc)
#ignore last three: lat, lon, alt variable
varnames <- names(info$var); nvars <- length(varnames) - 3; varnames <- varnames[1:nvars]
time <- info$dim$time$vals %>% as.Date(origin = "1970-01-01")
stations <- read.table(file_stations, header = T, stringsAsFactors = F)
site <- stations$station
# stations <- fread(file_stations, header = T, sep = "\t")
nstd <- nrow(stations)
dist <- rdist.earth(stations[, 3:2], miles = F)#lon, lat
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
  eval(parse(text =  sprintf("%s <- interp_main(xx, dist, smax = 100, nmax = 10, maxgap = 5, 
                       time, site, sinkfile1, sinkfile2)", varname)))
  eval(parse(text = sprintf("save(%s, file = '%smet2474_Interp_%s.rda')", varname, outdir, var)))
  eval(parse(text = sprintf("rm('%s')", varname)))
  gc(); gc()
  # return(X)
}
# ignore data have been dealed
varnames <- varnames[!(varnames %in% gsub("met2474_Interp_|.rda","",dir(outdir, pattern = "*.rda")))]
# profvis::profvis({ X <- interp_main(xx, dist, smax = 100, nmax = 10, maxgap = 5, time, site) })

cl <- makeCluster(7, type = "SOCK", outfile = "log.txt")
tmp <- clusterEvalQ(cl, {library(ncdf4)})
clusterExport(cl, c("time", "site"), envir = globalenv())#quickly return

tm <- snow.time(result <- parLapply(cl, varnames, Interp_Parallel, dist, outdir))
plot(tm)
stopCluster(cl); gc()