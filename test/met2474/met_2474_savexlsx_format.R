library(ncdf4)
library(floodmap)
library(magrittr)
library(data.table)
library(lubridate)

rm(list = ls())
source('R/mainfunc_transformat.R')
source("R/MissingInfo/dtime-S3Class.R", encoding = "utf-8")

file_stations <- "met2474/met2474_stations.txt"
# time <- info$dim$time$vals %>% as.Date(origin = "1970-01-01")
stations <- read.table(file_stations, header = T, stringsAsFactors = F)
stationId <- stations$station
nstd <- nrow(stations)
outdir <- "met2474/"

## 对插值后的数据进行整理
fdirs <- dir(outdir, pattern = "*.rda", full.names = T)
for (indir in fdirs) load(indir)
varnames <-  gsub("met2474_Interp_|.rda","", basename(fdirs)) %>% gsub("-", "_", .)
eval(parse(text = sprintf("met <- list(%s)", paste0(varnames, collapse = ", "))))
names(met) <- varnames

stationInfo <- lapply(met, `[[`, "stationInfo")
xx_interp <- lapply(met, `[[`, "x")
rm(list = c(varnames, "met"))

## save missig info
save(stationInfo, file = "met2474/met2474_stationInfo.rda")
info.list <- lapply(1:4, function(i) lapply(stationInfo, `[[`, i)) %>% set_names(c("origin", "naApprox", "linear", "hisavg"))
info <- lapply(info.list, function(x) do.call(cbind, x)[, -seq(9, 128, 8)])

x1 <- c("station", rep(colnames(info.list$origin$EVAP)[-1], 16))
x2 <- rep("", 113);x2[seq(2, 113, 7)] <- varnames
write.table(rbind(x1, x2), file = "header.txt", quote = F, row.names = F, sep = "\t")
## 按站点把数据进行保存
result <- list()
for (i in 1:nrow(stations)){
  cat(sprintf("[%03d]th running...\n", i))
  
  bigRange <- T
  dlist <- lapply(xx_interp, `[[`, i)
  begins <- lapply(dlist, `[[`, "begin") %>% do.call(c, .)
  ends <- lapply(dlist, `[[`, "end") %>% do.call(c, .)
  ## 两种截取数据的方法，bigRange or smallRange
  if (bigRange){
    ubegin <- min(begins, na.rm = T)
    uend <- max(ends, na.rm = T)
  }else{
    ubegin <- max(begins, na.rm = T)
    uend <- min(ends, na.rm = T)
  }
  # 系统已经设置UseMethod("window)
  dmat <- sapply(dlist, window.dtime, ubegin, uend) #%>% 
    # set_rownames(format(seq(ubegin, uend, by="day"), "%Y-%m-%d"))
  df <- data.table(time =format(as.Date(ubegin:uend)), dmat, stringsAsFactors = F)
  ## 对信息进行压缩
  # info <- paste(format(begins,"%Y%m%d"), format(ends,"%Y%m%d"), sep = ":")
  fname <- sprintf("met2474/met2474_csv/%03dth_%s.csv", i, stationId[i])
  # fname <- paste0("data/OriginDataInput_xlsx/", stationId[i], stations$Name[i], ".xlsx")
  fwrite(df, fname, quote = F, sep = ",")
  result[[i]] <- df
}