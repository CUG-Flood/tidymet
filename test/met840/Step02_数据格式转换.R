rm(list = ls())
source('R/中国气象数据整理main.R', encoding = 'UTF-8')

fstation <- "说明文档/中国气候日志数据V3station839.xlsx"
stations <- read.xlsx(fstation)

## global functions

## 根据fdirs, 数据的整理顺序为:"EVP" "GST" "PRE" "PRS" "RHU" "SSD" "TEM" "WIN"
fdirs <- dir("../data/中国气象数据共享中心/", full.names = T)
fpaths <- lapply(fdirs, dir, full.names = T)

##对该函数进行加强，增加其可移植性
stationId <- stations$StationNo
dir_output <- "output/"

info <- list()
info[[1]] <- list(zcol = 8:9, zcolname = c("EVP.big", "EVP.small"))# 1蒸发EVP
info[[2]] <- list(zcol = 8:10, zcolname = c("GST.avg", "GST.max", "GST.min"))# 2地表温度GST
info[[3]] <- list(zcol = 10, zcolname = c("Precp"))# 3降水PRE, 20-20时降水
info[[4]] <- list(zcol = 8:10, zcolname = c("PRS.avg", "PRS.max", "PRS.min"))# 4气压PRS
info[[5]] <- list(zcol = 8:9, zcolname = c("RHU.avg", "RHU.min"))# 5相对湿度RHU
info[[6]] <- list(zcol = 8, zcolname = c("SSD"))# 6光照时数SSD "TEM" "WIN"
info[[7]] <- list(zcol = 8:10, zcolname = c("Tavg", "Tmax", "Tmin"))# 7温度TEM
info[[8]] <- list(zcol = 8:12, zcolname = c("WIN.avg", "WIN.max", "WIN.maxd", "WIN.ext", "WIN.extd"))# 8风速WIN

for (i in seq_along(info)){
  ii <- info[[i]]
  zcol <- ii$zcol
  zcolname <- ii$zcolname
  fnames <- fpaths[[i]]
  ChangeFormat_element(fnames, zcol, zcolname, stationId, dir_output)
}