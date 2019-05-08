rm(list = ls())
# source('R/中国气象数据整理main.R', encoding = 'UTF-8')
library(openxlsx)
library(lubridate)

load("data/SPEI_InputData.rda")
fstation <- "说明文档/中国气候日志数据V3station839.xlsx"
stations <- read.xlsx(fstation)
stationId <- stations$StationNo %>% as.character()
## 对evp完全缺测的站点进行处理
evp.small <- data_out$EVP.small[stationId]
evp.big <- data_out$EVP.big[stationId]
Id_small.miss <- which(is.na(names(evp.small)))
Id_big.miss <- which(is.na(names(evp.big)))

for (i in Id_big.miss) 
  evp.big[[i]] <- list(data = NA, station = stationId[i], begin = NA, end = NA, by = "day")
for (i in Id_small.miss)
  evp.small[[i]] <- list(data = NA, station = stationId[i], begin = NA, end = NA, by = "day")

data_out$EVP.small <- evp.small
data_out$EVP.big <- evp.big

result <- list()
for (i in 1:nrow(stations)){
  cat(sprintf("[%03d]th running...\n", i))
  
  bigRange <- T
  dlist <- lapply(data_out, function(x) x[[i]])
  begins <- lapply(dlist, function(x) x$begin) %>% do.call(c, .)
  ends <- lapply(dlist, function(x) x$end) %>% do.call(c, .)
  ## 两种截取数据的方法，bigRange or smallRange
  if (bigRange){
    ubegin <- min(begins, na.rm = T)
    uend <- max(ends, na.rm = T)
  }else{
    ubegin <- max(begins, na.rm = T)
    uend <- min(ends, na.rm = T)
  }
  # 系统已经设置UseMethod("window)
  dmat <- sapply(dlist, window_dtime, ubegin, uend) %>% 
    set_rownames(format(seq(ubegin, uend, by="day"), "%Y-%m-%d"))
  df <- data.frame(time = seq(ubegin, uend, by = "day"), dmat)
  ## 对信息进行压缩
  # info <- paste(format(begins,"%Y%m%d"), format(ends,"%Y%m%d"), sep = ":")
  fname <- sprintf("data/OriginDataInput_xlsx/%03dth_%s%s.xlsx", i, stationId[i], stations$Name[i])
  # fname <- paste0("data/OriginDataInput_xlsx/", stationId[i], stations$Name[i], ".xlsx")
  write.xlsx(df, file = fname, row.names = F)
}

trim_begin <- function(begins){
  Id_begin <- which(format(begins, "%d") == "01")
  begins.show <- as.character(begins)
  if (length(Id_begin) > 0) begins.show[Id_begin] <- format(begins[Id_begin], "%Y%m")
  begins.show#quickly return
}
trim_end <- function(ends){
  days <- days_in_month(ends) %>% as.character()
  Id_end <- which(format(ends, "%d") == days)
  ends.show <- as.character(ends)
  if (length(Id_end) > 0) ends.show[Id_end] <- format(ends[Id_end], "%Y%m")
  ends.show#quickly return
}

info <- list()
for (i in 1:nrow(stations)){
  cat(sprintf("[%03d]th running...\n", i))
  dlist <- lapply(data_out, function(x) x[[i]])
  begins <- lapply(dlist, function(x) x$begin) %>% do.call(c, .)
  ends <- lapply(dlist, function(x) x$end) %>% do.call(c, .)
  ## 两种截取数据的方法，bigRange or smallRange
  ubegin.big <- min(begins, na.rm = T)
  uend.big <- max(ends, na.rm = T)
  ubegin.small <- max(begins, na.rm = T)
  uend.small <- min(ends, na.rm = T)
  info[[i]] <- c(trim_begin(begins), trim_end(ends), 
                 ubegin.big = trim_begin(ubegin.big), 
                 uend.big = trim_end(uend.big),
                 ubegin.small = trim_begin(ubegin.small), 
                 uend.small = trim_end(uend.small))
}
a <- do.call(rbind, info)
write.xlsx(a, file = "数据起止说明.xlsx")
# precip <- data_out$PRE[[i]]
# Tmax <- data_out$Tmax[[i]]
# Tmin <- data_out$Tmin[[i]]
# Tavg <- data_out$Tavg[[i]]
# U2 <- data_out$WIN[[i]] 
# tsun <- data_out$SSD[[i]]
# P <- data_out$PRS[[i]]
# RH <- data_out$RHU[[i]]
# Evp_small <- data_out$EVP.small[[i]]
# Evp_big <- data_out$EVP.big[[i]]
# GTmax <- data_out$GST.max[[i]]
# GTmin <- data_out$GST.max[[i]]
# GTavg <- data_out$GST.avg[[i]]

infolist <- lapply(1:4, function(i) lapply(stationInfo, function(x) x[[i]])) %>%
  set_names(names(stationInfo[[1]]))
fnames <- paste0(names(stationInfo[[1]]), ".xlsx")
for (i in seq_along(fnames))
  writelist_ToXlsx(infolist[[i]], fnames[i])
