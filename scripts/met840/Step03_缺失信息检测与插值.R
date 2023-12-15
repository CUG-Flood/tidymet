
infile <- "data/CMA_origin_daily839.rda"
if (file.exists(infile)){
  load(infile)
}else{
  fnames <- dir("output/", pattern = "*.rda", full.names = T)
  ## 这里只处理计算SPEI相关的要素
  tmp <- sapply(1:8, function(i) load(fnames[i], .GlobalEnv))
  rm(list = c("tmp", "WIN.ext", "WIN.extd", "WIN.max", "WIN.maxd", "PRS.max", "PRS.min", "RHU.min"))

  # save(Precp, Tmax, Tmin, Tavg, SSD, WIN.avg, RHU.avg, PRS.avg, EVP.small, EVP.big,
  #      GST.max, GST.min, GST.avg, file = infile)
  varnames <- c("PRE", "Tmax", "Tmin", "Tavg", "SSD", "WIN", "PRS", "RHU", "EVP.small", "EVP.big", 
                "GST.max", "GST.min", "GST.avg")
  data_in <- list(Precp, Tmax, Tmin, Tavg, SSD, WIN.avg, PRS.avg, RHU.avg, EVP.small, EVP.big,
                  GST.max, GST.min, GST.avg) %>% set_names(varnames)
  sinkfiles <- paste("log/", varnames, "_INTERP", sep = "")
  for (i in seq_along(data_in)) 
    data_in[[i]] <- list(x = data_in[[i]], sinkfile = sinkfiles[i])
  
  save(data_in, varnames, sinkfiles, file = infile)
  rm(list = setdiff(ls(), c("data_in", "varnames", "sinkfiles")))
  gc(); gc()
  # cat("Saving met839 vars splitly ...\n")
  # for (i in seq_along(data_in)){
  #   cat(sprintf("[%d] %s ------------\n", i, varnames[i]))
  #   data_in[[i]] <- list(x = data_in[[i]], sinkfile = sinkfiles[i])
  #   save(data_in[[i]], file = sprintf("data/met839_%s_Origin.rda", varnames[i]))
  # }
}

# 程序已经进行强化，可以接受输入数据全空的站点
# trim_blank <- function(data){
#   Id <- which(apply(data, 2, function(x) length(which(!is.na(x)))) == 0) %>% as.numeric()
#   if (length(Id) > 0){
#     data[, -Id]#quickly return
#   }else{
#     data#quickly return
#   }
# }
# EVP.small_trim <- trim_blank(EVP.small)
# EVP.big_trim <- trim_blank(EVP.big)

fstation <- "说明文档/中国气候日志数据V3station839.xlsx"
stations <- read.xlsx(fstation)
dist <- rdist.earth(stations[, 3:2])#lon, lat
## ---------------------- prepare for parallel compute -------------------
site <- stations$StationNo
time_day <- as.Date("1951-01-01"):as.Date("2016-06-30") %>% as.Date()

varnames <- c("PRE", "Tmax", "Tmin", "Tavg", "SSD", "WIN", "PRS", "RHU", "EVP.small", "EVP.big", 
              "GST.max", "GST.min", "GST.avg")
met <- list()
for (i in 7:length(data_in)){
  cat(sprintf("[%d] %s ------------\n", i, varnames[i]))
  met[[i]] <- interp_main(data_in[[i]]$x, dist, smax = 400, nmax = 10, maxgap = 5, time_day, site)
}

## -------------------- PARALLEL COMPUTE -----------------------
cl <- makeCluster(8, type = "SOCK", outfile = "log.txt")
tmp <- clusterEvalQ(cl, source('R/dtime-S3Class.R', encoding = 'UTF-8'))
clusterExport(cl, c("time_day", "site"))

Interp_Parallel <- function(data, dist){
  x <- data$x
  sinkfile <- data$sinkfile
  interp_main(x, dist, smax = 400, nmax = 10, maxgap = 5, time_day, site,
         sinkfile1 = paste0(sinkfile, "1.log"), sinkfile2 = paste0(sinkfile, "2.log"))
}

tm <- snow.time(met <- parLapply(cl, data_in, Interp_Parallel,dist))
plot(tm)
stopCluster(cl); gc()

names(met) <- varnames
stationInfo <- lapply(met, `[[`, "stationInfo")
xx_interp <- lapply(met, `[[`, "x")

## -------------------- CHECK DATA AFTER INTERPOLATE --------------------
par(mfrow = c(2, 3))
## 检查数据插值结果
for(i in seq_along(xx_interp))
  plot(sapply(xx_interp[[i]], function(x) max(x$data)), type = "b", ylab = varnames[i])

## 异常RHU处理
RHU_modify <- function(x){
  Id <- which(x$data > 100); x$data[Id] <- 100
  x#quickly return
}
Id_error <- which(sapply(xx_interp$RHU, function(x) length(which(x$data > 100))) > 0)
tmp <- lapply(xx_interp$RHU[Id_error], RHU_modify)
xx_interp$RHU[Id_error] <- tmp
# which(sapply(xx_interp$RHU, function(x) max(x$data)) > 100)
# 56691 56872 56986 57084 
# 449   462   475   493 
## save finally return
file_interp <- "data/CMA_met839_daily_AfterInterp.rda"
save(xx_interp, stationInfo, file = file_interp)

## ----------------- save into csv files, and cbind variables ------------------
rm(list = ls())

file_interp <- "data/CMA_met839_daily_AfterInterp.rda"
file_station <- "说明文档/中国气候日志数据V3station839.xlsx"
stations <- read.xlsx(file_station)
load(file_interp)

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
  fname <- sprintf("met839_csv/%03dth_%s%s.csv", i, stations[i, 1], stations[i, "Name"])
  # fname <- paste0("data/OriginDataInput_xlsx/", site[i], stations$Name[i], ".xlsx")
  fwrite(df, fname, quote = F, sep = ",")
  result[[i]] <- df
}

save(stationInfo, file = "data/met839_stationInfo.rda")
info.list <- lapply(1:4, function(i) lapply(stationInfo, `[[`, i)) %>% set_names(c("origin", "naApprox", "linear", "hisavg"))
info <- lapply(info.list, function(x) do.call(cbind, x)[, -seq(9, 8*length(x), 8)])
openxlsx::write.xlsx(info, file = "met839_stationInfo.xlsx")
