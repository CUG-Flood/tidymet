## Writed By Dongdong Kong, 2016/03/28
#  读取中国气象数据共享中心提供的原始文件
library(zoo)
library(magrittr)
library(openxlsx)
library(readr)
library(snow)
library(fields)
library(data.table)
##====================== global functions================================
## 首先查看具有多少个站点，生成站点信息文件，采取并行计算的读取方法，加快速度
#  获取站点的经度维度高程，迁移时间等信息
read_table_info <- function(fpath){
  x <- read.table(fpath)[, 1:7]
  df <- unique(x[, 1:4])
  Id <- rownames(df) %>% as.numeric
  x <- x[Id, ]#for retrive timestring
  time <- sprintf("%d-%02d-%02d", x[, 5], x[, 6], x[, 7])
  cbind(df, time) %>% set_colnames(c("StationNo", "lat", "lon", "alt", "time"))
}
read_table <- function(fpath, zcol, zcolname){
  x <- read.table(fpath)[, c(1, zcol)]
  colnames(x) <- c("StationNo", zcolname)#是否有必要加入时间要素待定
  x#quickly return
}
# fnameToDate函数已经修正，内存占用已解决
read_elementFile <- function(fname, zcol, zcolname, site){
  N <- length(site)#站点个数
  
  # Id_time <- fnameToTime(fname, returnId = T); days <- length(Id_time)
  timestr <- fnameToTime(fname, DailyDate = T); days <- length(timestr)
  df <- read_table(fname, zcol, zcolname); zcol = 1:length(zcol) + 1#减少操作的数据量
  StationNoi <- df[seq(1, nrow(df), days), 1]#获取具有观测值的台站号
  id <- match(StationNoi, site)
  
  for (i in seq_along(zcol)){
    eval(parse(text = sprintf("%s <- matrix(NA, nrow = days, ncol = N, dimnames = list(timestr, site))", zcolname[i])))
    eval(parse(text = sprintf("%s[, id] <- matrix(df[, %d], nrow = days)", zcolname[i], zcol[i])))
  }
  #gc()
  ## 这条命令为什么没有执行？
  eval(parse(text = paste0("df_list <- list(", paste(zcolname, zcolname, collapse = ", ", sep = " = "), ")")))
  df_list#quickly return
}

## snow parallel initial
Parallel_init <- function(){
  cl <- makeCluster(ncluster, type = "SOCK", outfile = "log.txt")
  clusterEvalQ(cl, library(magrittr))
  clusterExport(cl, c("read_table_info", "fnameToTime", "read_table", 
                      "read_elementFile", "monthdays"), envir = globalenv());cl#quickly return
  #assign('cl', cl, pos=globalenv())#quickly return, set cl to global envir
}
# fun <- get("read_table", mode = "function", envir = globalenv())#get function
# 并行work的开启比较浪费时间，因此把Parallel_init和stopcluster放在外面
read_table_parallel <- function(fpaths, info = FALSE, ncluster = 8, zcol = NULL, zcolname) {
  # cl <- Parallel_init()
  ## 添加数据读取的部分
  if (is.null(zcol)){
    ifelse(info, df <- parLapply(cl, fpaths, read_table_info), df <- parLapply(cl, fpaths, read_table))
  }else{
    df <- parLapply(cl, fpaths, read_table, zcol = zcol, zcolname)
  }
  df#quickly return
  # stopCluster(cl);df
}
# 输入文件名返回时间信息, RETURN DATE STRING

#' @title fnameToTime
#' @description return the last day's date of corresponding cma data file
#' @param file cma meteorology data file name 
#' @DailyId the corresponding daily Id of file's month. Id origin is "1951-01-01"
#' @DailyDate the corresponding daily date of file's month.
fnameToTime <- function(file, DailyId = FALSE, DailyDate = FALSE){
  time <- substr(file, nchar(file)-9, nchar(file)-4)
  year <- as.integer(substr(time, 1, 4))
  month <- as.integer(substr(time, 5, 6))
  days <- monthdays(year, month)
  if (DailyId){
    beginId <- as.numeric(as.Date(sprintf("%d-%02d-01", year, month)) - as.Date("1951-01-01") + 1)
    endId <- beginId + days -1
    beginId:endId#quickly return
  }else{
    if (DailyDate){
      format(seq(as.Date(sprintf("%d-%02d-01", year, month)), 
                 as.Date(sprintf("%d-%02d-%02d", year, month, days)), by = "day"), "%Y-%m-%d")#return
    }else{
      as.Date(sprintf("%d-%02d-%02d", year, month, days))#return last day date
    }
  }
}

ChangeFormat_element <- function(fnames, zcol, zcolname, site, dir_output){
  n <- length(fnames)
  fname_last <- fnames[n]
  time_day <- seq(as.Date("1951-01-01"), fnameToTime(fname_last), by = "day")
  timestr <- format(time_day, "%Y-%m-%d")
  fnameSuffix <- paste0(format(time_day[1], "%Y%m"), "-", format(time_day[length(time_day)], "%Y%m"), ".rda")# 保存文件后缀名
  
  cl <- Parallel_init()
  result <- parLapply(cl, fnames, read_elementFile, zcol = zcol, zcolname, site = site)
  ## 有必要单独进行处理
  getResult <- function(i) lapply(result, function(x) x[[i]]) %>% do.call(rbind, .)
  for (i in seq_along(zcol))
    eval(parse(text = sprintf("%s <- getResult(%d)", zcolname[i], i)))
  
  stopCluster(cl);gc()
  
  fname <- paste0(dir_output, toupper(substr(zcolname[1], 1, 3)), fnameSuffix)
  print("运行已结束，正在保存文件...")
  save(list = zcolname, file = fname)
  rm(list = ls()); tmp <- gc()#关闭并行计算入口，清理内存
}

##====================== global variables ================================
ncluster = 8
