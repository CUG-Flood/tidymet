rm(list = ls())
source('R/中国气象数据整理main.R', encoding = 'UTF-8')
finfo <- "说明文档/中国气象台站变迁记录.xlsx"
cl <- Parallel_init()
## -----------------global functions-------------------
# 对于发生迁移的气象站点应该如何处理
#'param i: 1, 2, 3分别代表首、中、尾
getSingleInfo <- function(x, i = 3){
  n <- nrow(x)
  pos <- c(1, ceiling(n/2), n)
  x[pos[i], ]
}

## ---------------global functions end ----------------

## 获取所有站点信息
if (file.exists(finfo)){
  info <- read.xlsx(finfo)
}else{
  path <- "../data/中国气象数据共享中心/PRE/"
  fpaths <- dir(path, full.names = T)
  
  system.time(result <- read_table_parallel(fpaths, info = T))
  tmp <- do.call(rbind, result) %>% set_rownames(1:nrow(.))
  info <- unique(tmp[, 1:4])
  info$time <- tmp[as.numeric(rownames(info)), "time"]
  # info <- convertPostionInfo(info)
  Id <- order(info$StationNo)##根据站点编号进行排序
  info <- info[Id, ]; rownames(info) <- 1:nrow(info)
  
  stationNo <- unique(info[, 1])
  N <- length(stationNo)
  
  changeTimes <- sapply(stationNo, function(x) length(which(info[, 1] == x)))## times of change position
  num_nochange <- length(which(changeTimes == 1))
  
  write.xlsx(info, file = finfo)
  Changepert <- (N-num_nochange)/N*100#存在一个特别严重的问题，96.7%的站点都经历过位置变迁
}

stationNo <- unique(info[, 1])
x <- split(info, info$StationNo) %>% lapply(., getSingleInfo) %>% do.call(rbind, .)
stationV3 <- convertPostionInfo(x)

stations <- read.xlsx("说明文档/台站信息/国家级地面观测站2170station.xlsx")
fnames <- dir("说明文档/台站信息/", full.names = T)
stations <- lapply(as.list(fnames[1:3]), function(x) read.xlsx(x)[, c("区站号", "站名")]) %>% 
  do.call(rbind, .) %>% unique
# length(which(!is.na(match(stationV3$StationNo, stations[[1]]$区站号))));756站点，662在这个数据集中

stations2 <- lapply(1:3, function(i) read.xlsx(fnames[4], i)[, c("区站号", "站名")]) %>% 
  do.call(rbind, .) %>% unique
stations <- unique(rbind(stations, stations2))

Id_match <- match(stationV3$StationNo, stations$区站号)
Id <- which(is.na(Id_match))
# 说明：57049台站号名称更换
# 陕西	二级站	57045	渭南	单轨自动	原一般站，现二级站，原57049华县
## 第一步给站点标上名称
stationV3$Name <- stations$站名[Id_match]

write.xlsx(stationV3, file = "说明文档/中国气候日志数据V3station839.xlsx")
## 第二步核对经纬度信息, 跳过