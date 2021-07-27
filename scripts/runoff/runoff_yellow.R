source("scripts/main_pkgs.R")

files = dir("O:/水文数据/daily data in Yellow River/files", full.names = TRUE)
st = read_xlsx("O:/水文数据/daily data in Yellow River/yr.stationinfo.xlsx")

sites = basename(files) %>% gsub(".csv", "", .)
info = match2(sites, st$name)
