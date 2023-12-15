library(missInfo)


st_met2481$name <- ""
st_met2481$prov <- ""
st_met2481$type <- ""

##
st <- readxl::read_excel("data-raw/station/SURF_CLI_CHN_MUL_DAY_STATION.xls")
info <- match2(st$site, st_met2481$site)
st_met2481[info$I_y, ]$name <- st$name[info$I_x]
st_met2481[info$I_y, ]$prov <- st$prov[info$I_x]

st <- readxl::read_excel("data-raw/station/附件2.国家级地面气象观测站站点基本信息全表（2016）.xls", skip = 2)
info <- match2(st$site, st_met2481$site)
st_met2481[info$I_y, ]$name <- st$name[info$I_x]
st_met2481[info$I_y, ]$type <- st$type[info$I_x]
st_met2481[info$I_y, ]$prov <- st$prov[info$I_x]

st_met2481 %<>% reorder_name(c("site", "name", "prov", "type", "lon", "lat", "alt"))
usethis::use_data(st_met2481, overwrite = TRUE)

# st = st_met2176_hourly
# info = match2(st$site, st_met2481$site)
# st_met2481[info$I_y, ]$name = st$name[info$I_x]

## 余下64个站点信息不全
# d = st_met2481[name == "" | type == "", .(site, name, type)]
# # fwrite(d, "a.csv")
# write_list2xlsx(d, "a.xlsx")
