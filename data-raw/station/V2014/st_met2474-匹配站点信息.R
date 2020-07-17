## 匹配站点 
library(purrr)
library(data.table)

# st      = fread("data-raw/station/mete2481_站点变迁记录-(1951-2018).csv") %>% cbind(I = 1:nrow(.), .)
st      = fread("data-raw/mete2481_站点变迁记录-(195101-201812).csv") %>% cbind(I = 1:nrow(.), .)
st[, 8:11] %<>% map(as.Date)
# st[, n := difftime(period_date_end, period_date_begin, units = "day") %>% as.numeric()]

I_long = st[, I[which.max(n_period)], .(site)]$V1
st_met2481 = st[I_long, ]
st_met2481$lat %<>% deg2dec()
st_met2481$lon %<>% deg2dec()
use_data(st_met2481, overwrite = TRUE)
# st$date_begin %<>% openxlsx::convertToDate()
# st$date_end %<>% openxlsx::convertToDate()
# st[, `:=`(alt = get_alt(alt))]

st_2014 = fread("data-raw/station/V2014/met2474_stations.txt")
st_2014[, `:=`(lat = dec2deg(lat), lon = dec2deg(lon))]

# 728 站点，根据高程可以匹配上
str_id = function(...) do.call(paste, list(..., sep = "-"))
str_id3 = function(lon, lat, alt, digit = 1) paste(lon, lat, round(alt, digit), sep = "-")
st[, str_id := str_id(lon, lat)]
{
    info = match2(st_2014[, str_id3(lon, lat, alt)], st[, str_id3(lon, lat, alt)]) %>% 
        {cbind(st[.$I_y, .(site)], .)}
    sites_bad = info[duplicated(site), ]$site
    info = info[!(site %in% sites_bad), ]
    # info = rbind(info2[!(site %in% sites_bad)], info3[site %in% sites_bad])
}

## 根据站点起止时间匹配剩下的两个站点
st_left = st_2014[-info$I_x, ]
foreach(i = 1:nrow(st_left)) %do% {
    str_x <- st_left[, str_id3(lon, lat, alt)][i]
    st[str_id3(lon, lat, alt) == str_x]
}

info_final <- data.table(site = c(54517, 54527), I_x = c(805, 814), I_y = c(3467, 3508)) %>% 
    rbind(info[, .(site, I_x, I_y)], .) %>% .[order(I_x), ]

st_met2474 = st[info_final$I_y, 1:13] %>% cbind(ID = 1:nrow(.), .)
# cbind(st_met2474[, .(site, lon, lat, alt)], st_2014)[site %in% sites_bad]
# fwrite(st_met2474, "data-raw/st_met2474-(1951-2014).csv")
use_data(st_met2474, overwrite = TRUE)
