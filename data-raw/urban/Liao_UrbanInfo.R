library(latticeGrob)
library(R.matlab)
library(missInfo)
library(sp2)

file <- "data-raw/urban/urban_fraction.mat"
l  <- readMat(file)
df <- l$urban.fraction %>%
    set_colnames(c("lat", "lon", 1980,1990,1995,2000,2005,2008,2010,2013)) %>% data.table()
df[, `:=`(lat = dec2deg(lat), lon = dec2deg(lon))]

# rm sites short
st = merge(df[, .(lon, lat)], st_met2474[, .(site, lon, lat, alt, date_begin, date_end)])
st[, n := difftime(
    pmin(date_end, as.Date("1990-12-31")), 
    pmax(date_begin, as.Date("1961-01-01")), , units = "days") %>% as.numeric()]
sites_long = st[n >= 365*10, site]
# get urbanInfo data
urbanInfo_liao = st_met2474[, .(site, lon, lat, alt)] %>% 
    merge(df[st$site %in% sites_long]) %>% reorder_name(c("I", "site"))
urbanInfo_liao[, `:=`(lat = deg2dec(lat), lon = deg2dec(lon))]


## 5deg gridId
cellsize <- 5
# range_CH <- c(16, 55, 71.5, 140)
range_CH <- c(70, 140, 15, 55)
grid <- get_grid(range_CH, cellsize = 5)
grid@grid@cellcentre.offset
grid_poly <- as(grid, "SpatialPolygonsDataFrame")

urbanInfo_liao$gridId = raster::extract(raster::raster(grid), df2sp(urbanInfo_liao))

d <- melt(urbanInfo_liao, c("site", "lon", "lat", "alt", "gridId"),
          variable.name = "year", value.name = "perc_urban")
d$kind = "Rural"
d[perc_urban >= 33, kind := "Urban"]

urbanInfo_liao <- d
urbanInfo_liao$kind %<>% factor()
urbanInfo_liao$year %<>% as.character() %>% as.numeric()
use_data(urbanInfo_liao, overwrite = TRUE)

## 2. select sites and grids
info = urbanInfo_liao[, as.list(table(kind)), .(year, gridId)]
years = c(1980, 1990, 1995, 2000, 2005, 2010)
temp = info[Rural > 0 & Urban > 0 & year %in% years, .N,.(year, gridId)]
gridIds = temp[, .N, .(gridId)][N == length(years), gridId] %>% sort()
urbanInfo_liao2 = urbanInfo_liao[gridId %in% gridIds & year %in% years, ]
# select type unchanged sites
d <- urbanInfo_liao2[, as.list(table(kind)), .(site)]
sites_sel = d[Rural == 0 | Urban == 0, site]
# 332 Rural and 887 Urban
urbanInfo_sel = urbanInfo_liao2[site %in% sites_sel]
urbanInfo_sel[, .N, .(site, lon, lat, alt)]
# l_urban <- urbanInfo_liao2 %>% {split(., .$year)}
l_urban <- urbanInfo_sel %>% {split(., .$year)}

# st = st_met2474[site %in% sites_sel, .(site, lon, lat, alt, date_begin, date_end)] 
# st[, `:=`(lon = deg2dec(lon), lat = deg2dec(lat))]
# sp <- df2sp(sp)
