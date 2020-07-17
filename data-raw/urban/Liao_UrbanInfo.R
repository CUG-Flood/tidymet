library(latticeGrob)
library(R.matlab)
library(missInfo)

file <- "data-raw/urban/urban_fraction.mat"
l  <- readMat(file)
st <- l$urban.fraction %>%
    set_colnames(c("lat", "lon", 1980,1990,1995,2000,2005,2008,2010,2013)) %>% data.table()
st[, `:=`(lat = dec2deg(lat), lon = dec2deg(lon))]


urbanInfo_liao = st_met2474[, .(site, lon, lat, alt)] %>% merge(st) %>% reorder_name(c("I", "site"))
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
use_data(urbanInfo_liao, overwrite = TRUE)
