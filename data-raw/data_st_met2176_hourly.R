library(plyr)
library(httr)
library(xml2)
library(data.table)

#' get_mete2000_stations
#' It has been saved in \code{data('st_hourly')}.
#' @export
get_mete2000_stations <- function() {
  p <- GET("http://data.cma.cn/Market/Detail/code/A.0012.0001/type/0.html") %>%
    content(encoding = "utf-8")

  prov <- xml_find_all(p, "//div[@class='jmarkImCl1AtItTe']") %>%
    {
      id <- xml_attr(., "id") %>% as.numeric()
      name <- xml_text(.)
      data.table(id, name)[id > 10, ]
    }

  get_station <- function(url) {
    l <- GET(url) %>%
      content(encoding = "utf-8") %>%
      xml_text() %>%
      fromJSON()
    cbind(prov = l$cityName, l$stations)
  }

  urls <- sprintf(
    "http://data.cma.cn/dataService/ajax/act/getStationsByProvinceID/dataCode/A.0012.0001.html?provinceID=%s",
    prov$id
  )
  res <- llply(urls, get_station, .progress = "text")
  # if no error return `df`, else return `list`
  tryCatch(
    {
      do.call(rbind, res) %>% data.table()
    },
    error = function(e) {
      message(sprintf("%s", e$message))
      res
    }
  )
}

st_hourly <- get_mete2000_stations()
use_data(st_hourly)

## 去除全部为NA的列
library(purrr)
library(dplyr)
library(Ipaper)

ind_good <- st_hourly %>%
  map_lgl(~ !all(is.na(.))) %>%
  which()
# alt2: 传感器高度
st_met2176_hourly <- st_hourly[, ..ind_good] %>%
  rename(site = StationID, lat = V05001, lon = V06001, alt = V07001, alt_obs = V07001_2) %>%
  mutate(provcode = V_ACODE) %>%
  select(-StationClass, -NETCODE, -V01301, -V_ACODE, -ProvinceCode, -IsOpenApi) %>%
  reorder_name(tail = c("provcode", "prov"))

# unknown variable
table(st_met2176_hourly$V02301) # 11, 12, 13
usethis::use_data(st_met2176_hourly, overwrite = TRUE)


# fwrite(meteItems_houly, "data-raw/met2176_hourly_variables.csv")
met2176_hourly_variables <- fread("data-raw/met2176_hourly_variables.csv")
usethis::use_data(met2176_hourly_variables, overwrite = TRUE)

LON <- st_met2176_hourly$lat %>% as.numeric()
LAT <- st_met2176_hourly$lon %>% as.numeric()
st_met2176_hourly$site %<>% as.integer()
st_met2176_hourly$lon <- LON
st_met2176_hourly$lat <- LAT
st_met2176_hourly$alt %<>% as.numeric()
st_met2176_hourly$alt_obs %<>% as.numeric()

library(dplyr)
st_met2176_hourly %<>%
  dplyr::rename(name = CNAME) %>%
  reorder_name(c("site", "name", "prov"))
