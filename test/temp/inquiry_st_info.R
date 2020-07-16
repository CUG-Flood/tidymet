## code to prepare `DATASET` dataset goes here

# usethis::use_data(DATASET, overwrite = TRUE)
library(Ipaper)
library(baidumap)

names <- c("prov", "type", "site", "name", "siteProp", "info")
st = read_xlsx2list("data-raw/station/全国气象站号2433station.xlsx") %>%
    map(data.table) %>%
    do.call(rbind, .) %>% set_names(names)
    # melt_list("type")

st_miss  <- fread("data-raw/st_miss.csv")
st_patch <- st_miss[, .(site, lon.x, lat.x, alt.x)] %>%
    merge(st[, .(site, name, prov)], ., all.y = TRUE)

# fwrite(st_patch, file = "data-raw/st_patches.csv")
d_patch <- fread("data-raw/st_patches.csv")
r = get_location(d_patch[, .(lon.x, lat.x)])
info = data.table(I = 1:nrow(r), y = r$province %>% substr(1, 2), x = d_patch$prov)
I_bad = info[x != y, ]$I
