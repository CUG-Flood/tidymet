library(plyr)
library(glue)
library(foreach)
library(iterators)
library(lubridate)

# dirs = list.dirs("N:/DATA/China/2400climate data")
dir_root = "N:/DATA/China/2400climate data"
varnames = c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")

# [826] 69473, 59034 2429 10839   1851 2019 10 235000002     64      5    106      5 9 9 9 9 9
#   win_avg: 5000002 -> 20
# [827] 17129: 53955 3531 11028   4616 2019 11 292500012     33      3     64      3 9 9 9 9 9
#   win_avg: 500012 -> 12
# [829]  3792: 51238 4454  8204   5322 2020  1  72500001     21     13     32     13 9 9 9 9 9
#   win_avg: 2500001 -> 10
foreach(varname = varnames[7:8], i = icount()) %do% {
    outfile = glue("{dir_root}/SURF_CLI_CHN_MUL_DAY_{varname} (195101-202003).csv")
    # if (file.exists(outfile)) return()

    indir = glue("{dir_root}/{varname}")
    files = dir(indir, "*.TXT", full.names = TRUE)

    lst <- foreach(file = files[1:length(files)], i = icount()) %do% {
        runningId(i, 10)
        tryCatch({
            fread(file)
        }, warning = function(e) {
            message(sprintf('[i] %s: %s', i, basename(file), e$message))
        })
    }
    # lst <- llply(files, fread, .progress = "text")
    df = do.call(rbind, lst)
    invisible()
    fwrite(df, outfile)
}

vars_common = c("site", "lat", "lon", "alt", "year", "month", "day")

library(purrr)
files = dir(dir_root, "*.csv", full.names = TRUE) %>% set_names(varnames)
I_sel = 1:7
lst <- map(files[7:8], ~fread(.x, select = I_sel) %>% set_colnames(vars_common[I_sel]))
l   <- map(lst, ~.x[year == 2019 & month == 12]) %>% map_int(nrow)

# 2019年数据异常
# ds$WIN[N < 30]
# site  N
# 1: 51468 11
# 2: 54287 20
# 3: 54646 20
# 4: 57713 19
# 5: 58358 20
# 6: 58726 22

    # 453
# d = fread(files[1])
## 获取台站变迁记录
get_history_location <- function() {
    st = df[, 1:7] %>% set_colnames(vars_common)
    st_moveInfo = dlply(st, .(site), function(d) {
        d <- data.table(d)
        d$tag = d[,1:4] %>% {!duplicated(.)} %>% cumsum()
        d$date = d[, make_date(year, month, day)]
        date_begin = min(d$date)
        date_end   = max(d$date)

        d[, .(period_date_begin = min(date), period_date_end = max(date),
            date_begin, date_end),
            .(site, tag, lon, lat, alt)]
    }, .progress = "text")

    st_moveInfo %<>% do.call(rbind, .)
    st_moveInfo[, moveTimes := max(tag), .(site)]
    st_moveInfo %<>% reorder_name(c("site", "moveTimes", "tag"))
    st_moveInfo[, 7:10] %<>% map(as.Date)
    st_moveInfo[, alt := get_alt(alt)]
    st_moveInfo[, `:=`(n_all = difftime(date_end, date_begin) %>% as.numeric(),
                       n_period = difftime(period_date_end, period_date_begin, units = "days") %>% as.numeric())]

    str_begin = df[1, sprintf("%d%02d", year, month)]
    str_end   = df[nrow(df), sprintf("%d%02d", year, month)]
    # library(glue)
    use_data(st_moveInfo, overwrite = TRUE)
    fwrite(st_moveInfo, glue::glue("data-raw/mete2481_站点变迁记录-({str_begin}-{str_end}).csv"))
}

