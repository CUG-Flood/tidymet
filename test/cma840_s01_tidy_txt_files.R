source('test/main_pkgs.R')
# Dongdong Kong
# Update 20190508
# ---------------
# This script is used to tidy China meteorological (V3, 840 stations) data
# (http://data.cma.cn/data/detail/dataCode/SURF_CLI_CHN_MUL_DAY_V3.0.html).
#
# Read data from txt files, and merge into a big csv file for every variable

dirs <- list.dirs(dir_root, full.names = TRUE)[-1]

varnames_pos <- c("site", "lat", "long", "alt", "year", "month", "day")

# MAIN scripts ------------------------------------------------------------

## 1. Get Station Position Information
s1_station_HisPos = FALSE
if (s1_station_HisPos) {
    files <- dir(dirs[1], '*.txt|TXT', full.names = T) # ET
    df <- ldply(files, function(file){
        # If station position has ever changed, more than one row will be returned.
        d <- fread(file, select = 1:7) %>% set_colnames(varnames_pos)
        rm_duplicate(d)
    }, .progress = "text", .id = NULL) %>%
        data.table() %>% .[order(site), ]

    # 1.1 Get historical meteorological station's position
    stationInfo_hisPos <- df %>% rm_duplicate() %>%
        plyr::mutate(date = ymd(sprintf("%d-%02d-%02d", year, month, day)),
                   year = NULL, month = NULL, day = NULL)
    fwrite(stationInfo_hisPos, "OUTPUT/mete840_stations_HistoryPosition.csv")

    # 1.2 Get roughly missing information
    station_HisPos <- df
    df2  <- station_HisPos[, .(site, date = make_date(year, month, day = 1L))] %>% unique()
    info <- df2[, zip_dates(date), .(site)]
    fwrite(info, "OUTPUT/mete840_stations_MissingInfo.csv")
}

## 2. tidy data ---------------------------------------------------------------
s2_tidy_meteV3 = TRUE
if (s2_tidy_meteV3) {
    vars_common <-  c("site", "date", "long", "lat", "alt")

    lst  <- mapply(read_var, dirs, varnames_all) # , limits = NULL
    lst2 <- mapply(function(x, varname) {
        x[, c(vars_common, varname, paste0('QC_', varname)), with = F]
    }, lst, varnames_sel)

    outfile_raw <- sprintf("%s/cma_mete840_allvars_raw (195101-201903).csv", dir_root)
    outfile <- sprintf("%s/cma_mete840_allvars (195101-201903).csv", dir_root)

    df_raw <- lst2 %>% {
        reduce(.[2:length(.)], merge, .init = .[[1]], by = vars_common, all = T)
    }
    df <- tidy_meteV3(df_raw)
    fwrite(df_raw, outfile_raw)
    fwrite(df, outfile)
    # df_raw <- fread(outfile_raw)
    # df <- tidy_meteV3(df_raw)
    # all.equal(df_raw, df)
}

# lst <- split(df, df$site)
# foreach()
# split into sites

# foreach(indir = dirs, i = icount()) %do% {
#     files <- dir(indir, "*.txt|*.TXT", full.names = TRUE)
#     date_begin <- first(files) %>% basename() %>% str_extract("\\d{6}")
#     date_end   <- last(files) %>% basename() %>% str_extract("\\d{6}")
#     outfile <- sprintf('%s/mete840_%s_(%s-%s).csv',
#                        dir_root, basename(indir), date_begin, date_end)
#     if (!file.exists(outfile)){
#         df <- llply(files[1:10], fread, header=FALSE, .progress = "text") %>%
#             do.call(rbind, .)
#         fwrite(df, outfile)
#     }
# }
