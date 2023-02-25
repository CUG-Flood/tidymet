source("./scripts/mete2481/main_met2481.R")

# read_mete(files[2], nrow = 1e5)
lst = llply(files, read_mete, lst_varnames, .progress = "text")
df = reduce(lst, merge, by = c("site", "date"), all.x = TRUE)
# 54016857
tailvars = df %>% dplyr::select(starts_with("QC.")) %>% colnames()
df %<>% reorder_name(tailvars = tailvars)

# df %>% select(!starts_with("QC."))

library(Ipaper)
outdir = path.mnt("/mnt/h/China_Latest_Meteorological_Data/2400climate\ data")
outfile = glue("{outdir}/ChinaMeteDaily_SURF_CLI_CHN_MUL_DAY-[195101,202003]_rawfile.csv")

fwrite(df, outfile)

## 1. 代表性站点 ---------------------------------------------------------------
outfile = "Z:/DATA/China/2400climate data/ChinaMeteDaily_SURF_CLI_CHN_MUL_DAY_[195101,202003]_rawfile.csv"
df = fread(outfile)

# df = fread(files[1], nrows = 1e5)
# 北京、武汉、广州
d = df[site %in% c(54511, 57494, 59287, 58449)] %>% 
    .[year(date) <= 2019]
# write_mete(d, "rawfile_")
tidy_mete2000(d)
write_mete(d %>% not_select_QC(), "processed_", date_end = "2019-12-31")

fwrite(df, glue("{outdir}/ChinaMeteDaily_SURF_CLI_CHN_MUL_DAY_[195101,202003]_processed.csv"))

## 2. 代表性站点 ---------------------------------------------------------------
tidy_mete2000(df)
# %>% not_select_QC()
write_mete(df, "OUTPUT/ChinaMetDaily_st2481_[195101,202003]/", date_end = NULL, overwrite = TRUE)
