source('test/main_pkgs.R')
library(ncdf4)

file <- "E:/GitHub/surface_met2474_daily.nc"

# Define an integer dimension 
t <- ncdim_def("time", "days since 1970-01-01", as.numeric(date_daily))
std <- ncdim_def("std", "std", 1:nstd)

# Define Variables
attrs <- lapply(info$var, function(x) x[c('name', 'units', 'longname')])
vars <- list()
for (i in seq_along(attrs)){
    if (i <= nvars - 3){
        vars[[i]] <- ncvar_def(attrs[[i]]$name, attrs[[i]]$units, list(t, std), missval = -999, attrs[[i]]$longname)
    }else{
        vars[[i]] <- ncvar_def(attrs[[i]]$name, attrs[[i]]$units, std, longname = attrs[[i]]$longname)
    }
}
# vars[[i + 1]] <- ncvar_def("time", "day", std, longname = "days since 1970-01-01")
# names(vars) <- c(names(info$var), "time")

# Create a netCDF file with this variable
outfile <- "data/kong1.nc"
fid <- nc_create(outfile, vars, force_v4 = TRUE)

# put global attributes
ncatt_put(fid, 0, "period", "19510101 - 20141231")
ncatt_put(fid, 0, "source", "China surface meteorological observatins at 2474 stations")
ncatt_put(fid, 0, "create_date", as.character(Sys.time()))
ncatt_put(fid, 0, "author", "Dongdong KONG, kongdd@mail2.sysu.edu.cn, Sun Yat-sen University")
# nc_close(fid)

for (i in seq_along(varsname)){
  fprintf("[%d] %s\n", i, varsname[i])
  ncvar_put(fid, vars[[i]], x_new[[i]])
}
info_kong <- nc_open(outfile)
info_kong
nc_close(fid)