data('prcp')
time_day <- prcp$date
site <- colnames(prcp)[-1]
mat  <- as.matrix(prcp[, -1])
Info1 <- missInfo(mat, clipdata = TRUE, time_day, site, verbose = TRUE)
Info2 <- missInfo(prcp, clipdata = TRUE, time_day, site, verbose = TRUE)
all.equal(Info2, Info1)
