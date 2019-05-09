#' interp_main
#'
#' Main funtion of interpolation, combined with na.approx, linear
#' and hisavg interpolation.
#'
#' @param df data.frame, with dimension of [n_date, 1+n_station], and the first
#' column is date.
#' @param st data.frame, with column at least of 'site', 'lat', 'lon' (degree).
#' @param maxgap maximum gap used in interp_approx interpolation
#' @inheritParams interp_linear
#'
#' @examples
#' \dontrun{
#' data("prcp")
#' data("st840")
#' r <- interp_main(prcp, st840, smax=200)
#' }
#' @importFrom purrr map
#' @export
interp_main <- function(df, st, smax = 100, nmax = 10, maxgap = 5,
    verbose = TRUE,
    sinkfile1 = "interp_lm1.log", sinkfile2 = "interp_lm2.log", ...)
{
    ## DIST
    sites <- st$site
    sites_sel <- colnames(df)[-1]
    I_sel <- match(sites_sel, sites)

    loc <- st[I_sel, .(lat, lon)] %>% as.matrix()
    dist <- rdist.earth(loc) %>% set_colnames(sites_sel)
    # filter searching distance

    ## MAIN SCIRPTS
    cat("[0    ] Detect missing information of original data ...\n")
  
    Info <- MissInfo.data.frame(df, clipdata = TRUE, time_day, site, verbose = verbose, ...)
    stationInfo <- Info$info
    xtrim <- Info$xtrim

    cat("[1.1  ] runing na.approx interpolate ...\n")
    ## 首先采用线性插值，对最长连续缺失< mapgap的站点采用邻近线性插值
    xx_approx <- interp_approx(xtrim, stationInfo, maxgap, verbose = verbose)

    cat("[1.2  ] Detect missing info after na.approx interpolate ...\n")
    stationInfo_approx <- MissInfo.list(xx_approx, verbose = verbose, ...)$info
    ## 邻近插值之后，采用相邻站点进行插值（线性拟合p<0.05通过显著性检验）

    cat("[2.1.1] runing linear lm interpolate ...\n")
    xx <- interp_linear(xx_approx, stationInfo_approx, dist = dist, smax, nmax, sinkfile = sinkfile1)

    xx_linear <- xx$xtrim
    cat("[2.1.2] Detect missing info after first lm interpolate ...\n")
    stationInfo_linear <- MissInfo.list(xx_linear, verbose = verbose, ...)$info

    # 进行二次插值，显示线性插值失败的站点
    # cat("Running : 正在进行相邻站点线性插值02\n")
    cat("[2.2.1] runing second linear lm interpolate ...\n")
    xx <- interp_linear(xx_linear, stationInfo_linear, dist, smax, nmax, sinkfile = sinkfile2)
    xx_linear <- xx$xtrim
    cat("[2.2.2] Detect missing info after second lm interpolate ...\n")
    stationInfo_linear2 <- MissInfo.list(xx_linear, verbose = verbose, ...)$info

    cat("[3.1  ] History average interpolate...\n")
    # save(xx_linear, stationInfo_linear2, file = "InterpBug.rda")#debug
    xx_hisavg <- interp_hisavg(xx_linear, stationInfo_linear2)
    cat("[3.2  ] Detect missing info after hisavg interpolate ...\n")
    stationInfo_hisavg <- MissInfo.list(xx_linear, verbose = verbose, ...)$info

    # return every step detected missing info
    stationInfo_list <- list(
        origin = stationInfo,
        approx = stationInfo_approx,
        linear = stationInfo_linear,
        hisavg = stationInfo_hisavg)
    list(stationInfo = stationInfo_list, x = xx_hisavg)
}
