#' @name MissInfo-package
#' @title MissInfo
#' @docType package
#' @keywords missing information, interpolate
#' @description detect missing information of meteorological data and interpolate missing values using common interpolation methods
#' @import magrittr foreach plyr
#' @importFrom stats .lm.fit aggregate df dist median pt
#' @keywords internal
"_PACKAGE"

# .onUnload <- function (libpath) {
#   library.dynam.unload("MissInfo", libpath)
# }
# exportPattern("MissInfo")
