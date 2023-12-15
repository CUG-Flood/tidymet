#' qc_mask
#'
#' @param d data.table
#' @param QCmin values of QC greater than QCmin will be set as NA.
#'
#' @export
qc_mask <- function(d, QCmin) {
  names <- colnames(d)
  I_varqc <- grep("QC_", names)
  varqc <- names[I_varqc]
  var <- varqc %>%
    gsub("QC_", "", .)

  mat_var <- d[, ..var]
  mat_qc <- d[, ..varqc]

  names_head <- setdiff(names, c(var, varqc))

  mat_var[mat_qc > QCmin] <- NA_integer_
  data.table(df[, ..names_head], mat_var)
}
