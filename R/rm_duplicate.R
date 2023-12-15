#' @export
rm_duplicate <- function(d) {
  I <-
    {
      !duplicated(d[, 1:4])
    } %>%
    which()
  d_pos <- d[I, ] %>%
    fix_alt()
  d_pos[order(site), ]
}
