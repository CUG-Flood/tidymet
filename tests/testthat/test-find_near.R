test_that("find_near works", {
  st <- st_met2176_hourly
  p <- c(116.4, 39.9)
  s = find_near(p, st)
  expect_equal(nrow(s), 1)
})
