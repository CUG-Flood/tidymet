context("test-tidy_location")

test_that("tidy_location works", {
    data("station_HisPos")
    d_1 <- station_HisPos[, .(site, lat, long, alt)]
    d_2 <- station_HisPos[, .(site, lat, lon = long, alt)]

    d_loc1 <- tidy_location(d_1)
    d_loc2 <- tidy_location(d_2)

    expect_equal(nrow(d_loc1), nrow(d_loc2))
    expect_true(all(d_loc1$lat <= 180))
    expect_true(all(d_loc1$alt < 6000))
})
