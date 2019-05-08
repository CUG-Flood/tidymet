test_that("zip_dates for multiple sites works", {
    data("station_HisPos")
    df2  <- station_HisPos[, .(site, date = make_date(year, month, day = 1L))] %>% unique()
    info <- df2[, zip_dates(date), .(site)]

    d <- info[site == "57516", ]
    expect_equal(d[, str_miss], "201704")
    expect_equal(d[, str_left], "195101-201703, 201705-201903")
    expect_equal(nrow(info), 840)
})
