test_that("tidy_meteV3 works", {
    data("mete840")
    df1 <- tidy_meteV3(mete840)
    expect_equal(colnames(df1), colnames(mete840))
    expect_true(max(df1$prcp20_20, na.rm = TRUE) < 2500)
    expect_true(all(df1$alt < 1e5))
})
