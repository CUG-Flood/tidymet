test_that("interp_main and dtime2mat works", {
    data("prcp")
    data("st840")

    r <- interp_main(prcp, st840, smax = 200, verbose = FALSE)

    df2 <- dtime2mat(r$x)
    expect_equal(ncol(df2), ncol(prcp))
    expect_false(all.equal(df2, prcp) == TRUE)
})
