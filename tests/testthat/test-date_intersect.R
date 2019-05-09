context("test-date_intersect")

test_that("date_intersect works", {
    int1 <- interval("2010-01-01", "2012-12-31")
    int2 <- interval("2011-01-01", "2012-12-31")
    int3 <- interval("2013-01-01", "2014-12-31")

    r1 <- int_intersect(int1, int2)
    r2 <- int_intersect(int2, int1)

    r3 <- int_intersect(int1, int3)
    r4 <- int_intersect(int3, int1)

    expect_equal(r1, r2)
    expect_equal(r3, r4)
    expect_equal(length(r1), 731)
    expect_null(r3)
})
