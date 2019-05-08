test_that("$coefficients works", {
    set.seed(129)
    n <- 7; p <- 2
    X <- matrix(rnorm(n * p), n, p) # no intercept!
    y <- rnorm(n)

    obj1 <- .lm.fit (x = cbind(1, X), y = y)
    obj2 <- lm(y~X)

    s1 <- summary_lm(obj1)
    s2 <- summary(obj2)

    expect_equal(as.numeric(s2$coefficients), as.numeric(s1))
})
