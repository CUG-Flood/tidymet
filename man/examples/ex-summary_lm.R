set.seed(129)
n <- 7; p <- 2
X <- matrix(rnorm(n * p), n, p) # no intercept!
y <- rnorm(n)

obj_1 <- .lm.fit (x = cbind(1, X), y = y)
obj_2 <- lm(y~X)
