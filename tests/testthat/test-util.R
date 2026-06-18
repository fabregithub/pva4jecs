test_that("row_sum preserves dimensions and normalizes rows", {
  x <- matrix(
    c(1, 3, 2,
      2, 2, 6,
      5, 5, 10),
    nrow = 3,
    byrow = TRUE
  )

  out <- row_sum(x)

  expect_equal(dim(out), dim(x))
  expect_equal(rowSums(out), rep(1, nrow(x)))
})

test_that("evlt preserves dimensions and creates unit-length rows", {
  x <- matrix(
    c(1, 20, 9,
      2, 10, 11,
      3, 25, 5,
      4, 15, 7),
    nrow = 4,
    byrow = TRUE
  )

  out <- evlt(x)

  expect_true(is.matrix(out))
  expect_equal(dim(out), dim(x))
  expect_equal(as.vector(rowSums(out^2)), rep(1, nrow(x)), tolerance = 1e-8)
})

test_that("estimate_X returns finite values with the same dimensions as input", {
  data("dataX", package = "pva4jecs")
  x <- as.matrix(dataX)

  estimated <- estimate_X(x, k = 3)

  expect_equal(dim(estimated), dim(x))
  expect_true(all(is.finite(estimated)))
})

test_that("PVA returns named A0 and F0 matrices", {
  data("dataX", package = "pva4jecs")
  x <- as.matrix(dataX)

  fit <- PVA(x, k = 3, N = 1)

  expect_named(fit, c("A0", "F0"))
  expect_equal(dim(fit$A0), c(nrow(x), 3))
  expect_equal(dim(fit$F0), c(3, ncol(x)))
  expect_true(all(is.finite(fit$A0)))
  expect_true(all(is.finite(fit$F0)))
})

test_that("plot helpers return ggplot objects", {
  data("dataX", package = "pva4jecs")
  x <- as.matrix(dataX)

  expect_s3_class(CV_plot(x), "ggplot")
  expect_s3_class(CD_plot(x, k = 3), "ggplot")
})
