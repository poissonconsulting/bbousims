test_that("age process matrix works", {
  age <- c(2, 3, 3)
  x <- bbs_matrix_age(age)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(0, 100, 200))
})