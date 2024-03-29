test_that("survival process matrix", {
  rates <- c(0.84, 0.85)
  x <- bbs_matrix_survival(rates)
  expect_identical(dim(x), c(2L, 2L))
  expect_identical(as.vector(x), c(0.84, 0, 0, 0.85))
})

test_that("survival process matrices", {
  # 4 seasons, 2 years, 2 stages
  rates <- array(
    c(
      0.85,
      0.84,
      0.86,
      0.87,
      0.89,
      0.83,
      0.81,
      0.85,
      0.85,
      0.84,
      0.86,
      0.87,
      0.89,
      0.83,
      0.81,
      0.85
    ),
    dim = c(4, 2, 2)
  )
  x <- bbs_matrix_survival_period(rates)
  expect_identical(dim(x), c(2L, 2L, 2L, 4L))
  expect_identical(dim(x[, , 1, 1]), c(2L, 2L))
})
