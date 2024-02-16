test_that("birth process matrix works", {
  rates <- c(0, 0, 0.2)
  x <- matrix_birth(rates)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(110, 110, 100))
})

test_that("can change female proportion", {
  rates <- c(0, 0, 0.4)
  x <- matrix_birth(rates, female_proportion = 0.75)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(130, 110, 100))
})

test_that("can change calf stage indices", {
  rates <- c(0.4, 0, 0)
  x <- matrix_birth(rates, male_recruit_stage = 3, female_recruit_stage = 2, female_proportion = 0.75)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(100, 130, 110))
})

test_that("birth process matrices work", {
  # 4 seasons, 2 years, 2 stages
  rates <- matrix(c(0, 0, 0.2,
                   0, 0, 0.3), nrow = 2, byrow = TRUE)
  x <- matrix_birth_year(rates)
  expect_identical(dim(x), c(3L, 3L, 2L))
  y1 <- x[,,1] %*% c(100, 100, 100)
  y2 <- x[,,2] %*% c(100, 100, 100)
  expect_identical(y1[,1], c(110, 110, 100))
  expect_identical(y2[,1], c(115, 115, 100))
})
