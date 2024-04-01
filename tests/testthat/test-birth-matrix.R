test_that("birth process matrix works", {
  rates <- c(0, 0, 0.2)
  x <- bbs_matrix_birth(rates)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(110, 100, 100))
})

test_that("birth process matrix with male recruit works", {
  rates <- c(0, 0, 0.4)
  x <- bbs_matrix_birth(rates, male_recruit_stage = 2L)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(120, 120, 100))
})

test_that("can change female proportion", {
  rates <- c(0, 0, 0.4)
  x <- bbs_matrix_birth(rates, proportion_female = 0.75)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(130, 100, 100))
})

test_that("can change calf stage indices", {
  rates <- c(0.4, 0, 0)
  x <- bbs_matrix_birth(rates, male_recruit_stage = 3, female_recruit_stage = 2, proportion_female = 0.75)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(100, 130, 110))
})

test_that("birth process matrices work", {
  # 4 seasons, 2 years, 2 stages
  rates <- matrix(c(0, 0, 0.2,
                    0, 0, 0.3), nrow = 2, byrow = TRUE)
  x <- bbs_matrix_birth_year(rates)
  expect_identical(dim(x), c(3L, 3L, 2L))
  y1 <- x[,,1] %*% c(100, 100, 100)
  y2 <- x[,,2] %*% c(100, 100, 100)
  expect_identical(y1[,1], c(110, 100, 100))
  expect_identical(y2[,1], c(115, 100, 100))
})

test_that("works with bbs_fecundity", {
  b0 <- 0.4
  rates <- bbs_fecundity(intercept = c(NA, logit(b0)))
  x <- bbs_matrix_birth_year(rates)
  expect_identical(dim(x), c(2L, 2L, 10L))
  y <- x[,,1] %*% c(100, 100)
  expect_equal(y[,1], c(100*(1 + (b0/2)), 100))
})

test_that("works with bbs_fecundity_caribou", {
  b0 <- 0.8
  rates <- bbs_fecundity_caribou(calves_per_adult_female = b0, trend = 0.2, nyear = 2)
  x <- bbs_matrix_birth_year(rates)
  expect_identical(dim(x), c(3L, 3L, 2L))
  y <- x[,,1] %*% c(100, 100, 100)
  expect_equal(y[,1], c(100*(1 + (b0/2)), 100, 100))
})