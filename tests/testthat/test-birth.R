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
  x <- bbs_matrix_birth(rates, female_proportion = 0.75)
  expect_identical(dim(x), c(3L,3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[,1], c(130, 100, 100))
})

test_that("can change calf stage indices", {
  rates <- c(0.4, 0, 0)
  x <- bbs_matrix_birth(rates, male_recruit_stage = 3, female_recruit_stage = 2, female_proportion = 0.75)
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

test_that("fecundity with trend", {
  calves_per_adult_female <- 0.4
  trend <- -0.05

  x <- bbs_fecundity(calves_per_adult_female = calves_per_adult_female,
                     trend = trend)
  
  expect_identical(dim(x), c(10L, 3L))
  expect_true(all(x[,c(1:2)] == 0))
  expect_equal(x[1,3], calves_per_adult_female)
  expect_true(all(x[,3] <= calves_per_adult_female))
})

test_that("fecundity with annual", {
  calves_per_adult_female <- 0.4
  annual_sd <- 0.1
  
  x <- bbs_fecundity(calves_per_adult_female = calves_per_adult_female,
                     annual_sd = annual_sd)
  
  expect_false(all(x[,3] <= calves_per_adult_female))
  expect_true(all(x[,c(1:2)] == 0))
})