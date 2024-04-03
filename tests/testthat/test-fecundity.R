test_that("fecundity process matrix", {
  rates <- c(0, 0, 0.9)
  x <- bbs_matrix_birth(rates, female_recruit_stage = 1, male_recruit_stage = NULL)
  expect_identical(dim(x), c(3L,3L))
  expect_identical(as.vector(x), c(1, 0, 0, 0, 1, 0, 0.45, 0, 1))
})

test_that("fecundity process matrices", {
  # 2 years, 2 stages
  rates <- array(c(0, 0, 0, 0, 0.9, 0.8), dim = c(2, 3))
  x <- bbs_matrix_birth_year(rates, male_recruit_stage = NULL)
  expect_identical(dim(x), c(3L, 3L, 2L))
  expect_identical(dim(x[,,1]), c(3L, 3L))
})

test_that("fecundity stochastic", {
  calves_per_adult_female <- 0.9
  annual_sd <- 0
  trend <- 0.2
  nyear <- 5

  x <- bbs_fecundity_caribou(calves_per_adult_female = calves_per_adult_female,
                      trend = trend, annual_sd = annual_sd, nyear = nyear)
  
  expect_type(x, "double")
  expect_identical(dim(x), as.integer(c(nyear, 3L)))
  expect_snapshot(x)
})
