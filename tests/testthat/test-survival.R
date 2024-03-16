test_that("survival process matrix", {
  rates <- c(0.84, 0.85)
  x <- matrix_survival(rates)
  expect_identical(dim(x), c(2L,2L))
  expect_identical(as.vector(x), c(0.84, 0, 0, 0.85))
})

test_that("survival process matrices", {
  # 4 seasons, 2 years, 2 stages
  rates <- array(c(0.85, 0.84, 0.86, 0.87, 0.89, 0.83, 0.81, 0.85,
                   0.85, 0.84, 0.86, 0.87, 0.89, 0.83, 0.81, 0.85), dim = c(4, 2, 2))
  x <- matrix_survival_period(rates)
  expect_identical(dim(x), c(2L, 2L, 2L, 4L))
  expect_identical(dim(x[,,1,1]), c(2L, 2L))
})

test_that("survival stochastic", {
  survival_adult_female <- 0.84
  survival_calf <- 0.5
  annual_sd <- 0.05
  period_sd <- 0.05
  annual_period_sd <- 0.05
  yearling_effect <- 0.1
  trend <- 0.2
  nyear <- 5
  nperiod_within_year <- 12
  
  x <- survival_period(survival_adult_female = survival_adult_female,
                       survival_calf = survival_calf,
                       yearling_effect = yearling_effect,
                       trend = trend,
                       annual_sd = annual_sd, period_sd = period_sd,
                       annual_period_sd = annual_period_sd, nyear = nyear, 
                       nperiod_within_year = nperiod_within_year)
  
  expect_type(x, "double")
  expect_identical(dim(x), as.integer(c(nperiod_within_year, nyear, length(stage))))
  expect_snapshot(x)
})