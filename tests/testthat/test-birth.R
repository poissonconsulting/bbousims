test_that("fecundity with intercept", {
  b0 <- 0.4
  intercept <- c(NA, logit(b0))

  x <- bbs_fecundity(intercept = intercept)
  
  expect_identical(dim(x), c(10L, 2L))
  expect_true(all(x[,1] == 0))
  expect_true(all(x[,2] == b0))
})

test_that("fecundity with trend", {
  b0 <- 0.4
  intercept <- c(NA, logit(b0))
  trend <- c(NA, 0.2)
  
  x <- bbs_fecundity(intercept = intercept,
                     trend = trend)
  
  expect_true(all(x[,1] == 0))
  # only increasing
  expect_true(all(x[,2] >= b0))
  # first year is intercept
  expect_equal(x[1,2], b0)
})

test_that("fecundity with annual random", {
  b0 <- 0.4
  intercept <- c(logit(b0), NA)
  annual_sd <- c(0.2, NA)
  
  x <- bbs_fecundity(intercept = intercept,
                     annual_sd = annual_sd)
  
  expect_true(all(x[,2] == 0))
  # all years different
  expect_equal(length(unique((x[,1]))), 10L)
})

test_that("fecundity caribou", {
  calves_per_adult_female <- 0.4
  # trend <- 0.1
  
  x <- bbs_fecundity_caribou(calves_per_adult_female = calves_per_adult_female)
  
  expect_true(all(x[,3] == calves_per_adult_female))
  expect_true(all(x[,c(1:2)] == 0))
})

test_that("fecundity caribou with trend", {
  calves_per_adult_female <- 0.4
  trend <- 0.1
  
  x <- bbs_fecundity_caribou(calves_per_adult_female = calves_per_adult_female, 
                             trend = trend)
  
  # first year intercept
  expect_true(all(x[1,3] == calves_per_adult_female))
  # all increasing
  expect_true(all(x[1,3] >= calves_per_adult_female))
  expect_true(all(x[,c(1:2)] == 0))
})

test_that("fecundity caribou with trend", {
  calves_per_adult_female <- 0.4
  trend <- -0.2
  annual_sd <- 0.3
  set.seed(1)
  
  x <- bbs_fecundity_caribou(calves_per_adult_female = calves_per_adult_female, 
                             trend = trend,
                             annual_sd = annual_sd)
  
  # all different
  expect_equal(length(unique(x[,3])), 10L)
  # last smaller than first due to underlying trend
  expect_true(x[1,3] > x[10,3])
})