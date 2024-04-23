test_that("fecundity with intercept", {
  b0 <- 0.4
  intercept <- c(NA, logit(b0))

  x <- bbs_fecundity(intercept = intercept)
  expect_snapshot({
    print(x)
  })
})

test_that("fecundity with trend", {
  b0 <- 0.4
  intercept <- c(NA, logit(b0))
  trend <- c(NA, 0.2)
  
  x <- bbs_fecundity(intercept = intercept,
                     trend = trend)
  
  expect_snapshot({
    print(x)
  })
  
  expect_true(all(x$eFecundity[,1] == 0))
  # only increasing
  expect_true(all(x$eFecundity[,2] >= b0))
  # first year is intercept
  expect_equal(x$eFecundity[1,2], b0)
})

test_that("fecundity with annual random", {
  withr::with_seed(10, {
    b0 <- 0.4
    intercept <- c(logit(b0), NA)
    annual_sd <- c(0.2, NA)
    
    x <- bbs_fecundity(intercept = intercept,
                       annual_sd = annual_sd)

    expect_snapshot({
      print(
        x
      )
    })
  })
  
  # all years different
  expect_equal(length(unique((x$eFecundity[,1]))), 10L)
})

test_that("fecundity caribou", {
  calves_per_adult_female <- 0.4
  x <- bbs_fecundity_caribou(calves_per_adult_female = calves_per_adult_female)
  expect_snapshot({
    print(x)
  })
  
})

test_that("fecundity caribou with trend", {
  calves_per_adult_female <- 0.4
  trend <- 0.1
  
  x <- bbs_fecundity_caribou(calves_per_adult_female = calves_per_adult_female, 
                             trend = trend)
  expect_snapshot({
    print(x)
  })
  
  # first year intercept
  expect_true(all(x$eFecundity[1,3] == calves_per_adult_female))
  # all increasing
  expect_true(all(x$eFecundity[1,3] >= calves_per_adult_female))
})

test_that("fecundity caribou with trend", {
  calves_per_adult_female <- 0.4
  trend <- -0.2
  annual_sd <- 0.3
  
  withr::with_seed(10, {
    x <- bbs_fecundity_caribou(calves_per_adult_female = calves_per_adult_female, 
                               trend = trend,
                               annual_sd = annual_sd)
    expect_snapshot({
      print(x)
    })
  })
  
  # all different
  expect_equal(length(unique(x$eFecundity[,3])), 10L)
  # last smaller than first due to underlying trend
  expect_true(x$eFecundity[1,3] > x$eFecundity[10,3])
})

