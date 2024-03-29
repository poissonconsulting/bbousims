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

test_that("survival with no random and no trend", {
  survival_adult_female <- 0.84
  survival_calf_female <- 0.5
  yearling_effect = 0.1
  nyear <- 5
  
  x <-
    bbs_survival_period(
      survival_adult_female = survival_adult_female,
      survival_calf_female = survival_calf_female,
      yearling_effect = yearling_effect,
      nyear = nyear
    )
  
  expect_true(all(unlist(x[, , 3]) == ilogit(logit(survival_adult_female^(1/12)))))
  expect_true(all(unlist(x[, , 1]) == ilogit(logit(survival_calf_female^(1/12)))))
  expect_true(all(unlist(x[, , 2]) == ilogit(logit(survival_adult_female^(1/12)) + yearling_effect)))
})

test_that("survival with random adult female, no trend, no yearling effect", {
  survival_adult_female <- 0.84
  survival_calf_female <- 0.5
  yearling_effect = 0
  annual_sd_adult_female <- 0.1
  month_sd_adult_female <- 0.2
  annual_month_sd_adult_female <- 0.05
  nyear <- 5
  
  x <-
    bbs_survival_period(
      survival_adult_female = survival_adult_female,
      survival_calf_female = survival_calf_female,
      yearling_effect = yearling_effect,
      annual_sd_adult_female = annual_sd_adult_female,
      month_sd_adult_female = month_sd_adult_female,
      annual_month_sd_adult_female = annual_month_sd_adult_female,
      nyear = nyear
    )
  
  expect_false(all(unlist(x[, , 3]) == ilogit(logit(survival_adult_female^(1/12)))))
  expect_true(all(unlist(x[, , 1]) == ilogit(logit(survival_calf_female^(1/12)))))
  expect_true(all(unlist(x[, , 2]) == unlist(x[, , 3])))
})

test_that("survival with random calf female, no trend, no yearling effect", {
  survival_adult_female <- 0.84
  survival_calf_female <- 0.5
  yearling_effect = 0
  annual_sd_calf_female <- 0.1
  month_sd_calf_female <- 0.2
  annual_month_sd_calf_female <- 0.05
  nyear <- 5
  
  x <-
    bbs_survival_period(
      survival_adult_female = survival_adult_female,
      survival_calf_female = survival_calf_female,
      yearling_effect = yearling_effect,
      annual_sd_calf_female = annual_sd_calf_female,
      month_sd_calf_female = month_sd_calf_female,
      annual_month_sd_calf_female = annual_month_sd_calf_female,
      nyear = nyear
    )
  
  expect_true(all(unlist(x[, , 3]) == ilogit(logit(survival_adult_female^(1/12)))))
  expect_false(all(unlist(x[, , 1]) == ilogit(logit(survival_calf_female^(1/12)))))
})

test_that("survival with calf female trend", {
  survival_adult_female <- 0.84
  survival_calf_female <- 0.5
  trend_calf_female = 0.2
  
  x <-
    bbs_survival_period(
      survival_adult_female = survival_adult_female,
      survival_calf_female = survival_calf_female,
      trend_calf_female = trend_calf_female
    )
  
  expect_true(all(unlist(x[1, , 1]) == unlist(x[2, , 1])))
  # year always increasing
  expect_true(all(x[1, , 1] >= x[1, 1, 1]))
  # first is equal to survival_adult_female
  expect_true(ilogit(logit(survival_calf_female^(1/12))) == x[1, 1, 1])
})

test_that("survival with calf female trend", {
  survival_adult_female <- 0.84
  survival_calf_female <- 0.5
  trend_adult_female = 0.2
  
  x <-
    bbs_survival_period(
      survival_adult_female = survival_adult_female,
      survival_calf_female = survival_calf_female,
      trend_adult_female = trend_adult_female
    )
  
  expect_true(all(unlist(x[1, , 3]) == unlist(x[2, , 3])))
  # year always increasing
  expect_true(all(x[1, , 3] >= x[1, 1, 3]))
  # check individual years
  expect_true(x[1, 2, 3] == ilogit(logit(survival_adult_female^(1/12)) + trend_adult_female))
  expect_true(x[1, 3, 3] == ilogit(logit(survival_adult_female^(1/12)) + trend_adult_female * 2))
  
  # first is equal to survival_adult_female
  expect_true(ilogit(logit(survival_calf_female^(1/12))) == x[1, 1, 1])
})

test_that("matrix survival period works", {
  survival_adult_female <- 0.84
  survival_calf_female <- 0.5

  x <-
    bbs_survival_period(
      survival_adult_female = survival_adult_female,
      survival_calf_female = survival_calf_female
    )
  
  y <- bbs_matrix_survival_period(x)
  
  expect_identical(dim(y), c(3L, 3L, 10L, 12L))
  expect_equal(y[1,1,1,1], survival_calf_female^(1/12))
})