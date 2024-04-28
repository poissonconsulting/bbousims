test_that("bbs_survival works", {
  b0_1 <- 0.94
  b0_2 <- 0.98
  intercept <- logit(c(b0_1, b0_2))

  x <-
    bbs_survival(
      intercept = intercept,
      nyear = 3,
      nperiod_within_year = 4
    )

  expect_equal(dim(x$eSurvival), c(4, 3, 2))
  expect_true(all(x$eSurvival[, , 1] == ilogit(logit(b0_1))))
  expect_true(all(x$eSurvival[, , 2] == ilogit(logit(b0_2))))
  expect_snapshot({
    print(x)
  })
})

test_that("bbs_survival with trend works", {
  b0_1 <- 0.94
  b0_2 <- 0.94
  intercept <- logit(c(b0_1, b0_2))
  trend <- c(0.1, 0.5)

  x <-
    bbs_survival(
      intercept = intercept,
      trend = trend,
      nyear = 3,
      nperiod_within_year = 4
    )

  ex <- x$eSurvival
  # first year equal to intercept
  expect_true(all(ex[, 1, 1] == ilogit(logit(b0_1))))
  expect_true(all(ex[, 1, 2] == ilogit(logit(b0_2))))
  # increasing within stage
  expect_true(all(ex[1, , 1] >= ex[1, 1, 1]))
  expect_true(all(ex[1, , 2] >= ex[1, 1, 2]))
  # stage 2 higher than stage 1
  expect_true(all(ex[1, , 2] >= ex[1, , 1]))
  expect_snapshot({
    print(x)
  })
})

test_that("bbs_survival with rndm annual works", {
  b0_1 <- 0.94
  b0_2 <- 0.94
  intercept <- logit(c(b0_1, b0_2))
  annual_sd <- c(0.5, 0)
  period_sd <- c(0, 0.5)

  withr::with_seed(10, {
    x <-
      bbs_survival(
        intercept = intercept,
        annual_sd = annual_sd,
        period_sd = period_sd,
        nyear = 3,
        nperiod_within_year = 4
      )

    ex <- x$eSurvival
    # first year not equal to intercept
    expect_false(all(ex[, 1, 1] == ilogit(logit(b0_1))))
    # first month not equal to intercept
    expect_false(all(ex[1, , 2] == ilogit(logit(b0_2))))
    # years not equal stage 1
    expect_equal(length(unique(ex[1, , 1])), 3)
    # month not equal stage 2
    expect_equal(length(unique(ex[, 1, 2])), 4)
    # within month equal stage 1
    expect_equal(length(unique(ex[1, , 2])), 1)
    # within year equal stage 2
    expect_equal(length(unique(ex[, 1, 1])), 1)

    expect_snapshot({
      print(x)
    })
  })
})

test_that("bbs_survival with rndm period and annual works", {
  b0_1 <- 0.94
  b0_2 <- 0.94
  intercept <- logit(c(b0_1, b0_2))
  annual_period_sd <- c(0.2, 2)

  withr::with_seed(10, {
    x <-
      bbs_survival(
        intercept = intercept,
        annual_period_sd = annual_period_sd,
        nyear = 3,
        nperiod_within_year = 4
      )

    ex <- x$eSurvival
    # test high variation between 0 and 1
    expect_true(all(unlist(ex) >= 0) & all(unlist(ex) <= 1))
    # first period not equal to intercept
    expect_false(all(ex[1, , 1] == ilogit(logit(b0_1))))
    # first year not equal to intercept
    expect_false(all(ex[, 1, 2] == ilogit(logit(b0_2))))
    # periods and years not equal
    expect_equal(length(unique(ex[, 1, 1])), 4)
    expect_equal(length(unique(ex[1, , 1])), 3)
    expect_equal(length(unique(ex[, 1, 2])), 4)
    expect_equal(length(unique(ex[1, , 2])), 3)
    expect_snapshot({
      print(x)
    })
  })
})

test_that("bbs_survival_caribou works with intercepts", {
  survival_adult_female <- 0.84
  survival_calf_female <- 0.5
  yearling_effect <- 0.2
  nyear <- 5

  x <-
    bbs_survival_caribou(
      survival_adult_female = survival_adult_female,
      survival_calf_female = survival_calf_female,
      yearling_effect = yearling_effect,
      nyear = nyear
    )

  ex <- x$eSurvival
  expect_true(all(unlist(ex[, , 3]) == ilogit(logit(survival_adult_female^(1 / 12)))))
  expect_true(all(unlist(ex[, , 1]) == ilogit(logit(survival_calf_female^(1 / 12)))))
  expect_true(all(unlist(ex[, , 2]) == ilogit(logit(survival_adult_female^(1 / 12)) + yearling_effect)))
  expect_snapshot({
    print(x)
  })
})

test_that("bbs_survival_caribou works with trend", {
  survival_adult_female <- 0.84
  survival_calf_female <- 0.5
  yearling_effect <- 0.2
  nyear <- 5

  x <-
    bbs_survival_caribou(
      survival_adult_female = survival_adult_female,
      survival_calf_female = survival_calf_female,
      yearling_effect = yearling_effect,
      trend_adult_female = 0.2,
      trend_calf_female = 0.5,
      nyear = nyear
    )

  ex <- x$eSurvival
  expect_true(all(as.vector(ex[, 1, 3]) == ilogit(logit(survival_adult_female^(1 / 12)))))
  expect_true(all(as.vector(ex[, 1, 2]) == ilogit(logit(survival_adult_female^(1 / 12)) + yearling_effect)))
  expect_true(all(as.vector(ex[, 1, 1]) == ilogit(logit(survival_calf_female^(1 / 12)))))
  expect_true(all(ex[1, , 1] >= ilogit(logit(survival_calf_female^(1 / 12)))))
  expect_equal(length(unique(ex[1, , 1])), nyear)
  expect_true(all(ex[1, , 3] >= ilogit(logit(survival_adult_female^(1 / 12)))))
  expect_equal(length(unique(ex[1, , 3])), nyear)
  expect_snapshot({
    print(x)
  })
})

test_that("bbs_survival_caribou works with random effects", {
  nyear <- 5

  withr::with_seed(10, {
    x <-
      bbs_survival_caribou(
        survival_adult_female = 0.84,
        survival_calf_female = 0.5,
        yearling_effect = 0.2,
        annual_sd_adult_female = 0.2,
        month_sd_calf_female = 0.1,
        annual_month_sd_calf_female = 0.5,
        nyear = nyear
      )

    ex <- x$eSurvival
    # no month variation adult and yearling
    expect_equal(length(unique(ex[, 1, 2])), 1)
    expect_equal(length(unique(ex[, 1, 3])), 1)

    # linear relationship logit yearling and adult
    expect_equal(length(unique(as.vector(round(logit(ex[, , 2]) - logit(ex[, , 3]), 2)))), 1)

    expect_snapshot({
      print(x)
    })
  })
})
