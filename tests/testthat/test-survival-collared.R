test_that("survival collared works", {
  withr::with_seed(10, {
    ncollar <- 30
    phi <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(ncollar,
      survival_adult_female_month_year = saf
    )
    expect_true(all(x$StartTotal[x$Month == 1] == ncollar))
    expect_snapshot({
      print(x)
    })
  })
})

test_that("survival collared works with different collar month", {
  withr::with_seed(10, {
    month_collar <- 3
    ncollar <- 30
    phi <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(ncollar,
      month_collar = month_collar,
      survival_adult_female_month_year = saf
    )
    expect_true(all(x$StartTotal[x$Month == month_collar] == ncollar))
    expect_snapshot({
      print(x)
    })
  })
})

test_that("survival collared works with uncertain mort", {
  withr::with_seed(10, {
    phi <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(30,
      survival_adult_female_month_year = saf,
      probability_uncertain_mortality = 0.5
    )
    expect_snapshot({
      print(x)
    })
  })
})

test_that("survival collared low survival works", {
  withr::with_seed(10, {
    phi <- bbs_survival_caribou(0.1, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(30,
      survival_adult_female_month_year = saf,
      probability_uncertain_mortality = 0.05
    )
    expect_snapshot({
      print(x)
    })
  })
})

test_that("survival collared low collars works", {
  withr::with_seed(10, {
    phi <- bbs_survival_caribou(0.1, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(5,
      survival_adult_female_month_year = saf,
      probability_uncertain_mortality = 0.05
    )
    expect_snapshot({
      print(x)
    })
  })
})
