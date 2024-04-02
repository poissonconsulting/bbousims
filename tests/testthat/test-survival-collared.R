test_that("survival collared works", {
  set.seed(101)
  phi <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
  x <- bbs_survival_collared(30, survival_adult_female_month_year = phi[,,3])
  expect_true(all(x$MortalitiesUncertain == 0))
  expect_snapshot_data(x, "survival_collared")
})

test_that("survival collared no uncertain works", {
  set.seed(101)
  phi <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
  x <- bbs_survival_collared(30, 
                             survival_adult_female_month_year = phi[,,3],
                             probability_uncertain_mortality = 0.5)
  expect_false(all(x$MortalitiesUncertain == 0))
  expect_snapshot_data(x, "survival_collared_uncertain")
})

test_that("survival collared low survival works", {
  set.seed(101)
  phi <- bbs_survival_caribou(0.1, survival_calf_female = 0.5)
  x <- bbs_survival_collared(20, 
                             survival_adult_female_month_year = phi[,,3],
                             probability_uncertain_mortality = 0.5)
  expect_true(any(x$StartTotal == 0))
  expect_snapshot_data(x, "survival_collared_dead")
})
