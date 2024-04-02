test_that("demographic summary works", {
  calves_per_adult_female = 0.9
  proportion_female = 0.5
  survival_adult_female = 0.85
  survival_calf = 0.5
  survival_yearling = survival_adult_female
  
  dem <- bbs_demographic_summary(proportion_female = proportion_female, 
                                calves_per_adult_female = calves_per_adult_female,
                                survival_adult_female = survival_adult_female,
                                survival_calf = survival_calf,
                                survival_yearling = survival_yearling)
  
  expect_true(is.list(dem))
  expect_snapshot(dem)
})

test_that("initial population works", {
  calves_per_adult_female = 0.9
  proportion_female = 0.5
  survival_adult_female = 0.85
  survival_calf = 0.5
  survival_yearling = survival_adult_female
  
  dem <- bbs_demographic_summary(proportion_female = proportion_female, 
                                 calves_per_adult_female = calves_per_adult_female,
                                 survival_adult_female = survival_adult_female,
                                 survival_calf = survival_calf,
                                 survival_yearling = survival_yearling)
  
  pop0 <- initial_population(1000,
                             stable_stage_dist = dem$stable_stage_dist)
  expect_snapshot(pop0)
})

