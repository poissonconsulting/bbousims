test_that("demographic summary works", {
  calves_per_adult_female = 0.9
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_adult_female = 0.85
  survival_calf = 0.5
  survival_yearling = survival_adult_female
  
  dem <- bb_demographic_summary(calves_per_adult_female = calves_per_adult_female,
                                proportion_adult_female = proportion_adult_female,
                                proportion_yearling_female = proportion_yearling_female,
                                survival_adult_female = survival_adult_female,
                                survival_calf = survival_calf,
                                survival_yearling = survival_yearling)
  
  expect_true(is.list(dem))
  expect_snapshot(dem)
})

test_that("initial population works", {
  adults_female = 1000
  calves_per_adult_female = 0.9
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_adult_female = 0.85
  survival_calf = 0.5
  survival_yearling = survival_adult_female
  
  dem <- bb_demographic_summary(calves_per_adult_female = calves_per_adult_female,
                                proportion_adult_female = proportion_adult_female,
                                proportion_yearling_female = proportion_yearling_female,
                                survival_adult_female = survival_adult_female,
                                survival_calf = survival_calf,
                                survival_yearling = survival_yearling)
  
  
  pop0 <- initial_population(1000,
                             stable_stage_dist = dem$stable_stage_dist,
                             proportion_adult_female = proportion_adult_female,
                             proportion_yearling_female = proportion_yearling_female)
  expect_snapshot(pop0)
})

