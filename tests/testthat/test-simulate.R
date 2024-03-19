test_that("simulating population with varying survival/fecundity rates works", {
  nyear = 10
  adult_females = 1000
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_adult_female = 0.84
  survival_calf = 0.5
  calves_per_adult_female = 0.5
  survival_trend = 0.2
  survival_annual_sd = 0.1
  survival_month_sd = 0.1
  survival_annual_month_sd = 0.1
  survival_yearling_effect = 0.05
  calves_per_adult_female_trend = 0.1
  calves_per_adult_female_annual_sd = 0.1
  
  dem <- bb_demographic_summary(sex_ratio = proportion_yearling_female, 
                                calves_per_adult_female = calves_per_adult_female,
                                survival_adult_female = survival_adult_female,
                                survival_calf = survival_calf)
  
  pop0 <- initial_population(adult_females = adult_females, 
                             dem$stable_stage_dist)
  
  # no yearling calves
  # matrix fecundity rates year x stage
  set.seed(101)
  fec <- fecundity_year(
    calves_per_adult_female = calves_per_adult_female,
    trend = calves_per_adult_female_trend,
    annual_sd = calves_per_adult_female_annual_sd,
    nyear = nyear)
  
  # array survival rates, month x year x stage
  phi <- survival_period(
    survival_adult_female = survival_adult_female,
    survival_calf = survival_calf,
    trend = survival_trend,
    annual_sd = survival_annual_sd,
    period_sd = survival_month_sd,
    annual_period_sd = survival_annual_month_sd,
    nyear = nyear)
  
  survival_matrices <- matrix_survival_period(phi)
  birth_matrices <- matrix_birth_year(fec)
  age_matrix <- matrix_age(c(2, 3, 3))
  
  # survival then ageing then birth (BAS model)
  population <- simulate_population_base(pop0, 
                                    birth = birth_matrices, 
                                    survival = survival_matrices,
                                    age = age_matrix,
                                    proportion_adult_female = proportion_adult_female,
                                    proportion_yearling_female = proportion_yearling_female)
  names(pop0) <- NULL
  expect_equal(population[c(1, 3, 5),1], pop0)
  expect_true(is.matrix(population))
  nstep <- as.integer(nyear*nperiod_within_year + 1)
  expect_equal(dim(population), c(6, nstep))
  expect_snapshot(population)
})

test_that("bb_simulate_population stochastic works", {
  nyear = 40
  adult_females = 1000
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_adult_female = 0.84
  survival_calf = 0.5
  calves_per_adult_female = 0.9
  survival_trend = 0
  survival_annual_sd = 0.1
  survival_month_sd = 0.1
  survival_annual_month_sd = 0.1
  survival_yearling_effect = 0
  calves_per_adult_female_trend = 0
  calves_per_adult_female_annual_sd = 0.1
  
  set.seed(101)
  population <- bb_simulate_population(nyear = nyear, 
                                       adult_females = adult_females, 
                                       proportion_adult_female = proportion_adult_female, 
                                       proportion_yearling_female = proportion_yearling_female, 
                                       survival_adult_female = survival_adult_female,
                                       survival_calf = survival_calf,
                                       survival_trend = survival_trend,
                                       survival_annual_sd = survival_annual_sd,
                                       survival_month_sd = survival_month_sd,
                                       survival_annual_month_sd = survival_annual_month_sd)
  
  expect_true(is.matrix(population))
  expect_silent(chk_whole_numeric(population))
})

test_that("bb_simulate_population deterministic works", {
  nyear = 10
  adult_females = 1000
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_adult_female = 0.84
  survival_calf = 0.5
  calves_per_adult_female = 0.5
  survival_trend = 0
  survival_annual_sd = 0.1
  survival_month_sd = 0.1
  survival_annual_month_sd = 0.1
  survival_yearling_effect = 0.05
  calves_per_adult_female_trend = 0
  calves_per_adult_female_annual_sd = 0.1
  
  population <- bb_simulate_population(nyear = nyear, 
                                       adult_females = adult_females, 
                                       proportion_adult_female = proportion_adult_female, 
                                       proportion_yearling_female = proportion_yearling_female, 
                                       survival_adult_female = survival_adult_female,
                                       survival_calf = survival_calf,
                                       survival_trend = survival_trend,
                                       survival_annual_sd = survival_annual_sd,
                                       survival_month_sd = survival_month_sd,
                                       survival_annual_month_sd = survival_annual_month_sd,
                                       stochastic = FALSE)
  
  expect_true(is.matrix(population))
  expect_chk_error(chk_whole_numeric(population))
})
