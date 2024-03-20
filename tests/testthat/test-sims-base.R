test_that("bb_sims_base works", {
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
  
  x <- bb_sims_base(month_composition = 9,
                    nyear = nyear,
                    population_init = pop0,
                    proportion_adult_female = proportion_adult_female,
                    proportion_yearling_female = proportion_yearling_female,
                    survival_month_year = phi,
                    fecundity_year = fec,
                    collared_adult_females = 30,
                    month_collar = 3,
               probability_uncertain_mortality_month_year = matrix(0.1, 12, nyear),
               probability_uncertain_survival_month_year = matrix(0.01, 12, nyear), 
               group_size_lambda_year = rep(10, nyear),
               group_size_theta_year = rep(0.2, nyear),
               group_max_proportion = 0.3,
               group_min_size = 2,
               group_coverage_year = rep(0.2, nyear),
               probability_unsexed_adult_female_year = rep(0.1, nyear),
               probability_unsexed_adult_male_year = rep(0.1, nyear))
  
  expect_true(is.list(x))
  expect_identical(names(x), c("survival", "recruitment", "abundance"))
  expect_true(all(unlist(lapply(x, is.data.frame))))
})
