test_that("bb_sims_base works", {
  
  nyear = 5
  fec <- fecundity_year(
    intercept = -1.3,
    stage = c(NA, NA, -0.1, NA, 0.05, NA),
    trend = -0.05,
    annual_sd = 0.1,
    nyear = 5)

  phi <- survival_period(
    intercept = 4.45,
    stage = c(-0.1, -0.1, 0, 0, 0.1, 0.1),
    trend = 0.1,
    annual_sd = 0.2,
    period_sd = 0.2,
    annual_period_sd = 0.1,
    nyear = nyear)
  
  x <- bb_sims_base(nyear = 5,
                    month_composition = 9,
               female_calves = 1000,
               male_calves = 1000,
               female_yearlings = 1000,
               male_yearlings = 1000,
               female_adults = 1000,
               male_adults = 1000,  
               survival_rates = phi,
               fecundity_rates = fec,
               collared_female_yearlings_month_year = matrix(5, 12, nyear),
               collared_female_adults_month_year = matrix(30, 12, nyear),
               probability_uncertain_mortality_month_year = matrix(0.1, 12, nyear),
               probability_uncertain_survivor_month_year = matrix(0, 12, nyear), # used?
               group_size_lambda_year = rep(10, nyear),
               group_size_theta_year = rep(2, nyear),
               max_group_proportion = 0.25,
               min_group_size = 3,
               groups_coverage_year = rep(0.2, nyear),
               proportion_adult_male_year = rep(0, nyear),
               probability_unsexed_adult_female_year = rep(0.1, nyear),
               probability_unsexed_adult_male_year = rep(0.1, nyear))
  
})
