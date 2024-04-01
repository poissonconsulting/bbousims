test_that("simulations", {
  
# recruitment trend -------------------------------------------------------
  set.seed(101)
  nyear <- 30
  month_composition = 9
  adult_females = 1000
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_calf = 0.5
  month_collar = 1L
  survival_adult_female = 0.85
  survival_trend = 0
  survival_annual_sd = 0.1
  survival_month_sd = 0.1
  survival_annual_month_sd = 0.05
  survival_yearling_effect = 0
  calves_per_adult_female_annual_sd = 0.1
  probability_uncertain_survivor = 0.01
  probability_uncertain_mortality = 0.05
  probability_unsexed_adult_female = 0.03
  probability_unsexed_adult_male = 0.02
  
  # 3 recruitment scenarios
  # 3 sample size scenarios
  # 9 simulations total
  calves_per_adult_female <- c(0, 0.1, 0.2)
  calves_per_adult_female_trend <- c(0.1)
  # this should adjust coverage?
  group_coverage <- c(0.1, 0.3, 0.5)
  collared_adult_females <- c(10, 30, 50)
  scenarios <- tidyr::expand_grid(calves_per_adult_female = calves_per_adult_female,
                                  group_coverage = group_coverage,
                                  collared_adult_females = collared_adult_females)
  x <- purrr::map(1:nrow(scenarios), ~ {
    cpaf <- scenarios$calves_per_adult_female[.x]
    cov <- scenarios$group_coverage[.x]
    collar <- scenarios$collared_adult_females[.x]
    x <- bb_sims(calves_per_adult_female = cpaf,
                 collared_adult_females = collar,
                 group_coverage = cov,
                 calves_per_adult_female_trend = 0.1,
                 group_size = 15,
                 nyear = 30,
                 month_composition = 9,
                 adult_females = 1000,
                 proportion_adult_female = 0.65,
                 proportion_yearling_female = 0.5,
                 survival_calf = 0.5,
                 month_collar = 1L,
                 survival_adult_female = 0.85,
                 survival_trend = 0,
                 survival_annual_sd = 0.1,
                 survival_month_sd = 0.1,
                 survival_annual_month_sd = 0.05,
                 survival_yearling_effect = 0,
                 calves_per_adult_female_annual_sd = 0.1,
                 probability_uncertain_survivor = 0.01,
                 probability_uncertain_mortality = 0.05,
                 probability_unsexed_adult_female = 0.03,
                 probability_unsexed_adult_male = 0.02)
    x
  })
 
})
