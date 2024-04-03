test_that("simulations", {
  skip()
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
  survival_trend_adult_female = 0
  survival_annual_sd_adult_female = 0.1
  survival_month_sd_adult_female = 0.1
  survival_annual_month_sd_adult_female = 0.05
  survival_annual_sd_calf_female = 0.1
  survival_month_sd_calf_female = 0.1
  survival_annual_month_sd_calf_female = 0.05
  survival_yearling_effect = 0
  calves_per_adult_female_annual_sd = 0.1
  probability_uncertain_survivor = 0.01
  probability_uncertain_mortality = 0.05
  probability_unsexed_adult_female = 0.03
  probability_unsexed_adult_male = 0.02
  
  # 2 recruitment scenarios
  # 2 sample size scenarios
  # 4 simulations total
  survival_calf_female <- c(0.5, 0.7)
  survival_trend_calf_female <- c(0.1)
  coverage <- 1:2
  group_coverage <- c(0.1, 0.6)
  collared_adult_females <- c(10, 50)
  scenario <- tidyr::expand_grid(survival_calf_female,
                                  survival_trend_calf_female,
                                  coverage) %>%
    dplyr::mutate(group_coverage = dplyr::case_when(coverage == 1 ~ group_coverage[1],
                                      coverage == 2 ~ group_coverage[2]),
           collared_adult_females = dplyr::case_when(coverage == 1 ~ collared_adult_females[1],
                                              coverage == 2 ~ collared_adult_females[2]))
  
  x <- purrr::map(1:nrow(scenario), ~ {
    scf <- scenario$survival_calf_female[.x]
    cov <- scenario$group_coverage[.x]
    collar <- scenarios$collared_adult_females[.x]
    x <- bbs_simulate_caribou(survival_calf_female = scf,
                 collared_adult_females = collar,
                 group_coverage = cov,
                 group_size = 15,
                 nyear = 30,
                 month_composition = 9,
                 adult_females = 500,
                 proportion_adult_female = 0.65,
                 proportion_yearling_female = 0.5,
                 month_collar = 3L,
                 survival_adult_female = 0.85,
                 survival_trend = 0,
                 survival_annual_sd_adult_female = 0.1,
                 survival_month_sd_adult_female = 0.1,
                 survival_annual_month_sd_adult_female = 0.05,
                 survival_annual_sd_calf_female = 0.1,
                 survival_month_sd_calf_female = 0.1,
                 survival_annual_month_sd_calf_female = 0.05,
                 survival_yearling_effect = 0,
                 calves_per_adult_female_annual_sd = 0.05,
                 probability_uncertain_survivor = 0.01,
                 probability_uncertain_mortality = 0.05,
                 probability_unsexed_adult_female = 0.03,
                 probability_unsexed_adult_male = 0.02)
    x
  })
 
})
