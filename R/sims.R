#' Simulate Boreal Caribou Data from key demographic and sampling rates 
#'
#' @inheritParams params
#'
#' @return A list of three tibbles.
#' The first named survival has columns year, month, starttotal, mortalitiescertain and mortalitiesuncertain.
#' The second named recruitment has columns year, month, cows, bulls, unknownadults, yearlings and calves.
#' And the final named abundance has columns year, month, sex, stage, abundance.
#' @export
#'
#' @examples
#' bbs_simulate_caribou()
bbs_simulate_caribou <- function(
    adult_females = 1000,
    nyear = 20,
    survival_adult_female = 0.85, # annual
    survival_calf_female = 0.5, # annual
    calves_per_adult_female = 0.7, # annual
    proportion_adult_female = 0.65,
    proportion_yearling_female = 0.5,
    survival_trend_adult_female = 0,
    survival_trend_calf_female = 0,
    survival_annual_sd_adult_female = 0,
    survival_annual_sd_calf_female = 0,
    survival_month_sd_adult_female = 0,
    survival_month_sd_calf_female = 0,
    survival_annual_month_sd_adult_female = 0,
    survival_annual_month_sd_calf_female = 0,
    calves_per_adult_female_trend = 0,
    calves_per_adult_female_annual_sd = 0,
    probability_unsexed_adult_female = 0,
    probability_unsexed_adult_male = 0,
    month_composition = 9L,
    collared_adult_females = 30,
    month_collar = 3L,
    probability_uncertain_mortality = 0,
    probability_uncertain_survival = 0,
    group_size = 5,
    group_coverage = 0.2,
    group_min_size = 3,
    group_max_proportion = 1) {
  
  population <- bbs_population_caribou(adult_females = adult_females,
                                       nyear = nyear,
                                       survival_adult_female = survival_adult_female,
                                       survival_calf_female = survival_calf_female, 
                                       calves_per_adult_female = calves_per_adult_female, 
                                       proportion_adult_female = proportion_adult_female,
                                       proportion_yearling_female = proportion_yearling_female,
                                       survival_trend_adult_female = survival_trend_adult_female,
                                       survival_trend_calf_female = survival_trend_calf_female,
                                       survival_annual_sd_adult_female = survival_annual_sd_adult_female,
                                       survival_annual_sd_calf_female = survival_annual_sd_calf_female,
                                       survival_month_sd_adult_female = survival_month_sd_adult_female,
                                       survival_month_sd_calf_female = survival_month_sd_calf_female,
                                       survival_annual_month_sd_adult_female = survival_annual_month_sd_adult_female,
                                       survival_annual_month_sd_calf_female = survival_annual_month_sd_calf_female,
                                       calves_per_adult_female_trend = calves_per_adult_female_trend,
                                       calves_per_adult_female_annual_sd = calves_per_adult_female_annual_sd)
  
  groups <- bbs_population_groups_survey(population,
                                     month_composition = month_composition,
                                     group_size_lambda = group_size,
                                     group_size_theta = 0,
                                     group_coverage = group_coverage,
                                     group_min_size = group_min_size,
                                     group_max_proportion = group_max_proportion)
  
  abundance <- abundance_tbl(population)
  
  recruitment <- recruitment_tbl(groups, 
                                 month_composition = month_composition,
                                 probability_unsexed_adult_male = probability_unsexed_adult_male,
                                 probability_unsexed_adult_female = probability_unsexed_adult_female)
  
  survival <- attr(population, "survival")
  survival_adult_female_month_year <- survival[,,3]
  survival <- bbs_survival_collared(collared_adult_females = collared_adult_females,
                                month_collar = month_collar,
                                survival_adult_female_month_year = survival_adult_female_month_year,
                                probability_uncertain_mortality = probability_uncertain_mortality,
                                probability_uncertain_survival = probability_uncertain_survival)
  
  list(survival = survival,
       recruitment = recruitment,
       abundance = abundance)

}

