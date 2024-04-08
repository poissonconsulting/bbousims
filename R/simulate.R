#' Simulate Boreal Caribou Data 
#' 
#' Simulate Boreal Caribou data from key demographic and sampling parameters.
#' 
#' See [bbs_survival_caribou()] and [bbs_fecundity_caribou()] for details on how rates are generated.
#' See [bbs_population_caribou()] for details on how population is simulated.
#' See [bbs_population_groups_survey()] for details on how groups are assigned for each composition survey.
#' See [bbs_survival_collared()] for details on how survival of collared adult female is determined.
#' 
#' Survival and recrutiment data.frames generated are formatted to be used as input data to [bboutools::bb_fit_survival()] and [bboutools::bb_fit_recruitment()], respectively. 
#'
#' @inheritParams params
#' @param population_name A string of the population name. This does not affect simulation but can be used as a unique identifier.
#' @param group_size A whole number of the average group size. Group sizes are drawn from a poisson distribution with lambda of `group_size`. 
#'
#' @return A list of three tibbles.
#' The first named survival has columns Year, Month, StartTotal, MortalitiesCertain and MortalitiesUncertain.
#' The second named recruitment has columns Year, Month, Cows, Bulls, UnknownAdults, Yearlings and Calves.
#' And the final named abundance has columns Year, Month, Stage, Abundance.
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
    group_max_proportion = 1,
    population_name = "A") {
  
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
  
  abundance <- abundance_tbl(population, 
                             population_name = population_name)
  
  recruitment <- recruitment_tbl(groups, 
                                 month_composition = month_composition,
                                 probability_unsexed_adult_male = probability_unsexed_adult_male,
                                 probability_unsexed_adult_female = probability_unsexed_adult_female,
                                 population_name = population_name)
  
  survival <- attr(population, "survival")
  survival_adult_female_month_year <- survival[,,3]
  survival <- bbs_survival_collared(collared_adult_females = collared_adult_females,
                                month_collar = month_collar,
                                survival_adult_female_month_year = survival_adult_female_month_year,
                                probability_uncertain_mortality = probability_uncertain_mortality,
                                probability_uncertain_survival = probability_uncertain_survival, 
                                population_name = population_name)
  
  x <- list(survival = survival,
       recruitment = recruitment,
       abundance = abundance)
  
  x <- class_bbou_simulation(x)
  x
}

