#' Simulate Boreal Caribou Data from Base Parameter
#'
#' The survival for a month is the survival from the start to the end of a month.
#' Except for the starting values, the abundance for a month is the abundance at the end of the month.
#' The number of collared females of a life stage is the minimum of the
#' number specified and the number of individuals available.
#' Individual are assigned to composition groups based on their relative proportions
#' until there are no individuals remaining.
#' Cows, bulls and yearlings are assigned to groups based on their relative proportions.
#' If a cow is assigned and there is space for one more individual then a calf is a
#' For each cow (or yearling if have calves) probability of having a calf.
#' The model includes demographic stochasticity.
#'
#' @param nyear The number of caribou years.
#' @param month_composition A number between 1 and 12 of the caribou month that composition survey occurs relative to the calving month.
#' @param population_init A vector of length 3 corresponding to the population of female calves, female yearlings and female adults at the start of the first year.
#' @param proportion_adult_females A number between 0 and 1 of the proportion of adults that are female. 
#' @param proportion_yearling_females A number between 0 and 1 of the proportion of adults that are female. This applies to calf sex ratio as well. 
#' @param survival_month_year An array of dimensions 12 x nyear x 3. The third dimension represents female calf, female yearling and female adult stages.
#' @param fecundity_year A matrix of of dimensions nyear x 3. The second dimension corresponds to female calf, female yearling and female adult stages.
#' @param collared_adult_females A whole positive number of the number of collared adult females. Each year the total collared adult females will be topped up to equal this number. 
#' @param probability_uncertain_mortality_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a mortality being uncertain.
#' @param probability_uncertain_survivor_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a survivor being uncertain.
#' @param group_size_lambda_year A vector of length nyear of the expected group size.
#' @param group_size_theta_year A vector of length nyear of the standard deviation of the extra-Poisson variation in the group size.
#' @param groups_coverage_year A vector of length nyear of the proportion of the groups in the composition survey.
#' @param probability_unsexed_adult_female_year A vector of length nyear of the probability of an adult female being unsexed.
#' @param probability_unsexed_adult_male_year A vector of length nyear of the probability of an adult male being unsexed.

#' @return A list of three tibbles.
#' The first named survival has columns year, month, starttotal, mortalitiescertain and mortalitiesuncertain.
#' The second named recruitment has columns year, month, cows, bulls, unknownadults, yearlings and calves.
#' And the final named abundance has columns year, month, sex, stage, abundance.
#'
#' @examples
#' bb_sims_base()
bb_sims_base <- function(
    nyear,
    month_composition,
    population_init,
    proportion_adult_female,
    proportion_yearling_female,
    survival_month_year,
    fecundity_year,
    collared_adult_females,
    month_collar,
    probability_uncertain_mortality_month_year,
    probability_uncertain_survival_month_year,
    group_size_lambda_year,
    group_size_theta_year,
    group_coverage_year,
    group_min_size,
    group_max_proportion,
    probability_unsexed_adult_female_year,
    probability_unsexed_adult_male_year) {
  chk_whole_number(nyear)
  chk_gt(nyear)
  chk_whole_number(month_composition)
  chk_range(month_composition, c(1, 12))
  
  survival_matrices <- matrix_survival_period(survival_month_year)
  birth_matrices <- matrix_birth_year(fecundity_year)
  age_matrix <- matrix_age()
  
  # survival then ageing then birth (BAS model)
  population <- simulate_population_base(population_init, 
                                         birth = birth_matrices, 
                                         survival = survival_matrices,
                                         age = age_matrix,
                                         proportion_adult_female = proportion_adult_female,
                                         proportion_yearling_female = proportion_yearling_female,
                                         stochastic = TRUE)
  
  groups <- population_groups_survey(population,
                                        month_composition = month_composition,
                                        group_size_lambda_year = group_size_lambda_year,
                                        group_size_theta_year = group_size_theta_year,
                                        group_coverage_year = group_coverage_year,
                                        group_min_size = group_min_size,
                                        group_max_proportion = group_max_proportion)
  
  abundance <- abundance_tbl(population)
  
  recruitment <- recruitment_tbl(groups, 
                                 month_composition = month_composition,
                                 probability_unsexed_adult_male_year = probability_unsexed_adult_male_year,
                                 probability_unsexed_adult_female_year = probability_unsexed_adult_female_year)
  
  survival_adult_female_month_year <- survival_month_year[,,3]
  survival <- survival_collared(collared_adult_females = collared_adult_females,
                                month_collar = month_collar,
                              survival_adult_female_month_year = survival_adult_female_month_year,
                              probability_uncertain_mortality_month_year = probability_uncertain_mortality_month_year,
                              probability_uncertain_survival_month_year = probability_uncertain_survival_month_year)
  
  list(survival = survival,
       recruitment = recruitment,
       abundance = abundance)
}

# dem <- bb_demographic_summary(sex_ratio = proportion_yearling_female, 
#                               calves_per_adult_female = calves_per_adult_female,
#                               survival_adult_female = survival_adult_female,
#                               survival_calf = survival_calf)
# 
# pop0 <- initial_population(adult_females = adult_females,
#                            stable_stage_dist = dem$stable_stage_dist)
# 
# fec <- fecundity_year(
#   calves_per_adult_female = calves_per_adult_female,
#   trend = calves_per_adult_female_trend,
#   annual_sd = calves_per_adult_female_annual_sd,
#   nyear = nyear)
# 
# # array survival rates, month x year x stage
# phi <- survival_period(
#   survival_adult_female = survival_adult_female,
#   survival_calf = survival_calf,
#   trend = survival_trend,
#   annual_sd = survival_annual_sd,
#   period_sd = survival_month_sd,
#   annual_period_sd = survival_annual_month_sd,
#   nyear = nyear)
# 
