#' #' Simulate Boreal Caribou Data from Base Parameter
#' #'
#' #' The survival for a month is the survival from the start to the end of a month.
#' #' Except for the starting values, the abundance for a month is the abundance at the end of the month.
#' #' The number of collared females of a life stage is the minimum of the
#' #' number specified and the number of individuals available.
#' #' Individual are assigned to composition groups based on their relative proportions
#' #' until there are no individuals remaining.
#' #' Cows, bulls and yearlings are assigned to groups based on their relative proportions.
#' #' If a cow is assigned and there is space for one more individual then a calf is a
#' #' For each cow (or yearling if have calves) probability of having a calf.
#' #' The model includes demographic stochasticity.
#' #'
#' #' @param nyear The number of caribou years.
#' #' @param month_composition A number between 1 and 12 of the caribou month that composition survey occurs relative to the calving month.
#' #' @param female_yearlings The number of female yearlings at the start of the first (calving) month in the first year.
#' #' @param male_yearlings The number of male yearlings at the start of the first (calving) month in the first year.
#' #' @param female_adults The number of female adults at the start of the first (calving) month in the first year.
#' #' @param male_adults The number of male adults at the start of the first (calving) month in the first year.
#' #' @param survival_calves_year A vector of length nyear of the calf survival from the composition survey to calving.
#' #' @param survival_female_yearlings_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female yearling.
#' #' @param survival_male_yearlings_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male yearling.
#' #' @param survival_female_adults_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female adult.
#' #' @param survival_male_adults_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male adult.
#' #' @param calves_per_female_yearling_year A vector of length nyear of the expected number of calves per female yearling during the composition survey.
#' #' @param calves_per_female_adult_year A vector of length nyear of the expected number of calves per female adult during the composition survey.
#' #' @param collared_female_yearlings_month_year A matrix of dimensions 12 x nyear of the number of collared female yearlings at the start of the month.
#' #' @param collared_female_adults_month_year A matrix of dimensions 12 x nyear of the number of collared female adults at the start of the month.
#' #' @param probability_uncertain_mortality_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a mortality being uncertain.
#' #' @param probability_uncertain_survivor_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a survivor being uncertain.
#' #' @param group_size_year A vector of length nyear of the expected group size.
#' #' @param group_phi_year A vector of length nyear of the standard deviation of the extra-Poisson variation in the group size.
#' #' @param groups_coverage_year A vector of length nyear of the proportion of the groups in the composition survey.
#' #' @param proportion_adult_male_year A vector of length nyear of the proportion of the adult males in mixed groups in the composition survey.
#' #' @param probability_unsexed_adult_female_year A vector of length nyear of the probability of an adult female being unsexed.
#' #' @param probability_unsexed_adult_male_year A vector of length nyear of the probability of an adult male being unsexed.
#' 
#' #' @return A list of three tibbles.
#' #' The first named survival has columns year, month, starttotal, mortalitiescertain and mortalitiesuncertain.
#' #' The second named recruitment has columns year, month, cows, bulls, unknownadults, yearlings and calves.
#' #' And the final named abundance has columns year, month, sex, stage, abundance.
#' #' @export
#' #'
#' #' @examples
#' #' bb_sims_base()
#' bb_sims_base <- function(
#'     nyear = 10,
#'     month_composition = 9,
#'     female_yearlings = NULL, # if null simulates values based female adults at equilibrium
#'     male_yearlings = female_yearlings,
#'     female_adults = 1000,
#'     male_adults = NULL, # if null simulates expected values based on female adults at equilibrium
#'     survival_calves_year = rep(0.985^(12 - month_composition), nyear),
#'     survival_female_yearlings_month_year = survival_female_adults_month_year,
#'     survival_male_yearlings_month_year = survival_male_adults_month_year,
#'     survival_female_adults_month_year = matrix(0.985, 12, nyear),
#'     survival_male_adults_month_year = survival_female_adults_month_year * 0.99,
#'     calves_per_female_yearling_year = rep(0, nyear),
#'     calves_per_female_adult_year = rep(0.3, nyear),
#'     collared_female_yearlings_month_year = matrix(0, 12, nyear),
#'     collared_female_adults_month_year = matrix(30, 12, nyear),
#'     probability_uncertain_mortality_month_year = matrix(0, 12, nyear),
#'     probability_uncertain_survivor_month_year = matrix(0, 12, nyear),
#'     group_size_year = rep(10, nyear),
#'     group_phi_year = rep(0, nyear),
#'     groups_coverage_year = 0.2,
#'     proportion_adult_male_year = rep(0, nyear),
#'     probability_unsexed_adult_female_year = rep(0, nyear),
#'     probability_unsexed_adult_male_year = rep(0, nyear)) {
#'   chk_whole_number(nyear)
#'   chk_gt(nyear)
#'   chk_whole_number(month_composition)
#'   chk_range(month_composition, c(1, 12))
#'   chk_numeric(proportion_adult_male_year)
#'   chk_length(proportion_adult_male_year, nyear)
#'   chk_range(proportion_adult_male_year)
#' 
#'   NULL
#' }
