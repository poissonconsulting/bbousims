#' Simulate Boreal Caribou Data from Model Parameters
#'
#' @inheritParams bb_sims_base
#' @param survival_adult_females A probability of the annual female adult survival.
#' @param survival_calves A probability of the annual female calf survival.
#' @param calves_per_adult_female The expected number of calves per adult female.
#' @param collared_adult_females The total number of collared females at the start of each month.
#' @param groups_coverage The proportion of the groups in the composition survey.
#' @param survival_trend The effect of an increase of one year on the log-odds monthly survival.
#' @param survival_annual_sd The standard deviation of the annual variation on the adult female log-odds monthly survival.
#' @param survival_month_sd The standard deviation of the monthly variation on the adult female log-odds monthly survival.
#' @param survival_annual_month_sd The standard deviation of the monthly variation within year variation on the adult female log-odds monthly survival.
#' @param survival_yearling_effect The effect of being a yearling on the adult female log-odds annual survival.
#' @param survival_male_effect The effect of being an male on the adult female log-odds annual survival.
#' @param probability_uncertain_mortality The probability of a mortality being uncertain.
#' @param probability_uncertain_survivor The probability of a survivor being uncertain.
#' @param group_size The expected size of a g,
#' @param probability_unsexed_adult_female = 0,
#' @param probability_unsexed_adult_male = 0,
#' @param proportion_adult_male = 1
#'
#' @return A list of three tibbles.
#' The first named survival has columns year, month, starttotal, mortalitiescertain and mortalitiesuncertain.
#' The second named recruitment has columns year, month, cows, bulls, unknownadults, yearlings and calves.
#' And the final named abundance has columns year, month, sex, stage, abundance.
#' @export
#'
#' @examples
#' bb_sims()
bb_sims <- function(
    nyear = 10,
    month_composition = 9,
    adult_females = 1000,
    proportion_adult_female = 0.65,
    proportion_yearling_female = 0.5,
    survival_adult_female = 0.85,
    survival_calf = 0.5,
    survival_yearling_effect = 0,
    survival_trend = 0,
    survival_annual_sd = 0,
    survival_month_sd = 0,
    survival_annual_month_sd = 0,
    calves_per_adult_female = 0.8,
    calves_per_adult_female_trend = 0,
    calves_per_adult_female_annual_sd = 0,
    probability_unsexed_adult_female = 0,
    probability_unsexed_adult_male = 0,
    collared_adult_females = 30,
    month_collar = 3L,
    probability_uncertain_mortality = 0,
    probability_uncertain_survivor = 0,
    group_size = 5,
    group_coverage = 0.2) {
  
  chk_whole_number(nyear)
  chk_gt(nyear)
  chk_whole_number(month_composition)
  chk_range(month_composition, c(1, 12))
  # chk_number(proportion_adult_male)
  # chk_range(proportion_adult_male)
  
  dem <- bb_demographic_summary(sex_ratio = proportion_yearling_female, 
                                calves_per_adult_female = calves_per_adult_female,
                                survival_adult_female = survival_adult_female,
                                survival_calf = survival_calf)
  
  pop0 <- initial_population(adult_females = adult_females, 
                             dem$stable_stage_dist)
  
  # no yearling calves
  # matrix fecundity rates year x stage
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
  
  bb_sims_base(nyear = nyear,
               month_composition = month_composition,
               population_init = pop0,
               proportion_adult_female = proportion_adult_female,
               proportion_yearling_female = proportion_yearling_female,
               survival_month_year = phi,
               fecundity_year = fec,
               collared_adult_females = collared_adult_females,
               month_collar = month_collar,
               probability_uncertain_mortality_month_year = matrix(probability_uncertain_mortality, 12, nyear),
               probability_uncertain_survival_month_year = matrix(probability_uncertain_survivor, 12, nyear), 
               group_size_lambda_year = rep(group_size, nyear),
               group_size_theta_year = rep(0, nyear),
               group_max_proportion = 0.25,
               group_min_size = 2,
               group_coverage_year = rep(group_coverage, nyear),
               probability_unsexed_adult_female_year = rep(probability_unsexed_adult_female, nyear),
               probability_unsexed_adult_male_year = rep(probability_unsexed_adult_male, nyear))
}
