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
    female_adults = 1000,
    survival_adult_females = 0.83,
    survival_calves = 0.83,
    calves_per_adult_female = 0.3,
    collared_adult_females = 30,
    groups_coverage = 0.1,
    survival_trend = 0,
    survival_annual_sd = 0,
    survival_month_sd = 0,
    survival_annual_month_sd = 0,
    survival_yearling_effect = 0,
    survival_male_effect = 0,
    probability_uncertain_mortality = 0,
    probability_uncertain_survivor = 0,
    group_size = 10,
    probability_unsexed_adult_female = 0,
    probability_unsexed_adult_male = 0,
    proportion_adult_male = 0
) {
  chk_whole_number(nyear)
  chk_gt(nyear)
  chk_whole_number(month_composition)
  chk_range(month_composition, c(1, 12))
  chk_number(proportion_adult_male)
  chk_range(proportion_adult_male)
  
  # will call bb_sims_base()
  
  NULL
}
