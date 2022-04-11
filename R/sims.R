#' Simulate Boreal Caribou Data from Model Parameters
#' 
#' @inheritParams bb_sims_base
#' @param survival_adult_females A probability of the annual female adult survival.
#' @param survival_fawns A probability of the annual female fawn survival.
#' @param fawns_per_adult_female The expected number of fawns per adult female.
#' @param collared_females The total number of collared females at the start of each month.
#' @param census_proportion The proportion of the population in the annual census.
#' @param survival_trend The effect of an increase of one year on the log-odds monthly survival.
#' @param survival_amplitude The amplitude of the effect of the annual cycle in survival.
#' @param survival_peak The month of the peak survival in the cycle.
#' @param survival_annual_sd The standard deviation of the annual variation on the adult female log-odds monthly survival.
#' @param survival_month_sd The standard deviation of the monthly variation on the adult female log-odds monthly survival.
#' @param survival_yearling_effect The effect of being a yearling on the adult female log-odds annual survival.
#' @param survival_male_effect The effect of being an male on the adult female log-odds annual survival.
#' @param fawns_per_yearling_female The expected number of fawns per yearling female.
#' @param collared_yearling_proportion The proportion of collared females that are yearlings.
#' @param probability_uncertain_mortality The probability of a mortality being uncertain.
#' @param probability_uncertain_survivor The probability of a survivor being uncertain.
#' @param group_size = 10,
#' @param group_phi = 10,
#' @param probability_unsexed_adult_female = 0,
#' @param probability_unsexed_adult_male = 0,
#' @param mixing_adult_male = 1
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
    female_adults = 1000,
    survival_adult_females = logit(0.95),
    survival_fawns = logit(0.95),
    fawns_per_adult_female = 0.3,
    collared_females = 30,
    census_proportion = 0.1,
    month_composition = 9,
    survival_trend = 0,
    survival_amplitude = 0,
    survival_peak = month_composition,
    survival_annual_sd = 0,
    survival_month_sd = 0,
    survival_yearling_effect = 0,
    survival_male_effect = 0,
    fawns_per_yearling_female = fawns_per_adult_female,
    collared_yearling_proportion = 0,
    probability_uncertain_mortality = 0,
    probability_uncertain_survivor = 0,
    group_size = 10,
    group_phi = 10,
    probability_unsexed_adult_female = 0,
    probability_unsexed_adult_male = 0,
    mixing_adult_male = 1
) {
  chk_whole_number(nyear)
  chk_gt(nyear)
  chk_whole_number(month_composition)
  chk_range(month_composition, c(1, 12))
  chk_number(mixing_adult_male)
  chk_range(mixing_adult_male)
  
  # will call bb_sims_base()
  
  NULL
}
