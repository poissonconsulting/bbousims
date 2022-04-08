#' Simulate Boreal Caribou Data from 
#' 
#' Returns a list of three tibbles.
#' The first named survival has columns year, month, starttotal, mortalitiescertain and mortalitiesuncertain.
#' The second named recruitment has columns year, month, cows, bulls, unknownadults, yearlings and calves.
#' And the final named abundances has columns year, month, sex, stage, abundance.
#' 
#' The survival for a month is the survival from the start to the end of a month.
#' The abundance for a month is the abundance at the end of the month.
#' The number of collared females of a life stage is the minimum of the
#' number specified and the number of individuals available.
#' Individual are assigned to census groups based on their relative proportions 
#' until there are no individuals remaining.
#' It's currently unclear how to implement mixing_adult_male_year.
#' The model includes demographic stochasticity.
#' 
#' @inheritParams template
#' @return A list of three tibbles.
#' @export
#'
#' @examples
#' bb_sims_base()
bb_sims_base <- function(
    nyear = 10,
    month_calving = 6,
    female_fawns = round(female_adults * mean(fawns_per_female_yearling_year)),
    male_fawns = female_fawns,
    female_yearlings = round(female_fawns * mean(survival_female_fawns_month_year)^12),
    male_yearlings = round(male_fawns * mean(survival_male_fawns_month_year)^12),
    female_adults = 1000,
    male_adults = round(female_adults * 0.65),
    survival_female_fawns_month_year = matrix(0.9, 12, nyear),
    survival_male_fawns_month_year = survival_male_fawns_month_year,
    survival_female_yearlings_month_year = matrix(0.9, 12, nyear),
    survival_male_yearlings_month_year = survival_female_yearlings_month_year * 0.9,
    survival_female_adults_month_year = survival_female_yearlings_month_year,
    survival_male_adults_month_year = survival_male_yearlings_month_year,
    fawns_per_female_yearling_year = rep(0.1, nyear),
    fawns_per_female_adult_year = fawns_per_female_yearling_year * 1.1,
    collared_female_yearlings_month_year = collared_female_adults_month_year,
    collared_female_adults_month_year = matrix(10, 12, nyear),
    probability_uncertain_mortality_month_year = matrix(0, 12, nyear),
    probability_uncertain_survivor_month_year = matrix(0, 12, nyear),
    month_census_year = rep(month_calving, nyear),
    group_size_year = rep(10, nyear),
    group_phi_year = rep(0, nyear),
    census_groups_year = rep(10, nyear),
    probability_unsexed_adult_female_year = rep(0, nyear),
    probability_unsexed_adult_male_year = rep(0, nyear),
    mixing_adult_male_year = rep(1, nyear)
) {
  chk_whole_number(nyear)
  chk_gt(nyear)
  chk_whole_number(month_calving)
  chk_range(month_calving, c(1, 12))
  chk_numeric(mixing_adult_male_year)
  chk_length(mixing_adult_male_year, nyear)
  chk_range(mixing_adult_male_year)

  NULL
}
