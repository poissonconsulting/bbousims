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
    adults = 1000,
    proportion_adult_female = 0.65,
    proportion_yearling_female = 0.5,
    survival = 0.985,
    survival_trend = 0.1,
    survival_annual_sd = 0.25,
    survival_month_sd = 0.25,
    survival_annual_month_sd = 0,
    # survival_calves = 0.83,
    calves_per_adult_female = 0.2,
    calves_per_adult_female_trend = 0.1,
    calves_per_adult_female_annual_sd = 0.25,
    collared_adult_females = 30,
    # survival_yearling_effect = 0,
    # survival_male_effect = 0,
    probability_uncertain_mortality = 0,
    # probability_uncertain_survivor = 0,
    group_size = 5,
    group_min_size = 2,
    group_max_proportion = 0.2,
    groups_coverage = 0.2,
    probability_unsexed_adult_female = 0,
    probability_unsexed_adult_male = 0) {
  chk_whole_number(nyear)
  chk_gt(nyear)
  chk_whole_number(month_composition)
  chk_range(month_composition, c(1, 12))
  # chk_number(proportion_adult_male)
  # chk_range(proportion_adult_male)
  
  # no yearling calves
  fec <- fecundity_year(
    intercept = log(calves_per_adult_female),
    stage = c(NA, NA, NA, NA, 0, NA),
    trend = calves_per_adult_female_trend,
    annual_sd = calves_per_adult_female_annual_sd,
    nyear = nyear)
  
  phi <- survival_period(
    intercept = logit(survival),
    stage = rep(0, 6),
    trend = survival_trend,
    annual_sd = survival_annual_sd,
    period_sd = survival_month_sd,
    annual_period_sd = survival_annual_month_sd,
    nyear = nyear)
  
  adultf <- rbinom(1, adults, proportion_adult_female)
  adultm <- adults - adultf
  yearlingf <- rbinom(1, adults, 0.5)
  yearlingm <- adults - yearlingf
  calff <- rbinom(1, adults, 0.5)
  calfm <- adults - calff
  
  # figure out initial pop sizes from female_adults
  bb_sims_base(nyear = nyear,
                    month_composition = month_composition,
                    female_calves = calff,
                    male_calves = calfm,
                    female_yearlings = yearlingf,
                    male_yearlings = yearlingm,
                    female_adults = adultf,
                    male_adults = adultm,  
                    survival_rates = phi,
                    fecundity_rates = fec,
                    # collared_female_yearlings_month_year = matrix(0, 12, nyear),
                    collared_adult_females = collared_adult_females,
                    probability_uncertain_mortality_month_year = matrix(probability_uncertain_mortality, 12, nyear),
                    probability_uncertain_survivor_month_year = matrix(probability_uncertain_survivor, 12, nyear), # used?
                    group_size_lambda_year = rep(group_size, nyear),
                    group_size_theta_year = rep(0, nyear),
                    group_max_proportion = group_max_proportion,
                    group_min_size = group_min_size,
                    groups_coverage_year = rep(groups_coverage, nyear),
                    # proportion_adult_female = rep(proportion_adult_female, nyear),
                    probability_unsexed_adult_female_year = rep(probability_unsexed_adult_female, nyear),
                    probability_unsexed_adult_male_year = rep(probability_unsexed_adult_male, nyear))
}
