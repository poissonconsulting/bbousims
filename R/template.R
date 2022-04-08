#' Template for bbousims Functions
#' 
#' @param nyear A positive count number of the number of years.
#' @param month_calving A count between 1 and 12 of the month that females calf.
#' @param female_fawns A count of the number of female fawns at the start of month 1 in year 1.
#' @param male_fawns A count of the number of male fawns at the start of month 1 in year 1.
#' @param female_yearlings A count of the number of female yearlings at the start of month 1 in year 1.
#' @param male_yearlings A count of the number of male yearlings at the start of month 1 in year 1.
#' @param female_adults A count of the number of female adults at the start of month 1 in year 1.
#' @param male_adults A count of the number of male adults at the start of month 1 in year 1.
#' @param survival_female_fawns_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female fawn.
#' @param survival_male_fawns_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male fawn.
#' @param survival_female_yearlings_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female yearling.
#' @param survival_male_yearlings_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male yearling.
#' @param survival_female_adults_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female adult.
#' @param survival_male_adults_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male adult.
#' @param fawns_per_female_yearling_year A vector of length nyear of the expected number of fawns per female yearling.
#' @param fawns_per_female_adult_year A vector of length nyear of the expected number of fawns per female adult.
#' @param collared_female_yearlings_month_year A matrix of dimensions 12 x nyear of the number of collared female yearlings at the start of the month.
#' @param collared_female_adults_month_year A matrix of dimensions 12 x nyear of the number of collared female adults at the start of the month.
#' @param probability_uncertain_mortality_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a mortality being uncertain.
#' @param probability_uncertain_survivor_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a survivor being uncertain.
#' @param month_census_year A vector of length nyear of the census month for each year.
#' @param group_size_year A vector of length nyear of the expected group size.
#' @param group_phi_year A vector of length nyear of the standard deviation of the extra-Poisson variation in the group size.
#' @param census_groups_year A vector of length nyear of the number of groups in the census.
#' @param probability_unsexed_adult_female_year A vector of length nyear of the probability of an adult female being unsexed.
#' @param probability_unsexed_adult_male_year A vector of length nyear of the probability of an adult male being unsexed.
#' @param mixing_adult_male_year A vector of length nyear of the extent of mixing of adult males with adult females.
#' @param nyear A positive count number of the number of years.
#' @param month_calving A count between 1 and 12 of the month that females calf.
#' @param female_fawns A count of the number of female fawns at the start of month 1 in year 1.
#' @param male_fawns A count of the number of male fawns at the start of month 1 in year 1.
#' @param female_yearlings A count of the number of female yearlings at the start of month 1 in year 1.
#' @param male_yearlings A count of the number of male yearlings at the start of month 1 in year 1.
#' @param female_adults A count of the number of female adults at the start of month 1 in year 1.
#' @param male_adults A count of the number of male adults at the start of month 1 in year 1.
#' @param survival_female_fawns_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female fawn.
#' @param survival_male_fawns_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male fawn.
#' @param survival_female_yearlings_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female yearling.
#' @param survival_male_yearlings_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male yearling.
#' @param survival_female_adults_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female adult.
#' @param survival_male_adults_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male adult.
#' @param fawns_per_female_yearling_year A vector of length nyear of the expected number of fawns per female yearling.
#' @param fawns_per_female_adult_year A vector of length nyear of the expected number of fawns per female adult.
#' @param collared_female_yearlings_month_year A matrix of dimensions 12 x nyear of the number of collared female yearlings at the start of the month.
#' @param collared_female_adults_month_year A matrix of dimensions 12 x nyear of the number of collared female adults at the start of the month.
#' @param probability_uncertain_mortality_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a mortality being uncertain.
#' @param probability_uncertain_survivor_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a survivor being uncertain.
#' @param month_census_year A vector of length nyear of the census month for each year.
#' @param group_size_year A vector of length nyear of the expected group size.
#' @param group_phi_year A vector of length nyear of the standard deviation of the extra-Poisson variation in the group size.
#' @param census_groups_year A vector of length nyear of the number of groups in the census.
#' @param probability_unsexed_adult_female_year A vector of length nyear of the probability of an adult female being unsexed.
#' @param probability_unsexed_adult_male_year A vector of length nyear of the probability of an adult male being unsexed.
#' @param mixing_adult_male_year A vector of length nyear of the extent of mixing of adult males with adult females.
#' @keywords internal
#' @name template
NULL