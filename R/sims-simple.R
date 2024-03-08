matrix_tbl <- function(x, values_to){
  x %>%
    dplyr::as_tibble(rownames = "month", .name_repair = NULL) %>%
    tidyr::pivot_longer(dplyr::contains("V"), names_to = "year", values_to = values_to) %>%
    dplyr::mutate(year = as.integer(gsub("V", "", year))) %>%
    dplyr::select(year, month, dplyr::everything())
}

abundance_tbl <- function(population){
  population %>%
    dplyr::as_tibble(.name_repair = NULL) %>%
    dplyr::mutate(stage = c("calf", "calf", "yearling", "yearling", "adult", "adult"),
                  sex = rep(c("female", "male"), 3)) %>%
    tidyr::pivot_longer(dplyr::contains("V"), names_to = "period", values_to = "abundance") %>%
    dplyr::mutate(period = as.integer(gsub("V", "", period)),
                  year = period_to_year(period),
                  month = period_to_month(period)) %>%
    dplyr::select(year, month, sex, stage, abundance)
}

recruitment_tbl <- function(groups, 
                            month_composition,
                            probability_unsexed_adult_male_year, 
                            probability_unsexed_adult_female_year){
  purrr::map_df(seq_along(groups), function(x) {
    group <- groups[[x]]
    prob_unsexed_female <- probability_unsexed_adult_female_year[x]
    prob_unsexed_male <- probability_unsexed_adult_male_year[x]
    purrr::map_df(seq_along(group), function(y){
      subgroup <- group[[y]]
      females <- sum(subgroup == 5)
      females_unknown <- rbinom(1, size = females, prob = prob_unsexed_female)
      females_known <- females - females_unknown
      males <- sum(subgroup == 6)
      males_unknown <- rbinom(1, size = males, prob = prob_unsexed_male)
      males_known <- males - males_unknown
      adults_unknown <- males_unknown + females_unknown
      
      dplyr::tibble(Year = x, Month = month_composition,
                    # total = length(subgroup),
                    PopulationName = "A",
                    Day = 1,
                    Cows = females_known,
                    Bulls = males_known,
                    Yearlings = sum(subgroup %in% c(3, 4)),
                    Calves = sum(subgroup %in% c(1, 2)),
                    UnknownAdults = adults_unknown)
    })
  })
}

simulate_survival <- function(collared_adult_females,
                         survival_female_adults_month_year,
                         probability_uncertain_mortality_month_year){
  
  starttotal <- collared_adult_females
  yearmon <- tidyr::expand_grid(year = 1:ncol(survival_female_adults_month_year), month = 1:12)
  purrr::map_df(1:nrow(yearmon), ~ {
    month <- yearmon$month[.x]
    year <- yearmon$year[.x]
    phi <- survival_female_adults_month_year[month, year]
    prob_uncertain <- probability_uncertain_mortality_month_year[month, year]
    last <- starttotal[length(starttotal)]
    dead <- rbinom(1, last, (1 - phi))
    dead_uncertain <- rbinom(1, dead, prob_uncertain)
    dead_certain <- dead - dead_uncertain
    update <- last - dead_certain
    starttotal <<- c(starttotal, update)
    tibble::tibble(PopulationName = "A",
                   Year = year, 
                   Month = month,
                   StartTotal = last,
                   MortalitiesCertain = dead_certain,
                   MortalitiesUncertain = dead_uncertain)
  })
}

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
#' @param female_yearlings The number of female yearlings at the start of the first (calving) month in the first year.
#' @param male_yearlings The number of male yearlings at the start of the first (calving) month in the first year.
#' @param female_adults The number of female adults at the start of the first (calving) month in the first year.
#' @param male_adults The number of male adults at the start of the first (calving) month in the first year.
#' @param survival_calves_year A vector of length nyear of the calf survival from the composition survey to calving.
#' @param survival_female_yearlings_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female yearling.
#' @param survival_male_yearlings_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male yearling.
#' @param survival_female_adults_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a female adult.
#' @param survival_male_adults_month_year A matrix of dimensions 12 x nyear of the monthly survival probability for a male adult.
#' @param calves_per_female_yearling_year A vector of length nyear of the expected number of calves per female yearling during the composition survey.
#' @param calves_per_female_adult_year A vector of length nyear of the expected number of calves per female adult during the composition survey.
#' @param collared_female_yearlings_month_year A matrix of dimensions 12 x nyear of the number of collared female yearlings at the start of the month.
#' @param collared_female_adults_month_year A matrix of dimensions 12 x nyear of the number of collared female adults at the start of the month.
#' @param probability_uncertain_mortality_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a mortality being uncertain.
#' @param probability_uncertain_survivor_month_year A matrix of dimensions 12 x nyear of the monthly probability of the fate of a survivor being uncertain.
#' @param group_size_year A vector of length nyear of the expected group size.
#' @param group_phi_year A vector of length nyear of the standard deviation of the extra-Poisson variation in the group size.
#' @param groups_coverage_year A vector of length nyear of the proportion of the groups in the composition survey.
#' @param proportion_adult_male_year A vector of length nyear of the proportion of the adult males in mixed groups in the composition survey.
#' @param probability_unsexed_adult_female_year A vector of length nyear of the probability of an adult female being unsexed.
#' @param probability_unsexed_adult_male_year A vector of length nyear of the probability of an adult male being unsexed.

#' @return A list of three tibbles.
#' The first named survival has columns year, month, starttotal, mortalitiescertain and mortalitiesuncertain.
#' The second named recruitment has columns year, month, cows, bulls, unknownadults, yearlings and calves.
#' And the final named abundance has columns year, month, sex, stage, abundance.
#' @export
#'
#' @examples
#' bb_sims_base()
bb_sims_base <- function(
    nyear = 5,
    month_composition = 9,
    female_calves = NULL,
    male_calves = female_calves,
    female_yearlings = NULL, # if null simulates values based female adults at equilibrium
    male_yearlings = female_yearlings,
    female_adults = 1000,
    male_adults = NULL,
    survival_rates = NULL,  # an array of survival rates with dimensions period, year and stage 
    fecundity_rates = NULL, # a matrix of the fecundity rates with dimensions year and stage
    # collared_female_yearlings_month_year = matrix(5, 12, nyear),
    collared_adult_females = 30,
    probability_uncertain_mortality_month_year = matrix(0.1, 12, nyear),
    probability_uncertain_survivor_month_year = matrix(0, 12, nyear), # used?
    group_size_lambda_year = rep(10, nyear),
    group_size_theta_year = rep(2, nyear),
    group_max_proportion = 0.25,
    group_min_size = 3,
    groups_coverage_year = rep(0.2, nyear),
    proportion_adult_male_year = rep(0, nyear),
    probability_unsexed_adult_female_year = rep(0.1, nyear),
    probability_unsexed_adult_male_year = rep(0.1, nyear)) {
  
  # chk_whole_number(nyear)
  # chk_gt(nyear)
  # nyear <- dim(fecundity_rates)[1]
  chk_whole_number(month_composition)
  chk_range(month_composition, c(1, 12))
  # chk_numeric(proportion_adult_male_year)
  # chk_length(proportion_adult_male_year, nyear)
  # chk_range(proportion_adult_male_year)
  
  pop0 <- c(female_calves, male_calves,
            female_yearlings, male_yearlings, 
            female_adults, male_adults)
  
  survival_matrices <- matrix_survival_period(survival_rates)
  birth_matrices <- matrix_birth_year(fecundity_rates)
  age_matrix <- matrix_age()
  
  population <- simulate_population(pop0, 
                                    birth = birth_matrices, 
                                    survival = survival_matrices,
                                    age = age_matrix)
  
  groups <- population_groups_survey(population = population,
                                     month_composition = month_composition,
                                     group_size_lambda_year = group_size_lambda_year,
                                     group_size_theta_year = group_size_theta_year,
                                     group_max_proportion = group_max_proportion,
                                     group_min_size = group_min_size,
                                     groups_coverage_year = groups_coverage_year)
  
  abundance <- abundance_tbl(population)
  
  recruitment <- recruitment_tbl(groups, 
                                 month_composition = month_composition,
                                 probability_unsexed_adult_male_year = probability_unsexed_adult_male_year,
                                 probability_unsexed_adult_female_year = probability_unsexed_adult_female_year)
  
  survival <- simulate_survival(collared_adult_females,
                           survival_female_adults_month_year = survival_rates[,,5],
                           probability_uncertain_mortality_month_year = probability_uncertain_mortality_month_year)
  
  list(survival = survival,
       recruitment = recruitment,
       abundance = abundance)
  
}