matrix_tbl <- function(x, values_to){
  x %>%
    dplyr::as_tibble(rownames = "month", .name_repair = NULL) %>%
    tidyr::pivot_longer(dplyr::contains("V"), names_to = "year", values_to = values_to) %>%
    dplyr::mutate(year = as.integer(gsub("V", "", year))) %>%
    dplyr::select(year, month, dplyr::everything())
}

abundance_tbl <- function(population, population_name){
  population %>%
    dplyr::as_tibble(.name_repair = NULL) %>%
    dplyr::mutate(Stage = c("Female Calf", "Male Calf", "Female Yearling", "Male Yearling", "Female Adult", "Male Adult"),
                  Sex = rep(c("Female", "Male"), 3)) %>%
    tidyr::pivot_longer(dplyr::contains("V"), names_to = "Period", values_to = "Abundance") %>%
    dplyr::mutate(Period = as.integer(gsub("V", "", Period)),
                  Year = period_to_year(Period),
                  Month = period_to_month(Period),
                  PopulationName = population_name) %>%
    dplyr::select(Year, Month, PopulationName, Sex, Stage, Abundance)
}

recruitment_tbl <- function(groups,
                            month_composition,
                            probability_unsexed_adult_male,
                            probability_unsexed_adult_female,
                            population_name){
  purrr::map_df(seq_along(groups), function(x) {
    group <- groups[[x]]
    prob_unsexed_female <- probability_unsexed_adult_female
    prob_unsexed_male <- probability_unsexed_adult_male
    purrr::map_df(seq_along(group), function(y){
      subgroup <- group[[y]]
      females <- sum(subgroup == 5)
      females_unknown <- rbinom(1, size = females, prob = prob_unsexed_female)
      females_known <- females - females_unknown
      males <- sum(subgroup == 6)
      males_unknown <- rbinom(1, size = males, prob = prob_unsexed_male)
      males_known <- males - males_unknown
      adults_unknown <- males_unknown + females_unknown

      dplyr::tibble(Year = x,
                    Month = month_composition,
                    PopulationName = population_name,
                    Day = 1,
                    Cows = females_known,
                    Bulls = males_known,
                    Yearlings = sum(subgroup %in% c(3, 4)),
                    Calves = sum(subgroup %in% c(1, 2)),
                    UnknownAdults = adults_unknown)
    })
  })
}