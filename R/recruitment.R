abundance_tbl <- function(population, population_name = "A") {
  colnames(population) <- seq_len(ncol(population))
  stages <- c("Female Calf", "Male Calf", "Female Yearling", "Male Yearling", "Female Adult", "Male Adult")
  levels <- c("Female Adult", "Male Adult", "Female Yearling", "Male Yearling", "Female Calf", "Male Calf")
  nstep <- ncol(population) + 1
  population %>%
    as.data.frame() %>%
    mutate(
      Stage = stages,
      Stage = factor(.data$Stage, levels = levels)
    ) %>%
    pivot_longer(-all_of(nstep), names_to = "Period", values_to = "Abundance") %>%
    mutate(
      Period = as.integer(.data$Period) - 1,
      Year = period_to_year(.data$Period),
      Month = period_to_month(.data$Period),
      PopulationName = population_name
    ) %>%
    select("Year", "Month", "Period", "PopulationName", "Stage", "Abundance")
}

recruitment_tbl <- function(groups,
                            month_composition,
                            probability_unsexed_adult_male,
                            probability_unsexed_adult_female,
                            population_name) {
  purrr::map_df(seq_along(groups), function(x) {
    group <- groups[[x]]
    prob_unsexed_female <- probability_unsexed_adult_female
    prob_unsexed_male <- probability_unsexed_adult_male
    purrr::map_df(seq_along(group), function(y) {
      subgroup <- group[[y]]
      females <- sum(subgroup == 5)
      females_unknown <- rbinom(1, size = females, prob = prob_unsexed_female)
      females_known <- females - females_unknown
      males <- sum(subgroup == 6)
      males_unknown <- rbinom(1, size = males, prob = prob_unsexed_male)
      males_known <- males - males_unknown
      adults_unknown <- males_unknown + females_unknown

      tibble(
        Year = x,
        Month = month_composition,
        PopulationName = population_name,
        Day = 1,
        Cows = females_known,
        Bulls = males_known,
        Yearlings = sum(subgroup %in% c(3, 4)),
        Calves = sum(subgroup %in% c(1, 2)),
        UnknownAdults = adults_unknown
      )
    })
  })
}
