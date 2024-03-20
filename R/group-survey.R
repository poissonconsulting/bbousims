#' Assign population into groups for each composition survey
#' 
#' See `bb_population_groups()` for details on group assignment. 
#' Month composition is month of composition surveys relative to the start of the caribou year. 
#' Groups are sampled in each composition survey in proportion to groups_coverage_year. 
#' 
#' @inheritParams params
#'
#' @return A list of observed groups in each composition survey.
#'
population_groups_survey <- function(population, 
                                     month_composition,
                                     group_size_lambda_year, 
                                     group_size_theta_year,
                                     group_max_proportion,
                                     group_min_size,
                                     group_coverage_year){
  nstep <- ncol(population)
  nyear <- (nstep - 1)/12
  composition_ind <- month_composition + 1
  survey <- c(composition_ind, composition_ind + 12 * 1:nyear)
  survey <- survey[survey <= nstep]
  
  purrr::map(seq_along(survey), ~ {
    pop <- population[,survey[.x]]
    y <- population1_groups(population = pop,
                            group_size_lambda = group_size_lambda_year[.x],
                            group_size_theta = group_size_theta_year[.x],
                            group_max_proportion = group_max_proportion,
                            group_min_size = group_min_size)
    sample(y, round(group_coverage_year[.x] * length(y)))
  })
}