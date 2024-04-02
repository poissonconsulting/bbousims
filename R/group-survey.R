#' Assign population into groups for each composition survey
#' 
#' See [bbs_population_groups()] for details on group assignment. 
#' Groups are sampled in each composition survey in proportion to `group_coverage`. 
#' 
#' @inheritParams params
#' @param month_composition A whole number between 1 and 12 of the month that composition surveys take place, relative to the start of the caribou year.  
#' @param group_coverage A number between 0 and 1 of the proportion of groups sampled. 
#' 
#' @export
#' @return A list of observed groups in each composition survey.
#'
bbs_population_groups_survey <- function(population, 
                                     month_composition = 9,
                                     group_size_lambda = 5,
                                     group_size_theta = 2,
                                     group_max_proportion = 1/4,
                                     group_min_size = 2,
                                     group_coverage = 0.2){
  chk_matrix(population)
  chk_whole_numeric(population)
  chk_whole_number(month_composition)
  chk_range(month_composition, c(1, 12))
  chk_number(group_size_lambda)
  chk_gt(group_size_lambda)
  chk_number(group_size_theta)
  chk_gt(group_size_theta)
  chk_number(group_max_proportion)
  chk_range(group_max_proportion)
  chk_whole_number(group_min_size)
  chk_gte(group_min_size)
  chk_number(group_coverage)
  chk_range(group_coverage)
  
  nstep <- ncol(population)
  nyear <- (nstep - 1)/12
  composition_ind <- month_composition + 1
  survey <- c(composition_ind, composition_ind + 12 * 1:nyear)
  survey <- survey[survey <= nstep]
  
  purrr::map(seq_along(survey), ~ {
    pop <- population[,survey[.x]]
    y <- population1_groups(population = pop,
                            group_size_lambda = group_size_lambda,
                            group_size_theta = group_size_theta,
                            group_max_proportion = group_max_proportion,
                            group_min_size = group_min_size)
    sample(y, round(group_coverage * length(y)))
  })
}