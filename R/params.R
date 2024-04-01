#' Parameter Descriptions for bboutools Functions
#'
#' @param ... Unused parameters.
#' @param survival_adult_female A number between 0 and 1 of the annual female adult survival. 
#' @param survival_calf_female A number between 0 and 1 of the annual female calf survival. 
#' @param calves_per_adult_female A number of the calves per adult female. 
#' @param proportion_female A number between 0 and 1 indicating the proportion of recruits that are female.
#' @param nyear A whole number of the number of years. 
#' @param stochastic A flag indicating whether to 
#' @param female_recruit_stage A positive whole number of the stage representing female recruits.
#' @param male_recruit_stage A positive whole number of the stage representing male recruits. Ignored if NULL. 
#' @param stochastic A flag indicating whether to include demographic stochasticity.
#' 
#' @param population A population object (output of `simulate_population()`).
#' @param group_size_lambda A number of the lambda value of the gamma-poisson distribution to draw groups sizes from. 
#' @param group_size_theta A number of the theta value of the gamma-poisson distribution to draw groups sizes from. 
#' @param max_group_proportion A number between 0 and 1 of the maximum group size as proportion of the total population. 
#' @param min_group_size A whole positive number of the minimum group size.
#' @param recruit_stages A vector of whole numbers indicating the recruit stages (e.g., calf).
#' @param reproductive_female_stages A vector of whole numbers indicating the reproductive female stages (e.g., cows).
#'
#' @keywords internal
#' @name params
NULL