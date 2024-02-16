#' Parameter Descriptions for bboutools Functions
#'
#' @param ... Unused parameters.
#' @param population_init A vector of the initial population for each stage. 
#' @param birth A matrix of the birth process matrix (output of [matrix_birth()]). 
#' @param age A matrix of the age process matrix (output of [matrix_age()]). 
#' @param survival A matrix of the survival process matrix (output of [matrix_survival()]). 
#' @param nyear A whole number of the number of years. 
#' @param nsims A whole number between 1 and 1,000,000 specifying the number of simulated data sets. 
#' @param female_recruit_stage A positive whole number of the stage representing female recruits.
#' @param male_recruit_stage A positive whole number of the stage representing male recruits.
#' @param female_proportion A number between 0 and 1 indicating the proportion of recruits that are female.
#' the number of data sets to simulate.
#'
#' @keywords internal
#' @name params
NULL