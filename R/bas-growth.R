#' BAS model population growth
#' 
#' Model population growth given survival, ageing, and birth subprocesses in that order. 
#' The number of columns and rows of the birth, age and survival matrices must be identical to the length of the initial population vector. 
#' 
#' @param population_init A vector of the initial population for each state. 
#' @param birth A matrix of the birth matrix (from [birth_matrix()]). 
#' @param age A matrix of the age matrix (from [age_matrix()]). 
#' @param survival A matrix of the survival matrix (from [survival_matrix()]). 
#' @param nperiods A whole number of the number of periods to apply population growth. 
#'
#' @return A matrix of the population with one column and number rows corresponding to the length of the initial population vector. 
#' @export
#'
#' @examples
#' #' if (interactive()) {
#'   nstate <- 5
#'   population0<- rep(100, nstate)
#'   survival <- survival_matrix(rep(0.87, nstate))
#'   age <- age_matrix(0.5, 5)
#'   birth <- birth_matrix(c(0, 0.2, 0, 0.2, 0))
#'   bas_population_growth(population0, survival = survival, age = age, birth = birth)
#' }

bas_population_growth <- function(population_init, survival, age, birth, nperiods = 1){
  chk_numeric(population_init)
  # TODO add chk_population_matrix for better chks (same row as cols)
  chk_matrix(survival)
  chk_matrix(age)
  chk_matrix(birth)
  chk_identical(length(population_init), ncol(survival))
  chk_identical(length(population_init), ncol(age))
  chk_identical(length(population_init), ncol(birth))
  chk_whole_number(nperiod)
  
  population <- matrix(NA, ncol = nperiods+1, nrow = length(population_init))
  population[,1] <- population_init
  
  for(period in 1:nperiods){
    population[, period+1] <- birth %*% age %*% survival %*% population[,period] 
  }
  population
}
