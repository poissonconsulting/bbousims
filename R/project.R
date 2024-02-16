# these functions are deterministic! see simulate.R for stochastic equivalents

#' Project population from BAS model
#' 
#' Project population growth given initial population in each state and survival, ageing, and birth subprocesses, in that order. 
#' The number of columns and rows of the birth, age and survival matrices must be identical to the length of the initial population vector. 
#' 
#' @param population_init A vector of the initial population for each state. 
#' @param birth A matrix of the birth process matrix (from [birth_matrix()]). 
#' @param age A matrix of the age process matrix (from [age_matrix()]). 
#' @param survival A matrix of the survival process matrix (from [survival_matrix()]). 
#' @param nperiod A whole number of the number of periods to apply population growth. 
#'
#' @return A matrix of the projected population with dimensions state and `nperiod + 1` to include the initial population. 
#' @export
#'
#' @examples
#' #' if (interactive()) {
#'   nstate <- 6
#'   population0 <- rep(100, nstate)
#'   survival <- matrix_survival(rep(0.87, nstate))
#'   age <- matrix_age(c(2, 3, 4, 5, 6, 5, 6))
#'   birth <- matrix_birth(c(0, 0, 0.2, 0, 0.2, 0))
#'   project_population_bas(population0, survival = survival, age = age, birth = birth, nperiod = 5)
#' }

project_population_bas <- function(population_init, birth, age, survival, nperiod = 1){
  chk_numeric(population_init)
  # TODO add chk_population_matrix for better chks (same row as cols)
  chk_matrix(survival)
  chk_matrix(age)
  chk_matrix(birth)
  chk_identical(length(population_init), ncol(survival))
  chk_identical(length(population_init), ncol(age))
  chk_identical(length(population_init), ncol(birth))
  chk_whole_number(nperiod)
  
  population <- matrix(NA, ncol = nperiod+1, nrow = length(population_init))
  population[,1] <- population_init
  
  for(period in 1:nperiod){
    population[, period+1] <- birth %*% age %*% survival %*% population[,period] 
  }
  population
}

#' Project population from BAS model by period
#' 
#' project population growth by period given initial population and survival, ageing, and birth subprocesses in that order. 
#' Survival rates vary by year, period and state; fecundity rates vary by year and state; ageing process does not vary with time. 
#' 
#' @param population_init A vector of the initial population for each state. 
#' @param birth An array of the birth matrices (from [birth_year()]). 
#' @param age An array of the age matrices (from [age_year()]). 
#' @param survival An array of the survival matrices (from [survival_year_month()]). 
#'
#' @return An array of the projected population with dimensions state, period and year. Returned object does not include initial population.   
#' @export
#'
#' @examples
#' #' if (interactive()) {
#'   nstate <- 5
#'   population0<- rep(100, nstate)
#'   x1 <- matrix(c(rep(0.98, 12), rep(0.97, 12)), nrow = 12)
#'   x2 <- matrix(c(rep(0.97, 12), rep(0.96, 12)), nrow = 12)
#'   x3 <- matrix(c(rep(0.95, 12), rep(0.94, 12)), nrow = 12)
#'   x4 <- matrix(c(rep(0.99, 12), rep(0.99, 12)), nrow = 12)
#'   x5 <- matrix(c(rep(0.98, 12), rep(0.96, 12)), nrow = 12)
#'   
#'   survival_rates <- array(c(x1, x2, x3, x4, x5), dim = c(12, 2, 5))
#'   survival <- survival_year_month(survival_rates)
#'   
#'   birth_rates <- matrix(c(0, 0.2, 0, 0.3, 0,
#'                           0, 0.3, 0, 0.35, 0), ncol = 5, byrow = TRUE)
#'   birth <- birth_year(birth_rates)
#'   
#'   age <- age_year(rep(0.5, 2), nstate = 5)
#'   
#'   bas_population_growth_period(population0, survival = survival, age = age, birth = birth)
#' }
project_population_bas_period <- function(population_init, birth, age, survival){
  dims <- dim(survival)
  nyear <- dims[4]
  nperiod <- dims[3]
  nstate <- length(population_init)
  periods <- 1:(nyear*nperiod)
  population <- matrix(NA, ncol = length(periods)+1, nrow = nstate)
  population[,1] <- population0
  
  for(year in 1:nyear){
    for(period in 1:nperiod){
      period_cont <- year_period(year, period, nperiod)
      if(period == nperiod){
        population[, period_cont+1] <- birth[,,year] %*% age %*% survival[,,period,year] %*% population[,period_cont]
      } else {
        population[, period_cont+1] <- survival[,,period,year] %*% population[,period_cont]
      }
    }
  }
  population
  # pop <- c(population)
  # # remove initial
  # projection <- pop[(nstate+1):length(pop)]
  # array(projection, dim = c(nstate, nperiod, nyear))
}