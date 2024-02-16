# these functions are deterministic! see simulate.R for stochastic equivalents

#' Project population from BAS model
#' 
#' Project population growth given initial population in each stage and survival, ageing, and birth subprocesses, in that order. 
#' The number of columns and rows of the birth, age and survival matrices must be identical to the length of the initial population vector. 
#' 
#' @inheritParams params
#' @param birth A matrix of the birth process matrix (from [matrix_birth()]). 
#' @param age A matrix of the age process matrix (from [matrix_age()]). 
#' @param survival A matrix of the survival process matrix (from [matrix_survival()]). 
#' @param nperiod A whole number of the number of periods to apply population growth. 
#'
#' @return A matrix of the projected population with dimensions stage and `nperiod + 1` to include the initial population. 
#' @export
#'
#' @examples
#' if (interactive()) {
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
#' Project population growth by period given initial population and survival, ageing, and birth subprocesses in that order. 
#' Survival rates vary by year, period and stage; fecundity rates vary by year and stage; ageing process does not vary with time. 
#' 
#' @inheritParams params
#' @param birth An array of the birth matrices (output of [matrix_birth_year()]). 
#' @param age An age process matrix (output of [matrix_age]). 
#' @param survival An array of the survival matrices (output of [matrix_survival_period()]). 
#'
#' @return An array of the projected population with dimensions stage, period and year. Returned object does not include initial population.   
#' @export
#'
#' @examples
#' if (interactive()) {
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
  population[,1] <- population_init
  
  for(year in 1:nyear){
    for(period in 1:nperiod){
      period_now <- year_period(year, period, nperiod)
      if(period == nperiod){
        population[, period_now+1] <- birth[,,year] %*% age %*% survival[,,period,year] %*% population[,period_now]
      } else {
        population[, period_now+1] <- survival[,,period,year] %*% population[,period_now]
      }
    }
  }
  population
  # pop <- c(population)
  # # remove initial
  # projection <- pop[(nstate+1):length(pop)]
  # array(projection, dim = c(nstate, nperiod, nyear))
}