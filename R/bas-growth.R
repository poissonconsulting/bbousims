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
  chk_whole_number(nperiods)
  
  population <- matrix(NA, ncol = nperiods+1, nrow = length(population_init))
  population[,1] <- population_init
  
  for(period in 1:nperiods){
    population[, period+1] <- birth %*% age %*% survival %*% population[,period] 
  }
  population
}

#' BAS model population growth period
#' 
#' Model population growth by period given survival, ageing, and birth subprocesses in that order. 
#' Survival rates vary by year, month and state; fecundity rates vary by year and state; ageing varies by year (i.e., according to female proportion). 
#' The first dimension of the birth, age and survival arrays must be identical to the length of the initial population vector. 
#' 
#' @param population_init A vector of the initial population for each state. 
#' @param birth An array of the birth matrices (from [birth_year()]). 
#' @param age An array of the age matrices (from [age_year()]). 
#' @param survival An array of the survival matrices (from [survival_year_month()]). 
#'
#' @return An array of the projected population with dimensions state, month and year. Returned object does not include initial population.   
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
bas_population_growth_period <- function(population_init, survival, age, birth){
  dims <- dim(survival)
  nyear <- dims[3]
  nstate <- length(population_init)
  nperiod <- nyear*12
  population <- matrix(NA, ncol = nperiod+1, nrow = nstate)
  population[,1] <- population0
  
  for(year in 1:nyear){
    for(month in 1:12){
      period <- yearmon_period(year, month)
      if(month == 12){
        population[, period+1] <- birth[,,year] %*% age[,,year] %*% survival[,,year,month] %*% population[,period]
      } else {
        population[, period+1] <- survival[,,year,month] %*% population[,period]
      }
    }
  }
  pop <- c(population)
  # remove initial
  projection <- pop[(nstate+1):length(pop)]
  print("hi")
  array(projection, dim = c(nstate, 12, nyear))
}