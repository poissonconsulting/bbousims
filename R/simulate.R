#' Simulate population with constant rates
#' 
#' Simulate population projections given initial population in each stage and survival, ageing, and birth process matrix.
#' Survival, ageing and birth are assumed to be constant through time. 
#' This model assumes that survival, ageing and birth occur in that order at the end of each year. 
#' The dimensions of birth, age and survival process matrices must be identical to the length of the initial population vector. 
#' 
#' @inheritParams params
#' @param birth A matrix of the birth process matrix (output of [matrix_birth()]). 
#' @param age A matrix of the age process matrix (output of [matrix_age()]). 
#' @param survival A matrix of the survival process matrix (output of [matrix_survival()]). 
#'
#' @return A nlist object with projected abundance at each stage, year and simulation.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   nstage <- 6
#'   population0 <- rep(100, nstage)
#'   survival <- matrix_survival(rep(0.87, nstage))
#'   age <- matrix_age(c(2, 3, 4, 5, 6, 5, 6))
#'   birth <- matrix_birth(c(0, 0, 0.2, 0, 0.2, 0))
#'   simulate_population_constant(population0, survival = survival, age = age, 
#'     birth = birth, nyear = 5, nsims = 100)
#' }
simulate_population_constant <- function(population_init, birth, age, survival, nyear = 10L, nsims = 100L){
  chk_numeric(population_init)
  # TODO add chk_population_matrix for better chks (same row as cols)
  chk_matrix(survival)
  chk_matrix(age)
  chk_matrix(birth)
  chk_identical(length(population_init), ncol(survival))
  chk_identical(length(population_init), ncol(age))
  chk_identical(length(population_init), ncol(birth))

  code <- "
  abundance <- matrix(0, nrow = nstage, ncol = nstep)
  abundance[,1] <- population_init

  for(i in 2:nstep){
    abundance[,i] <- birth %*b% (age %*b% (survival %*b% abundance[,i-1])) 
  }"
  
  nstage <- length(population_init)
  # to include initial population in projection
  nstep <- nyear + 1
  consts = list(nstep = nstep, nstage = nstage, population_init = population_init)
  params <- list(survival = survival, age = age, birth = birth)
  population <- sims_simulate(code = code, constants = consts, 
                parameters = params, nsims = nsims, monitor = "abundance")
  population
}

#' Simulate population with varying rates
#' 
#' Simulate population projections given initial population in each stage and survival, ageing, and birth process matrix.
#' Survival can vary by period (e.g., month) and year, birth can vary by year, and ageing is constant. 
#' This model assumes that survival occurs at the end of each period and survival, ageing and birth occur at the end of each year, in that order. 
#' The dimensions of birth, age and survival process matrices must be identical to the length of the initial population vector. 
#' 
#' @inheritParams params
#' @param birth An array of the birth matrices (output of [matrix_birth_year()]). 
#' @param age An age process matrix (output of [matrix_age]). 
#' @param survival An array of the survival matrices (output of [matrix_survival_period()]). 
#'
#' @return A nlist object with projected abundance at each stage, period and simulation.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   nstage <- 6
#'   population0 <- rep(100, nstage)
#'   survival <- matrix_survival(rep(0.87, nstage))
#'   age <- matrix_age(c(2, 3, 4, 5, 6, 5, 6))
#'   birth <- matrix_birth(c(0, 0, 0.2, 0, 0.2, 0))
#'   simulate_population(population0, survival = survival, age = age, 
#'     birth = birth, nsims = 100)
#' }
simulate_population <- function(population_init, birth, age, survival, nsims = 100L){
  # chk_numeric(population_init)
  # # TODO add chk_population_matrix for better chks (same row as cols)
  # chk_matrix(survival)
  # chk_matrix(age)
  # chk_matrix(birth)
  # chk_identical(length(population_init), ncol(survival))
  # chk_identical(length(population_init), ncol(age))
  # chk_identical(length(population_init), ncol(birth))
  # chk_whole_number(nperiod)
  
  code <- "
  abundance <- matrix(0, nrow = nstage, ncol = nstep)
  abundance[,1] <- population_init
  
  for(year in 1:nyear){
    for(period in 1:nperiod){
      period_now <- (year-1) * nperiod + period
      if(period == nperiod){
        abundance[,period_now+1] <- birth[,,year] %*b% (age %*b% (survival[,,year,period] %*b% abundance[,period_now])) 
      } else {
        abundance[,period_now+1] <- survival[,,year,period] %*b% abundance[,period_now]
      }
    }
  }"
  
  nstage <- length(population_init)
  nperiod <- dim(survival)[4]
  nyear <- dim(survival)[3]
  nstep <- nperiod*nyear + 1
  consts = list(nperiod = nperiod, nyear = nyear, nstep = nstep, 
                nstage = nstage, population_init = population_init)
  params <- list(survival = survival, age = age, birth = birth)
  population <- sims_simulate(code = code, constants = consts, 
                              parameters = params, nsims = nsims, monitor = "abundance")
  population
}