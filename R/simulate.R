message("no longer need simulate_population_constant?")
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
simulate_population <- function(population_init, 
                                birth, 
                                age, 
                                survival,
                                proportion_adult_female,
                                proportion_yearling_female){
  # chk_numeric(population_init)
  # # TODO add chk_population_matrix for better chks (same row as cols)
  # chk_matrix(survival)
  # chk_matrix(age)
  # chk_matrix(birth)
  # chk_identical(length(population_init), ncol(survival))
  # chk_identical(length(population_init), ncol(age))
  # chk_identical(length(population_init), ncol(birth))
  # chk_whole_number(nperiod)
  
  chk_whole_numeric(population_init)
  population_init <- as.integer(population_init)
  
  nstage <- length(population_init)
  nperiod <- dim(survival)[4]
  nyear <- dim(survival)[3]
  nstep <- nperiod*nyear + 1
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
  }
  abundance
  m <- matrix(NA, nrow = 6, ncol = ncol(abundance))
  m[1,] <- abundance[1,]
  m[2,] <- round(abundance[1,] / proportion_yearling_female - abundance[1,])
  m[3,] <- abundance[2,]
  m[4,] <- round(abundance[2,] / proportion_yearling_female - abundance[2,])
  m[5,] <- abundance[3,]
  m[6,] <- round(abundance[3,] / proportion_adult_female - abundance[3,])
  m
}

simulate_population2 <- function( nyear = 20,
                                  adult_females = 1000,
                                  proportion_adult_female = 0.65,
                                  proportion_yearling_female = 0.5,
                                  survival_adult_female = 0.985,
                                  survival_calf = 0.985^12,
                                  calves_per_adult_female = 0.9,
                                  survival_trend = 0.5,
                                  survival_annual_sd = 0,
                                  survival_month_sd = 0,
                                  survival_annual_month_sd = 0,
                                  calves_per_adult_female_trend = 0,
                                  calves_per_adult_female_annual_sd = 0){
  
  survival_adult_female_year <- survival_adult_female^12
  survival_yearling_female_year <- survival_adult_female_year
  female_calves <- proportion_yearling_female * survival_adult_female_year * calves_per_adult_female
  survival_calf_year <- survival_calf^12
  
  A <- matrix(c(0,  0,  female_calves,
                survival_calf_year, 0,  0,
                0,  survival_yearling_female_year, survival_adult_female_year), 
              nrow = 3, byrow = TRUE)
  
  age_dist <- popbio::eigen.analysis(A)$stable.stage
  
  n_af <- adult_females
  n <- n_af / age_dist[3]
  n_yf <- n * age_dist[2]
  n_cf <- n * age_dist[1]
  n_am <- n_af/proportion_adult_female - n_af
  n_ym <- n_yf/proportion_yearling_female - n_yf
  # assuming proportion yearling female same as proportion calf female
  n_cm <- n_cf/proportion_yearling_female - n_cf
  
  pop0 <- round(c(n_cf, n_cm, 
                  n_yf, n_ym, 
                  n_af, n_am))
  
  # no yearling calves
  # matrix fecundity rates year x stage
  fec <- fecundity_year(
    intercept = log(calves_per_adult_female),
    stage = c(NA, NA, NA, NA, 0, NA),
    trend = calves_per_adult_female_trend,
    annual_sd = calves_per_adult_female_annual_sd,
    nyear = nyear)
  
  # array survival rates, month x year x stage
  phi <- survival_period(
    intercept = logit(survival_adult_female),
    stage = rep(0, 6),
    trend = survival_trend,
    annual_sd = survival_annual_sd,
    period_sd = survival_month_sd,
    annual_period_sd = survival_annual_month_sd,
    nyear = nyear)
  
  survival_matrices <- matrix_survival_period(phi)
  birth_matrices <- matrix_birth_year(fec)
  age_matrix <- matrix_age()
  
  # survival then ageing then birth (BAS model)
  population <- simulate_population(pop0, 
                                    birth = birth_matrices, 
                                    survival = survival_matrices,
                                    age = age_matrix)
  population
}

plot_population <- function(population){
  library(ggplot2)
  x <- as.data.frame(t(population))
  colnames(x) <- 1:6
  x$Period <- 1:nrow(x)
  x <- tidyr::pivot_longer(x, 1:6, names_to = "Stage", values_to = "Abundance")
  ggplot(data = x) +
    geom_line(aes(x = Period, y = Abundance, color = Stage)) 
}