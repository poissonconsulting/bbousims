rbinom_map <- function(size, prob){
  purrr::map_dbl(size, ~ {
    rbinom(1, size = round(.x / prob),  prob = 1 - prob)
  })
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
#' @return A matrix of the population by stage and period. 
#'
simulate_population_base <- function(population_init, 
                                birth, 
                                age, 
                                survival,
                                proportion_adult_female,
                                proportion_yearling_female,
                                stochastic = TRUE){
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
  
  if(stochastic){
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
  } else {
    for(year in 1:nyear){
      for(period in 1:nperiod){
        period_now <- (year-1) * nperiod + period
        if(period == nperiod){
          abundance[,period_now+1] <- birth[,,year] %*% (age %*% (survival[,,year,period] %*% abundance[,period_now])) 
        } else {
          abundance[,period_now+1] <- survival[,,year,period] %*% abundance[,period_now]
        }
      }
    }
  }
  m <- matrix(NA, nrow = 6, ncol = ncol(abundance))
  m[1,] <- abundance[1,]
  m[3,] <- abundance[2,]
  m[5,] <- abundance[3,]
  
  if(stochastic){
    m[2,] <- rbinom_map(abundance[1,], proportion_yearling_female) 
    m[4,] <- rbinom_map(abundance[2,], proportion_yearling_female) 
    m[6,] <- rbinom_map(abundance[3,], proportion_adult_female) 
  } else {
    m[2,] <- abundance[1,] / proportion_yearling_female - abundance[1,]
    m[4,] <- abundance[2,] / proportion_yearling_female - abundance[2,]
    m[6,] <- abundance[3,] / proportion_adult_female - abundance[3,]
  }
  m
}

#' Simulate population with varying rates
#' 
#' Simulate population projections given initial population in each stage and survival, ageing, and birth process matrix.
#' Survival can vary by period (e.g., month) and year, birth can vary by year, and ageing is constant. 
#' This model assumes that survival occurs at the end of each period and survival, ageing and birth occur at the end of each year, in that order. 
#' The dimensions of birth, age and survival process matrices must be identical to the length of the initial population vector. 
#' 
#' @inheritParams params
#' @param stochastic A flag indicating whether to include demographic stochasticity.
#'
#' @return A nlist object with projected abundance at each stage, period and simulation.
#' @export
#'
bb_simulate_population <- function(nyear = 20,
                                  adult_females = 1000,
                                  proportion_adult_female = 0.65,
                                  proportion_yearling_female = 0.5,
                                  survival_adult_female = 0.84, # annual
                                  survival_calf = 0.5, # annual
                                  calves_per_adult_female = 0.5,
                                  survival_trend = 0.2,
                                  survival_annual_sd = 0,
                                  survival_month_sd = 0,
                                  survival_annual_month_sd = 0,
                                  calves_per_adult_female_trend = 0.1,
                                  calves_per_adult_female_annual_sd = 0,
                                  stochastic = TRUE){
  
  dem <- bb_demographic_summary(sex_ratio = proportion_yearling_female, 
                                calves_per_adult_female = calves_per_adult_female,
                                survival_adult_female = survival_adult_female,
                                survival_calf = survival_calf)
  
  pop0 <- initial_population(adult_females = adult_females,
                             stable_stage_dist = dem$stable_stage_dist)
  
  fec <- fecundity_year(
    calves_per_adult_female = calves_per_adult_female,
    trend = calves_per_adult_female_trend,
    annual_sd = calves_per_adult_female_annual_sd,
    nyear = nyear)
  
  # array survival rates, month x year x stage
  phi <- survival_period(
    survival_adult_female = survival_adult_female,
    survival_calf = survival_calf,
    trend = survival_trend,
    annual_sd = survival_annual_sd,
    period_sd = survival_month_sd,
    annual_period_sd = survival_annual_month_sd,
    nyear = nyear)
  
  survival_matrices <- matrix_survival_period(phi)
  birth_matrices <- matrix_birth_year(fec)
  age_matrix <- matrix_age()
  
  # survival then ageing then birth (BAS model)
  population <- simulate_population_base(pop0, 
                                    birth = birth_matrices, 
                                    survival = survival_matrices,
                                    age = age_matrix,
                                    proportion_adult_female = proportion_adult_female,
                                    proportion_yearling_female = proportion_yearling_female,
                                    stochastic = stochastic)
  population
}

#' Plot population
#' 
#' Plot simulated population (i.e., from output of `bb_simulate_population()`).
#' 
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#'
#' @return A nlist object with projected abundance at each stage, period and simulation.
#' @export
#'
bb_plot_population <- function(population, annual = TRUE){
  library(ggplot2)
  x <- as.data.frame(t(population))
  colnames(x) <- 1:6
  x$Period <- 1:nrow(x)
  x$Year <- period_to_year(x$Period)
  x <- 
    x %>%
    tidyr::pivot_longer(1:6, names_to = "Stage", values_to = "Abundance") %>%
    dplyr::mutate(Stage = dplyr::case_when(Stage == 1 ~ "Female Calf",
                                    Stage == 2 ~ "Male Calf",
                                    Stage == 3 ~ "Female Yearling",
                                    Stage == 4 ~ "Male Yearling",
                                    Stage == 5 ~ "Female Adult",
                                    Stage == 6 ~ "Male Adult"),
                  Stage = factor(Stage, levels = c("Female Calf",
                                                   "Male Calf",
                                                   "Female Yearling",
                                                   "Male Yearling",
                                                   "Female Adult",
                                                   "Male Adult")))
  if(annual){
    x <- 
      x %>%
      dplyr::arrange(Period) %>%
      dplyr::group_by(Year, Stage) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }
  ggplot2::ggplot(data = x) +
    ggplot2::geom_line(ggplot2::aes(x = Year, y = Abundance, color = Stage)) +
    poispalette::scale_color_disc_poisson()
}
