rbinom_map <- function(size, prob) {
  purrr::map_dbl(size, ~ {
    rbinom(1, size = round(.x / prob),  prob = 1 - prob)
  })
}

add_male_population <- function(population, 
                                proportion_adult_female,
                                proportion_yearling_female,
                                stochastic){
  m <- matrix(NA, nrow = 6, ncol = ncol(population))
  m[1, ] <- population[1, ]
  m[3, ] <- population[2, ]
  m[5, ] <- population[3, ]
  
  if (stochastic) {
    m[2, ] <- rbinom_map(population[1, ], proportion_yearling_female)
    m[4, ] <- rbinom_map(population[2, ], proportion_yearling_female)
    m[6, ] <- rbinom_map(population[3, ], proportion_adult_female)
  } else {
    m[2, ] <- population[1, ] / proportion_yearling_female - population[1, ]
    m[4, ] <-
      population[2, ] / proportion_yearling_female - population[2, ]
    m[6, ] <-
      population[3, ] / proportion_adult_female - population[3, ]
  }
  m
}

#' Simulate population from BAS model
#'
#' Simulate population projections given initial population in each stage and survival, ageing, and birth process matrices.
#' Survival can vary by period (e.g., month) and year, birth can vary by year, and ageing is constant.
#' This model assumes that survival occurs at the end of each period and survival, ageing and birth occur at the end of each year, in that order.
#' The dimensions of birth, age and survival process matrices must be identical to the length of the initial population vector.
#'
#' @inheritParams params
#' @param population_init A vector of the initial population for each stage. 
#' @param birth An array of the birth matrices (output of [bbs_matrix_birth_year()]).
#' @param age An age process matrix (output of [bbs_matrix_age]).
#' @param survival An array of the survival matrices (output of [bbs_matrix_survival_period()]).
#'
#' @export
#' @return A matrix of the population by stage and period.
#'
bbs_population <- function(population_init,
                           birth,
                           age,
                           survival,
                           stochastic = TRUE) {
  chk_whole_numeric(population_init)
  chk_array(birth)
  chk_length(dim(birth), 3L)
  chk_matrix(age)
  chk_array(survival)
  chk_length(dim(survival), 4L)
  chk_identical(length(population_init), ncol(survival))
  chk_identical(length(population_init), ncol(age))
  chk_identical(length(population_init), ncol(birth))
  # same number of years
  chk_identical(dim(birth)[3], dim(survival)[3])
  chk_flag(stochastic)

  population_init <- as.integer(population_init)
  
  nstage <- length(population_init)
  nperiod <- dim(survival)[4]
  nyear <- dim(survival)[3]
  nstep <- nperiod * nyear + 1
  abundance <- matrix(0, nrow = nstage, ncol = nstep)
  abundance[, 1] <- population_init
  
  mmult <- `%*%`
  if (stochastic) {
    mmult <- `%*b%`
  }
  
  for (year in 1:nyear) {
    for (period in 1:nperiod) {
      period_now <- (year - 1) * nperiod + period
      if (period == nperiod) {
        abundance[, period_now + 1] <-
          mmult(birth[, , year], mmult(age, mmult(survival[, , year, period], abundance[, period_now])))
      } else {
        abundance[, period_now + 1] <-
          mmult(survival[, , year, period], abundance[, period_now])
      }
    }
  }
  abundance <- class_bbou_population(abundance)
  abundance
}

#' Simulate Boreal Caribou population from BAS model
#'
#' Simulate population projection for Boreal Caribou from key survival and recruitment rates.
#'
#' This model assumes that survival occurs at the end of each period and survival, ageing and birth occur at the end of each year, in that order.
#' Initial population is determined by calculating the stable age distribution (output of [bbs_demographic_summary()]).
#' Survival and fecundity rates are generated from [bbs_survival_caribou()] and [bbs_fecundity_caribou()]
#' and these are converted into processes matrices using [bbs_matrix_survival_period()] and [bbs_matrix_birth_year()].
#' [bbs_population()] is called internally to project population. 
#' Yearling female survival is assumed to be the same as adult female survival.
#'
#' @inheritParams params
#'
#' @return A matrix of the population by stage and period. 
#' @export
#'
bbs_population_caribou <- function(adult_females = 1000,
                                   nyear = 20,
                                   survival_adult_female = 0.85, # annual
                                   survival_calf_female = 0.5, # annual
                                   calves_per_adult_female = 0.7, # annual
                                   proportion_adult_female = 0.65,
                                   proportion_yearling_female = 0.5,
                                   survival_trend_adult_female = 0,
                                   survival_trend_calf_female = 0,
                                   survival_annual_sd_adult_female = 0,
                                   survival_annual_sd_calf_female = 0,
                                   survival_month_sd_adult_female = 0,
                                   survival_month_sd_calf_female = 0,
                                   survival_annual_month_sd_adult_female = 0,
                                   survival_annual_month_sd_calf_female = 0,
                                   calves_per_adult_female_trend = 0,
                                   calves_per_adult_female_annual_sd = 0,
                                   stochastic = TRUE) {
  chk_whole_number(adult_females)
  chk_gt(adult_females)
  chk_whole_number(nyear)
  chk_gt(nyear)
  chk_number(survival_adult_female)
  chk_range(survival_adult_female)
  chk_number(survival_calf_female)
  chk_range(survival_calf_female)
  chk_number(calves_per_adult_female)
  chk_gte(calves_per_adult_female)
  chk_number(proportion_adult_female)
  chk_range(proportion_adult_female)
  chk_number(proportion_yearling_female)
  chk_range(proportion_yearling_female)
  chk_number(survival_trend_adult_female)
  chk_number(survival_trend_calf_female)
  chk_number(survival_annual_sd_adult_female)
  chk_gte(survival_annual_sd_adult_female)
  chk_number(survival_annual_sd_calf_female)
  chk_gte(survival_annual_sd_calf_female)
  chk_number(survival_month_sd_adult_female)
  chk_gte(survival_month_sd_adult_female)
  chk_number(survival_month_sd_calf_female)
  chk_gte(survival_month_sd_calf_female)
  chk_number(survival_annual_month_sd_adult_female)
  chk_gte(survival_annual_month_sd_adult_female)
  chk_number(survival_annual_month_sd_calf_female)
  chk_gte(survival_annual_month_sd_calf_female)
  chk_number(calves_per_adult_female_trend)
  chk_number(calves_per_adult_female_annual_sd)
  chk_gte(calves_per_adult_female_annual_sd)
  chk_flag(stochastic)
  
  stable_stage_dist <- bbs_stable_stage_distribution(proportion_female = proportion_yearling_female, 
                                                     calves_per_adult_female = calves_per_adult_female,
                                                     survival_calf = survival_calf_female, 
                                                     survival_yearling = survival_adult_female, 
                                                     survival_adult_female = survival_adult_female)
  
  pop0 <- initial_population(adult_females = adult_females,
                             stable_stage_dist = stable_stage_dist)
  
  fec <- bbs_fecundity_caribou(
    calves_per_adult_female = calves_per_adult_female,
    trend = calves_per_adult_female_trend,
    annual_sd = calves_per_adult_female_annual_sd,
    nyear = nyear
  )
  
  # array survival rates, month x year x stage
  phi <- bbs_survival_caribou(
    survival_adult_female = survival_adult_female,
    survival_calf_female = survival_calf_female,
    trend_adult_female = survival_trend_adult_female,
    trend_calf_female = survival_trend_calf_female,
    annual_sd_adult_female = survival_annual_sd_adult_female,
    annual_sd_calf_female = survival_annual_sd_calf_female,
    month_sd_adult_female = survival_month_sd_adult_female,
    month_sd_calf_female = survival_month_sd_calf_female,
    annual_month_sd_adult_female = survival_annual_month_sd_adult_female,
    annual_month_sd_calf_female = survival_annual_month_sd_calf_female,
    nyear = nyear
  )
  
  survival_mat <- bbs_matrix_survival_period(phi)
  birth_mat <- bbs_matrix_birth_year(fec)
  age_mat <- bbs_matrix_age()
  
  population <- bbs_population(
    pop0,
    birth = birth_mat,
    survival = survival_mat,
    age = age_mat,
    stochastic = stochastic
  )
  
  x <- add_male_population(population, 
                      proportion_adult_female = proportion_adult_female,
                      proportion_yearling_female = proportion_yearling_female,
                      stochastic = stochastic)
  
  attr(x, "survival") <- phi
  attr(x, "fecundity") <- fec
  x <- class_bbou_population_caribou(x) 
  x
}
