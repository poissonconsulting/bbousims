rbinom_map <- function(size, prob) {
  purrr::map_dbl(size, ~ {
    rbinom(1, size = round(.x / prob), prob = 1 - prob)
  })
}

add_male_population <- function(population,
                                proportion_adult_female,
                                proportion_yearling_female,
                                stochastic) {
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

#' Simulate population from survival, ageing and birth
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
#' @examples
#' if (interactive()) {
#'   pop0 <- c(100, 200)
#'   survival <- bbs_survival(intercept = logit(c(0.95, 0.98)))
#'   fecundity <- bbs_fecundity(intercept = c(NA, logit(0.4)))
#'   survival_mat <- bbs_matrix_survival_period(survival$eSurvival)
#'   birth_mat <- bbs_matrix_birth_year(fecundity$eFecundity)
#'   age_mat <- bbs_matrix_age(c(2, 2))
#'   x <- bbs_population(pop0,
#'     birth = birth_mat,
#'     age = age_mat,
#'     survival = survival_mat
#'   )
#' }
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

#' Simulate Boreal Caribou population from survival, ageing and birth.
#'
#' Simulate population projection for Boreal Caribou from key survival and fecundity rates.
#'
#' This model assumes that survival occurs at the end of each period and survival, ageing and birth occur at the end of each year, in that order.
#' Initial population size for all stages is determined from the initial number of adult females and the calculated stable stage distribution.
#' Survival and fecundity arguments accept outputs of [bbs_survival_caribou()] and [bbs_fecundity_caribou()].
#' These are converted into processes matrices using [bbs_matrix_survival_period()] and [bbs_matrix_birth_year()] prior to projection.
#' [bbs_population()] is called internally to project population.
#' Yearling female survival is assumed to be the same as adult female survival.
#' Stages 1-6 in output matrix correspond to female calf, male calf, female yearling, male yearling, female adult and male adult, respectively.
#'
#' @inheritParams params
#'
#' @return A matrix of the population by stage and period.
#' @export
#' @examples
#' if (interactive()) {
#'   survival <- bbs_survival_caribou(0.84)
#'   fecundity <- bbs_fecundity_caribou(0.7)
#'   x <- bbs_population_caribou(survival, fecundity = fecundity)
#' }
bbs_population_caribou <- function(survival,
                                   fecundity,
                                   adult_females = 1000,
                                   proportion_adult_female = 0.65,
                                   proportion_yearling_female = 0.5,
                                   stochastic = TRUE) {
  .chk_survival(survival)
  .chk_fecundity(fecundity)
  chk_whole_number(adult_females)
  chk_gt(adult_females)
  chk_number(proportion_adult_female)
  chk_range(proportion_adult_female)
  chk_number(proportion_yearling_female)
  chk_range(proportion_yearling_female)
  chk_flag(stochastic)

  stable_stage_dist <- stable_stage_distribution(
    proportion_female = proportion_yearling_female,
    calves_per_adult_female = fecundity$b0[3],
    survival_calf = survival$b0[1],
    survival_yearling = survival$b0[3],
    survival_adult_female = survival$b0[3]
  )

  pop0 <- initial_population(
    adult_females = adult_females,
    stable_stage_dist = stable_stage_dist
  )

  survival_mat <- bbs_matrix_survival_period(survival$eSurvival)
  birth_mat <- bbs_matrix_birth_year(fecundity$eFecundity)
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
    stochastic = stochastic
  )

  x <- class_bbou_population_caribou(x)
  x
}
