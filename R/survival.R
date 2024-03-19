#' Create a survival process matrix.
#' 
#' @param survival A vector of the survival rates in each stage.
#'
#' @return A matrix of the survival subprocess.
#' @export
#'
#' @examples
#' matrix_survival(c(0.87, 0.84, 0.84, 0.89, 0.9)) %*% rep(100, 5)
matrix_survival <- function(survival){
  chk_numeric(survival)
  chk_range(survival)
  
  x <- empty_matrix(length(survival))
  diag(x) <- survival
  x
}

#' Create survival matrix for each year and period.
#' 
#' @param survival An array of the survival rates with dimensions period, year and stage. Period represents any subdivision of a year (i.e., week, month, season).
#'
#' @return An array of survival process matrices with dimensions stage, stage, year, period.
#' @export
#'
#' @examples
#' if (interactive()) {
#'     survival_rates <- lapply(c(-0.01, 0.01, 0, -0.02, 0.01, 0.01), function(x){
#'       c(rep(0.98, 4), rep(0.97, 4), rep(0.96, 4)) + x
#'     })
#'     survival_rates <- array(unlist(survival_rates), dim = c(4, 3, 6))
#'     matrix_survival_period(survival_rates)
#' }
#' 
matrix_survival_period <- function(survival){
  dims <- dim(survival)
  nperiod <- dims[1]
  nyear <- dims[2]
  nstate <- dims[3]
  x <- array(0, dim = c(nstate, nstate, nyear, nperiod))
  for(year in 1:nyear){
    for(period in 1:nperiod){
      x[,,year,period] <- matrix_survival(survival[period, year, ])
    }
  }
  x
}

#' Get stochastic survival rates by period, year and stage.
#' 
#' Stages include female calves, female yearlings and female adults. 
#' The calf survival rates are fixed in time. 
#' Variation from random effects is stochastic. 
#' Trend is on centered year; therefore `survival_adult_female` is the rate in the mean year. 
#' 
#' @inheritParams params
#' @param survival_adult_female A number between 0 and 1 of the annual adult female survival. 
#' @param survival_calf A number between 0 and 1 of the annual calf survival. 
#' @param yearling_effect A number of the effect of yearling on the log-odds adult female periodic survival. 
#' @param trend A number of the effect of an increase of one year on the log-odds adult female periodic survival.
#' @param annual_sd A number of the standard deviation of the annual variation on the log-odds periodic survival.
#' @param period_sd A number of the standard deviation of the periodic (i.e., month, season) variation on the log-odds periodic survival.
#' @param annual_period_sd A number of the standard deviation of the periodic variation within year variation on the log-odds periodic survival.
#' @param nperiod_within_year A whole number of the number of periods within a year. 
#'
#' @return An array of survival rates with dimensions period, year, stage.
#' @export
#'
#' @examples
#' survival <- survival_period(4.5, stage = c(0, 0.1, -0.2), trend = 0.1,
#'    annual_sd = 0.3, period_sd = 0.2, annual_period_sd = 0.1, nyear = 5, 
#'    nperiod_within_year = 12)
#' 
survival_period <- function(survival_adult_female, 
                            survival_calf, 
                            nyear = 10, 
                            trend = 0, 
                            annual_sd = 0,
                            period_sd = 0, 
                            annual_period_sd = 0, 
                            yearling_effect = 0, 
                            nperiod_within_year = 12){
 
  nperiod <- nperiod_within_year
  nstage <- 3
  
  # convert annual to monthly on logit scale
  survival_adult_female <- logit(survival_adult_female^(1/nperiod_within_year))
  survival_calf <- logit(survival_calf^(1/nperiod_within_year))
  
  esurvival <- array(0, dim = c(nperiod, nyear, nstage))
  bannual <- vector(length = nyear)
  bperiod <- vector(length = nperiod)
  bannual_period <- matrix(0, nrow = nperiod, ncol = nyear)
  # intercept is now 
  year <- 1:nyear - 1
  stage <- c(0, yearling_effect, 0)
  
  for(yr in 1:nyear){
    bannual[yr] <- rnorm(1, 0, annual_sd)
    for(prd in 1:nperiod){
      bannual_period[prd,yr] <- rnorm(1, 0, annual_period_sd)
    }
  }
  for(prd in 1:nperiod){
    bperiod[prd] <- rnorm(1, 0, period_sd)
  }
  
  for(yr in 1:nyear){
    for(prd in 1:nperiod){
      for(stg in 1:nstage){
        esurvival[prd, yr, stg] <- ilogit(survival_adult_female + trend * year[yr] + stage[stg] + bannual[yr] + bperiod[prd] + bannual_period[prd, yr])
      }
    }
  }
  # set fixed calf
  esurvival[,,1] <- ilogit(survival_calf)
  esurvival
}
