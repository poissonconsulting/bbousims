#' Create a survival process matrix.
#' 
#' @param survival A vector of the survival rates in each stage.
#'
#' @return A matrix of the survival subprocess.
#' @export
#'
#' @examples
#' bbs_matrix_survival(c(0.5, 0.83, 0.84)) %*% rep(100, 5)
bbs_matrix_survival <- function(survival){
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
bbs_matrix_survival_period <- function(survival){
  dims <- dim(survival)
  nperiod <- dims[1]
  nyear <- dims[2]
  nstate <- dims[3]
  x <- array(0, dim = c(nstate, nstate, nyear, nperiod))
  for(year in 1:nyear){
    for(period in 1:nperiod){
      x[,,year,period] <- bbs_matrix_survival(survival[period, year, ])
    }
  }
  x
}

#' Get stochastic Boreal Caribou survival rates by month, year and stage.
#' 
#' Stages include female calves, female yearlings and female adults. 
#' Calf and adult female survival can vary by trend, annual random effect, month random effect and month within annual random effect. 
#' Yearling survival is created as an effect on adult female survival rates.
#' Variation from random effects is stochastic. 
#' Year is scaled to Year - 1 for trend; therefore the intercept is the rate in the first year. 
#' 
#' @inheritParams params
#' @param survival_adult_female A number between 0 and 1 of the annual adult female survival. 
#' @param survival_calf A number between 0 and 1 of the annual calf survival. 
#' @param yearling_effect A number of the effect of yearling on the log-odds adult female monthly survival. 
#' @param trend_adult_female A number of the effect of an increase of one year on the log-odds adult female monthly survival.
#' @param annual_sd_adult_female A number of the standard deviation of the annual variation in adult female survival on the log-odds monthly survival.
#' @param month_sd_adult_female A number of the standard deviation of the monthly variation in adult female survival on the log-odds monthly survival.
#' @param annual_month_sd_adult_female A number of the standard deviation of the monthly variation within year variation in adult female survival on the log-odds monthly survival.
#' @param trend_calf_female A number of the effect of an increase of one year on the log-odds calf female monthly survival.
#' @param annual_sd_calf_female A number of the standard deviation of the annual variation in calf female survival on the log-odds monthly survival.
#' @param month_sd_calf_female A number of the standard deviation of the monthly variation in calf female survival on the log-odds monthly survival.
#' @param annual_month_sd_calf_female A number of the standard deviation of the monthly variation within year variation in calf female survival on the log-odds monthly survival.
#'
#' @return An array of survival rates with dimensions month, year, stage.
#' @export
#'
#' @examples
#' survival <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
#' 
bbs_survival_caribou <- function(survival_adult_female, 
                            survival_calf_female, 
                            nyear = 10, 
                            trend_adult_female = 0, 
                            annual_sd_adult_female = 0,
                            month_sd_adult_female = 0, 
                            annual_month_sd_adult_female = 0, 
                            trend_calf_female = 0, 
                            annual_sd_calf_female = 0,
                            month_sd_calf_female = 0, 
                            annual_month_sd_calf_female = 0, 
                            yearling_effect = 0){
 
  # convert annual to monthly on logit scale
  survival_adult_female <- logit(survival_adult_female^(1/12))
  survival_calf_female <- logit(survival_calf_female^(1/12))
  
  intercept <- c(survival_calf_female, 0, survival_adult_female)
  trend <- c(trend_calf_female, 0, trend_adult_female, trend_adult_female)
  annual_sd <- c(annual_sd_calf_female, 0, annual_sd_adult_female)
  period_sd <- c(month_sd_calf_female, 0, month_sd_adult_female)
  annual_period_sd <- c(annual_month_sd_calf_female, 0, annual_month_sd_adult_female)
  
  x <- bbs_survival(intercept = intercept,
               nyear = nyear,
               trend = trend, 
               annual_sd = annual_sd, 
               period_sd = period_sd,
               annual_period_sd = annual_period_sd,
               nperiod_within_year = 12)
  
  # add yearling effect
  x[,,2] <- ilogit(logit(x[,,3]) + yearling_effect)
  x
}

#' Get stochastic survival rates by period, year and stage.
#' 
#' Each stage can vary by intercept, year trend, annual random effect, period random effect and period within annual random effect. 
#' All values are provided on the log-odds scale. 
#' Variation from random effects is stochastic. 
#' Year is scaled to Year - 1 for trend; therefore the intercept is the rate in the first year. 
#' 
#' @return An array of survival rates with dimensions period, year, stage.
#' @export
#'
#' @examples
#' survival <- bbs_survival(intercept = logit(c(0.94, 0.98, 0.98)), trend = c(0, 0, 0.2))
#' 
bbs_survival <- function(intercept, 
                         nyear = 10,
                         trend = c(0, 0, 0), 
                         annual_sd = c(0, 0, 0),
                         period_sd = c(0, 0, 0),
                         annual_period_sd = c(0, 0, 0),
                         nperiod_within_year = 12){
  
  nstage <- length(intercept)
  nperiod <- nperiod_within_year
  
  esurvival <- array(0, dim = c(nperiod, nyear, nstage))
  # eaf <- array(0, dim = c(12, nyear))
  bannual <- array(0, dim = c(nyear, nstage))
  bperiod <- array(0, dim = c(nperiod, nstage))
  bannual_period <- array(0, dim = c(nyear, nperiod, nstage))
  # intercept is first year
  year <- 1:nyear - 1
  
  for(stg in 1:nstage){
    for(yr in 1:nyear){
      bannual[yr, stg] <- rnorm(1, 0, annual_sd[stg])
      for(prd in 1:nperiod){
        bannual_period[yr, prd, stg] <- rnorm(1, 0, annual_period_sd[stg])
      }
    }
  }
  
  for(stg in 1:nstage){
    for(prd in 1:nperiod){
      bperiod[prd, stg] <- rnorm(1, 0, period_sd[stg])
    }
  }
  
  for(yr in 1:nyear){
    for(prd in 1:nperiod){
      for(stg in 1:nstage){
        esurvival[prd, yr, stg] <- ilogit(intercept[stg] + trend[stg] * year[yr] + bannual[yr, stg] + bperiod[prd, stg] + bannual_period[yr, prd, stg])
      }
    }
  }
  esurvival
}
