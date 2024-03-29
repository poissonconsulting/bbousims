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

#' Get stochastic survival rates by period, year and stage.
#' 
#' Stages include female calves, female yearlings and female adults. 
#' Calf and adult female survival can vary by trend, annual random effect, month random effect and month within annual random effect. 
#' Yearling survival is generated as an effect on adult female survival. 
#' Variation from random effects is stochastic. 
#' Trend is on centered year; therefore `survival_adult_female` is the rate in the mean year. 
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
#' survival <- survival_period(4.5, stage = c(0, 0.1, -0.2), trend = 0.1,
#'    annual_sd = 0.3, period_sd = 0.2, annual_period_sd = 0.1, nyear = 5, 
#'    nperiod_within_year = 12)
#' 
bbs_survival <- function(survival_adult_female, 
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
 
  nstage <- 3
  
  # convert annual to monthly on logit scale
  survival_adult_female <- logit(survival_adult_female^(1/12))
  survival_calf_female <- logit(survival_calf_female^(1/12))
  
  esurvival <- array(0, dim = c(12, nyear, nstage))
  eaf <- array(0, dim = c(12, nyear))
  bannual_af <- vector(length = nyear)
  bmonth_af <- vector(length = 12)
  bannual_month_af <- matrix(0, nrow = 12, ncol = nyear)
  bannual_cf <- vector(length = nyear)
  bmonth_cf <- vector(length = 12)
  bannual_month_cf <- matrix(0, nrow = 12, ncol = nyear)
  # intercept is first year
  year <- 1:nyear - 1

  for(yr in 1:nyear){
    bannual_af[yr] <- rnorm(1, 0, annual_sd_adult_female)
    bannual_cf[yr] <- rnorm(1, 0, annual_sd_calf_female)
    for(mth in 1:12){
      bannual_month_af[mth,yr] <- rnorm(1, 0, annual_month_sd_adult_female)
      bannual_month_cf[mth,yr] <- rnorm(1, 0, annual_month_sd_calf_female)
    }
  }
  for(mth in 1:12){
    bmonth_af[mth] <- rnorm(1, 0, month_sd_adult_female)
    bmonth_cf[mth] <- rnorm(1, 0, month_sd_calf_female)
  }
  
  for(yr in 1:nyear){
    for(mth in 1:12){
      for(stg in 1:nstage){
        eaf[mth, yr] <- survival_adult_female + trend_adult_female * year[yr] + bannual_af[yr] + bmonth_af[mth] + bannual_month_af[mth, yr]
        esurvival[mth, yr, 1] <- ilogit(survival_calf_female + trend_calf_female * year[yr] + bannual_cf[yr] + bmonth_cf[mth] + bannual_month_cf[mth, yr])
        esurvival[mth, yr, 2] <- ilogit(eaf[mth, yr] + yearling_effect)
        esurvival[mth, yr, 3] <- ilogit(eaf[mth, yr]) 
      }
    }
  }
  esurvival
}
