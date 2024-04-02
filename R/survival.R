#' Get stochastic Boreal Caribou survival rates by month, year and stage.
#' 
#' Stages include female calves, female yearlings and female adults. 
#' Calf and adult female survival can vary by trend, annual random effect, month random effect and month within annual random effect. 
#' Yearling survival is created as an effect on adult female survival rates.
#' Variation from random effects is stochastic. 
#' Year is scaled to Year - 1 for trend, which ensures that the intercept is the rate in the first year. 
#' 
#' @inheritParams params
#' @param trend_adult_female A number of the effect of an increase of one year on the log-odds adult female monthly survival.
#' @param annual_sd_adult_female A number of the standard deviation of the annual variation in adult female survival on the log-odds monthly survival.
#' @param month_sd_adult_female A number of the standard deviation of the monthly variation in adult female survival on the log-odds monthly survival.
#' @param annual_month_sd_adult_female A number of the standard deviation of the monthly variation within year variation in adult female survival on the log-odds monthly survival.
#' @param trend_calf_female A number of the effect of an increase of one year on the log-odds calf female monthly survival.
#' @param annual_sd_calf_female A number of the standard deviation of the annual variation in calf female survival on the log-odds monthly survival.
#' @param month_sd_calf_female A number of the standard deviation of the monthly variation in calf female survival on the log-odds monthly survival.
#' @param annual_month_sd_calf_female A number of the standard deviation of the monthly variation within year variation in calf female survival on the log-odds monthly survival.
#' @param yearling_effect A number of the effect of yearling on the log-odds adult female monthly survival. 
#'
#' @return An array of survival rates with dimensions month, year, stage.
#' @export
#'
#' @examples
#' survival <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
#' 
bbs_survival_caribou <- function(survival_adult_female, 
                            survival_calf_female = 0.5, 
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
  chk_number(survival_adult_female)
  chk_range(survival_adult_female)
  chk_number(survival_calf_female)
  chk_range(survival_calf_female)
  chk_gte(nyear)
  chk_whole_number(nyear)
  chk_number(trend_adult_female)
  chk_number(annual_sd_adult_female)
  chk_number(month_sd_adult_female)
  chk_number(annual_month_sd_adult_female)
  chk_number(trend_calf_female)
  chk_number(annual_sd_calf_female)
  chk_number(month_sd_calf_female)
  chk_number(annual_month_sd_calf_female)
  chk_number(yearling_effect)
 
  # convert annual to monthly on logit scale
  survival_adult_female <- logit(survival_adult_female^(1/12))
  survival_calf_female <- logit(survival_calf_female^(1/12))
  
  intercept <- c(survival_calf_female, 0, survival_adult_female)
  trend <- c(trend_calf_female, 0, trend_adult_female)
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
#' Year is scaled to Year - 1 for trend, which ensures that the intercept is the rate in the first year. 
#' 
#' @inheritParams params
#' @param intercept A vector of the intercept of the log-odds survival by stage.
#' @param trend A vector of the effect of an increase of one year on the log-odds survival by stage.
#' @param annual_sd A vector of the standard deviation of the annual variation of the log-odds survival by stage.
#' @param period_sd A vector of the standard deviation of the period variation of the log-odds survival by stage.
#' @param annual_period_sd A vector of the standard deviation of the period within annual variation on the log-odds survival by stage.
#' @param nperiod_within_year A whole number of the number of periods in a year. 
#'
#' @return An array of survival rates with dimensions period, year, stage.
#' @export
#'
#' @examples
#' survival <- bbs_survival(intercept = logit(c(0.94, 0.98, 0.98)), trend = c(0, 0, 0.2))
#' 
bbs_survival <- function(intercept, 
                         nyear = 10,
                         trend = rep(0, length(intercept)), 
                         annual_sd = rep(0, length(intercept)),
                         period_sd = rep(0, length(intercept)),
                         annual_period_sd = rep(0, length(intercept)),
                         nperiod_within_year = 12){
  chk_numeric(intercept)
  chk_whole_number(nyear)
  chk_gt(nyear)
  chk_numeric(trend)
  chk_length(trend, length(intercept))
  chk_numeric(trend)
  chk_length(annual_sd, length(intercept))
  chk_numeric(annual_sd)
  chk_length(period_sd, length(intercept))
  chk_numeric(period_sd)
  chk_length(annual_period_sd, length(intercept))
  chk_numeric(annual_period_sd)
  chk_whole_number(nperiod_within_year)
  chk_gt(nperiod_within_year)
  
  nstage <- length(intercept)
  nperiod <- nperiod_within_year
  
  esurvival <- array(0, dim = c(nperiod, nyear, nstage))
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
