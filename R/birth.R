#' Get stochastic fecundity rates by year and stage.
#'
#' Year is scaled to Year - 1 for trend, which ensures that the intercept is the rate in the first year.
#'
#' @inheritParams params
#' @param intercept A vector of the intercept of the log-odds fecundity by stage. If NA, fecundity is set to 0 for all years.
#' @param trend A vector of the effect of an increase of one year on the log-odds fecundity by stage.
#' @param annual_sd A vector of the standard deviation of the annual variation on the log-odds fecundity by stage.
#'
#' @return A matrix of fecundity rates with dimensions year and stage.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   bbs_fecundity(c(NA, logit(0.4)), trend = c(NA, 0.1), annual_sd = c(NA, 0.05))
#' }
#'
bbs_fecundity <- function(intercept,
                          trend = rep(0, length(intercept)),
                          annual_sd = rep(0, length(intercept)),
                          nyear = 10) {
  chk_numeric(intercept)
  chk_numeric(trend)
  chk_length(trend, length(intercept))
  chk_numeric(annual_sd)
  chk_length(annual_sd, length(intercept))
  chk_gte(nyear)
  chk_whole_number(nyear)

  nstage <- length(intercept)
  efecundity <- array(0, dim = c(nyear, nstage))
  bannual <- array(0, dim = c(nyear, nstage))
  year <- 1:nyear - 1
  zero <- is.na(intercept)
  annual_sd[is.na(annual_sd)] <- 0

  for (stg in 1:nstage) {
    for (yr in 1:nyear) {
      bannual[yr, stg] <- rnorm(1, 0, annual_sd[stg])
    }
  }

  for (yr in 1:nyear) {
    for (stg in 1:nstage) {
      efecundity[yr, stg] <-
        ilogit(intercept[stg] + trend[stg] * year[yr] + bannual[yr, stg])
    }
  }

  efecundity[, zero] <- 0
  list(
    eFecundity = efecundity,
    b0 = intercept,
    bYear = trend,
    bAnnual = bannual
  )
}

#' Get stochastic Boreal Caribou fecundity rates by year and stage.
#'
#' Year is scaled to Year - 1 for trend, which ensures that `calves_per_adult_female` is the rate in the first year.
#'
#' @inheritParams params
#' @param calves_per_adult_female A number of the calves per adult female.
#' @param trend A number of the effect of an increase of one year on the log-odds calves per adult female.
#' @param annual_sd A number of the standard deviation of the annual variation on the log-odds calves per adult female.
#'
#' @return A matrix of fecundity rates with dimensions year and stage.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   bbs_fecundity_caribou(0.4, trend = 0.1, annual_sd = 0.3)
#' }
#'
bbs_fecundity_caribou <- function(calves_per_adult_female,
                                  trend = 0,
                                  annual_sd = 0,
                                  nyear = 10) {
  chk_number(calves_per_adult_female)
  chk_gte(calves_per_adult_female)
  chk_number(trend)
  chk_number(annual_sd)
  chk_gte(nyear)
  chk_whole_number(nyear)

  intercept <- c(NA, NA, logit(calves_per_adult_female))
  trend <- c(0, 0, trend)
  annual_sd <- c(0, 0, annual_sd)

  bbs_fecundity(intercept,
    trend = trend,
    annual_sd = annual_sd,
    nyear = nyear
  )
}
