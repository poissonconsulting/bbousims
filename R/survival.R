#' Create a survival process matrix.
#' 
#' @param survival A vector of the survival rates in each state.
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
#' @param survival An array of the survival rates with dimensions period, year and state. Period represents any subdivision of a year (i.e., week, month, season).
#'
#' @return An array of survival process matrices with dimensions state, state, year, period.
#' @export
#'
#' @examples
#' #' if (interactive()) {
#'   survival_rates <- lapply(c(-0.01, 0.01, 0, -0.02, 0.01, 0.01), function(x){
#'    rep(c(0.98, 0.97, 0.96), 4) + x
#'   })
#'.  survival_rates <- array(unlist(survival_rates), dim = c(3, 4, 6))
#'   matrix_survival_period(survival_rates)
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
