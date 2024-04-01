#' Create a survival process matrix.
#' 
#' @param survival A vector of the survival rates in each stage.
#'
#' @return A matrix of the survival subprocess.
#' @export
#'
#' @examples
#' if(interactive()){
#'   bbs_matrix_survival(c(0.5, 0.83, 0.84)) %*% rep(100, 3)
#' }
#' 
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
#'     survival_rates <- bbs_survival(logit(c(0.94, 0.98)), nyear = 2, nperiod_within_year = 1)
#'     bbs_matrix_survival_period(survival_rates)
#' }
#' 
bbs_matrix_survival_period <- function(survival){
  chk_is(survival, "array")
  chk_length(dim(survival), 3L)
  
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
