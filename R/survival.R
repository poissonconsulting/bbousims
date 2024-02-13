#' Create a survival matrix.
#' 
#' @param survival A vector of the survival rates in each state.
#'
#' @return A survival population matrix.
#' @export
#'
#' @examples
#' survival_matrix(c(0.87, 0.84, 0.84, 0.89, 0.9)) %*% rep(100, 5)
survival_matrix <- function(survival){
  chk_numeric(survival)
  chk_range(survival)
  nstate <- length(survival)
  x <- matrix(rep(0, nstate*nstate), ncol = nstate)
  for(i in 1:nstate){
    x[i,i] <- survival[i]
  }
  x
}

#' Create survival matrix for each year and month.
#' 
#' @param survival An array of the survival rates with dimensions month, year and state.
#'
#' @return An array of survival population matrices with dimensions state, state, year, month.
#' @export
#'
#' @examples
#' #' if (interactive()) {
#'   x1 <- matrix(rep(c(0.98, 0.97), 12), nrow = 12, byrow = TRUE)
#'   x2 <- matrix(rep(c(0.97, 0.96), 12), nrow = 12, byrow = TRUE)
#'   x3 <- matrix(rep(c(0.95, 0.94), 12), nrow = 12, byrow = TRUE)
#'   x4 <- matrix(rep(c(0.99, 0.99), 12), nrow = 12, byrow = TRUE)
#'   x5 <- matrix(rep(c(0.98, 0.96), 12), nrow = 12, byrow = TRUE)
#'   
#'   survival <- array(c(x1, x2, x3, x4, x5), dim = c(12, 2, 5))
#'   survival_year_month(survival)
#' }
#' 
survival_year_month <- function(survival){
  dims <- dim(survival)
  nyear <- dims[2]
  nstate <- dims[3]
  x <- array(0, dim = c(nstate, nstate, nyear, 12))
  for(year in 1:nyear){
    for(month in 1:12){
      x[,,year,month] <- survival_matrix(survival[month, year, ])
    }
  }
  x
}
