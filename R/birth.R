#' Create a birth matrix.
#' 
#' @param fecundity A vector of the fecundity rates in each state.
#'
#' @return A birth population matrix.
#' @export
#'
#' @examples
#' birth_matrix(c(0, 0.2, 0, 0.25, 0)) %*% rep(100, 5)

birth_matrix <- function(fecundity){
  chk_numeric(fecundity)
  chk_range(fecundity)
  nstate <- length(fecundity)
  x <- matrix(rep(0, nstate*nstate), ncol = nstate)
  for(i in 1:nstate){
    x[i,i] <- 1
    x[1,i] <- fecundity[i]
  }
  x
}

#' Create birth matrix for each year.
#' 
#' @param fecundity A matrix of the fecundity rates with dimensions year and state.
#'
#' @return An array with dimensions state, state, year.
#' @export
#'
#' @examples
#' #' if (interactive()) {
#'   birth_year(matrix(c(0, 0.2, 0, 0.3, 0,
#'                       0, 0.3, 0, 0.35, 0,
#'                       0, 0.25, 0, 0.3, 0), ncol = 5, byrow = TRUE))
#' }
#' 
birth_year <- function(fecundity){
  dims <- dim(fecundity)
  nyear <- dims[1]
  nstate <- dims[2]
  x <- array(0, dim = c(nstate, nstate, nyear))
  for(year in 1:nyear){
    x[,,year] <- birth_matrix(fecundity[year,])
  }
  x
}
