#' Create an ageing matrix.
#'
#' It is assumed that number of states is odd, where the first state has no sex assignment and the subsequent states are grouped by female and male.  
#' Even states are female and odd states are male. 
#' State 1 will age into state 2 (female) and 3 (male) according to the female_proportion.
#' There must be at least 3 states.
#' The final two states retain their own individuals and collect individuals in the previous two states.
#'
#' @param nstate The number of states.
#' @param female_proportion A number between 0 and 1 of the proportion of new recruits that are female. 
#'
#' @return An ageing population matrix.
#' @export
#'
#' @examples
#' age_matrix(0.5, 5) %*% c(100, 50, 50, 75, 80)

age_matrix <- function(female_proportion, nstate){
  chk_range(female_proportion)
  chk_whole_number(nstate)
  chk_range(nstate, c(3, Inf))
  x <- matrix(rep(0, nstate*nstate), ncol = nstate)
  # first state ages into second and third state according to female ratio
  x[2,1] <- female_proportion
  x[3,1] <- 1 - female_proportion
  # last two states collect
  x[nstate, nstate] <- 1
  x[nstate-1, nstate-1] <- 1
  # the rest age normally 
  if(nstate > 3){
    for(i in 4:nstate){
      x[i, i-2] <- 1
    }
  }
  x
}

#' Create age matrix for each year.
#' 
#' @param female_proportion A vector of female_proportion rates in each year.
#' @param nstate A whole number of the number of states. 
#'
#' @return An array with dimensions state, state, year.
#' @export
#'
#' @examples
#' #' if (interactive()) {
#'   age_year(rep(0.5, 3), 5)
#' }
#' 
age_year <- function(female_proportion, nstate){
  nyear <- length(female_proportion)
  x <- array(0, dim = c(nstate, nstate, nyear))
  for(year in 1:nyear){
    x[,,year] <- age_matrix(female_proportion[year], nstate)
  }
  x
}


