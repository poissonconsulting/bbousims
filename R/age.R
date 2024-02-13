#' Create an ageing matrix.
#'
#' It is assumed that number of states is odd, where the first state has no sex assignment and the subsequent states are grouped by female and male.  
#' Even states are female and odd states are male. 
#' State 1 will age into state 2 (female) and 3 (male) according to the proportion_female.
#' There must be at least 3 states.
#' The final two states retain their own individuals and collect individuals in the previous two states.
#'
#' @param nstate The number of states.
#' @param proportion_female A number between 0 and 1 of the proportion of new recruits that are female. 
#'
#' @return An ageing population matrix.
#' @export
#'
#' @examples
#' age_matrix(0.5, 5) %*% c(100, 50, 50, 75, 80)

age_matrix <- function(proportion_female, nstate){
  chk_range(proportion_female)
  chk_whole_number(nstate)
  chk_range(nstate, c(3, Inf))
  x <- matrix(rep(0, nstate*nstate), ncol = nstate)
  # first state ages into second and third state according to female ratio
  x[2,1] <- proportion_female
  x[3,1] <- 1 - proportion_female
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



