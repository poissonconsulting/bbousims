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
