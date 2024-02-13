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
