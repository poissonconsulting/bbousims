#' Create an age process matrix.
#'
#' @param age A vector indicating the stage to age into. If ageing does not occur, the stage should reference itself. 
#'
#' @return A matrix of the age subprocess.
#' @export
#'
#' @examples
#' matrix_age(c(3, 4, 5, 6, 5, 6)) %*% c(100, 50, 50, 75, 80, 60)
matrix_age <- function(age){
  chk_whole_numeric(age)
  chk_range(age, range(0, length(age)))
  
  x <- empty_matrix(length(age))
  for(i in seq_along(age)){
    x[age[i], i] <- 1
  }
  x
}
