#' Create an age process matrix.
#'
#' @param age A vector indicating the stage to age into. If ageing does not occur, the stage should reference itself. 
#'
#' @return A matrix of the age subprocess.
#' @export
#'
#' @examples
#' matrix_age(c(2, 3, 3)) %*% c(80, 50, 150)
matrix_age <- function(age = c(2, 3, 3)){
  chk_whole_numeric(age)
  chk_range(age, range(0, length(age)))
  
  x <- empty_matrix(length(age))
  for(i in seq_along(age)){
    x[age[i], i] <- 1
  }
  x
}
