#' Stochastic matrix multiplication for age-structured population projection.
#' 
#' This assumes that matrix x has identical number of rows and columns; matrix y has one column; and matrix y has the same number of rows as matrix x. 
#' A binomial distribution is used, where size is drawn from the y matrix and prob is drawn from the x matrix. 
#'  
#' @param x A matrix with identical number of rows and columns.
#' @param y A matrix with one column. If vector, it will be converted to a one-column matrix.
#'
#' @return A matrix.
#' @export
#'
#' @examples
#' if(interactive()){
#'   population0 <- c(148, 82, 111, 99)
#'   survival_mat <- matrix_survival(c(0.845, 0.872, 0.859, 0.861))
#'   set.seed(102)
#'   survival_mat %*b% population0
#' }
#' 
`%*b%` <- function(x, y){
  y <- as.matrix(y)
  chk_identical(ncol(y), 1L)
  chk_identical(ncol(x), nrow(y))
  chk_identical(nrow(x), ncol(x))
  
  res <- matrix(0, ncol = 1, nrow = nrow(x))
  
  for(i in 1:nrow(x)){
    bin <- vector(length = nrow(x))
    for(j in 1:length(bin)){
      bin[j] <- rbinom(1, size = y[i], prob = x[i,j])
    }
    res[i,1] <- sum(bin)
  }
  res
}
