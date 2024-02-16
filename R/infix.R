#' Stochastic matrix multiplication for age-structured population projection.
#' 
#' Matrix x should be a process matrix with identical number of rows and columns.
#' Matrix y should have one column, with each row indicating abundance at a given stage. The number of rows must be identical to the number of columns in x. 
#' A binomial distribution is used, where size is drawn from the y matrix and prob is drawn from the x matrix. 
#'  
#' @param x A matrix with identical number of rows and columns.
#' @param y A vector or matrix with one column. 
#'
#' @return A vector of the projected abundance for each stage.
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
  
  res <- vector(length = nrow(x))
  
  for(i in 1:nrow(x)){
    bin <- vector(length = nrow(x))
    for(j in 1:length(bin)){
      bin[j] <- rbinom(1, size = as.integer(y[j,1]), prob = x[i,j])
    }
    res[i] <- sum(bin)
  }
  res
}
