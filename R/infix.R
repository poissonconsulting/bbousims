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
#' population0 <- c(148, 82, 111, 99)
#' survival_mat <- bbs_matrix_survival(c(0.845, 0.872, 0.859, 0.861))
#' survival_mat %*b% population0
`%*b%` <- function(x, y) {
  y <- as.matrix(y)
  chk_identical(ncol(y), 1L)
  chk_identical(ncol(x), nrow(y))
  chk_identical(nrow(x), ncol(x))

  res <- vector(length = nrow(x))

  for (i in seq_len(nrow(x))) {
    bin <- vector(length = nrow(x))
    for (j in seq_along(bin)) {
      bin[j] <- rbinom(1, size = as.integer(y[j, 1]), prob = x[i, j])
    }
    res[i] <- sum(bin)
  }
  matrix(res, ncol = 1)
}


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
#' population0 <- c(148, 82, 111, 99)
#' survival_mat <- bbs_matrix_survival(c(0.845, 0.872, 0.859, 0.861))
#' survival_mat %*b% population0
`%*b2%` <- function(x, y) {
  chk_identical(ncol(x), ncol(y))
  chk_identical(nrow(x), nrow(y))

  res <- matrix(0, ncol = ncol(x), nrow = nrow(x))
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      res[i, j] <- rbinom(1, size = x[i, j], prob = y[i, j])
    }
  }
  res
}
