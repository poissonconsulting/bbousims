% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/infix.R
\name{\%*b2\%}
\alias{\%*b2\%}
\title{Stochastic matrix multiplication for age-structured population projection.}
\usage{
x \%*b2\% y
}
\arguments{
\item{x}{A matrix with identical number of rows and columns.}

\item{y}{A vector or matrix with one column.}
}
\value{
A vector of the projected abundance for each stage.
}
\description{
Matrix x should be a process matrix with identical number of rows and columns.
Matrix y should have one column, with each row indicating abundance at a given stage. The number of rows must be identical to the number of columns in x.
A binomial distribution is used, where size is drawn from the y matrix and prob is drawn from the x matrix.
}
\examples{
population0 <- c(148, 82, 111, 99)
survival_mat <- bbs_matrix_survival(c(0.845, 0.872, 0.859, 0.861))
survival_mat \%*b\% population0
}
