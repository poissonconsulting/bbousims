#' @importFrom extras logit logit
#' @export
extras::logit

#' @importFrom extras logit ilogit
#' @export
extras::ilogit

#' Inverse Logarithmic Transformation
#' @param x A numeric object.
#' @export
#' @examples
#' ilog(10)
ilog <- function(x) exp(x)
