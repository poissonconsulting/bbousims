#' @export
print.bbou_population <- function(x, ...) {
  attr(x, "fecundity") <- NULL
  attr(x, "survival") <- NULL
  attr(x, "class") <- NULL
  print.default(x)
}

#' @export
print.bbou_population_caribou <- function(x, ...) {
  attr(x, "fecundity") <- NULL
  attr(x, "survival") <- NULL
  attr(x, "class") <- NULL
  print.default(x)
}

#' @export
print.bbou_simulation <- function(x, ...) {
  attr(x, "class") <- NULL
  print.default(x)
}