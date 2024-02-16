#' Create a birth process matrix.
#' 
#' @param fecundity A vector of the fecundity rates in each stage.
#' @inheritParams params
#'
#' @return A birth process matrix.
#' @export
#'
#' @examples
#' matrix_birth(c(0, 0, 0.2, 0, 0.25, 0)) %*% rep(100, 6)
matrix_birth <- function(fecundity, female_recruit_stage = 1, male_recruit_stage = 2, female_proportion = 0.5){
  chk_numeric(fecundity)
  chk_gte(fecundity)
  chk_whole_number(male_recruit_stage)
  chk_range(male_recruit_stage, range = c(0, length(fecundity)))
  chk_whole_number(female_recruit_stage)
  chk_range(female_recruit_stage, range = c(0, length(fecundity)))
  chk_range(female_proportion)
  
  x <- empty_matrix(length(fecundity))
  for(i in seq_along(fecundity)){
    x[female_recruit_stage,i] <- fecundity[i]*female_proportion
    x[male_recruit_stage,i] <- fecundity[i]*(1 - female_proportion)
  }
  diag(x) <- 1
  x
}

#' Create a birth process matrix for each year.
#' 
#' @param fecundity A matrix of the fecundity rates with dimensions year and stage.
#' @inheritParams params
#' @return An array of the birth process matrices with dimensions stage, stage, year.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   birth_year(matrix(c(0, 0.2, 0, 0.3, 0,
#'                       0, 0.3, 0, 0.35, 0,
#'                       0, 0.25, 0, 0.3, 0), ncol = 5, byrow = TRUE))
#' }
matrix_birth_year <- function(fecundity, female_recruit_stage = 1, male_recruit_stage = 2, female_proportion = 0.5){
  dims <- dim(fecundity)
  nyear <- dims[1]
  nstate <- dims[2]
  x <- array(0, dim = c(nstate, nstate, nyear))
  for(year in 1:nyear){
    x[,,year] <- matrix_birth(fecundity[year,], 
                              female_recruit_stage = female_recruit_stage,
                              male_recruit_stage = male_recruit_stage,
                              female_proportion = female_proportion)
  }
  x
}
