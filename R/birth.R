#' Create a birth process matrix.
#' 
#' @param fecundity A vector of the fecundity rates in each state.
#' @param female_recruit_state A number indicating the index position of the female recruit state. 
#' @param male_recruit_state A number indicating the index position of the male recruit state. 
#' @param female_proportion A number between 0 and 1 indicating the proportion of recruits that are female.
#'
#' @return A matrix of the birth subprocess.
#' @export
#'
#' @examples
#' matrix_birth(c(0, 0, 0.2, 0, 0.25, 0)) %*% rep(100, 6)
message("fertility not fecundity - fecundity is fertility and survival combined")
matrix_birth <- function(fecundity, female_recruit_state = 1, male_recruit_state = 2, female_proportion = 0.5){
  chk_numeric(fecundity)
  chk_gte(fecundity)
  chk_whole_number(male_recruit_state)
  chk_range(male_recruit_state, range = c(0, length(fecundity)))
  chk_whole_number(female_recruit_state)
  chk_range(female_recruit_state, range = c(0, length(fecundity)))
  chk_range(female_proportion)
  
  x <- empty_matrix(length(fecundity))
  for(i in seq_along(fecundity)){
    x[female_recruit_state,i] <- fecundity[i]*female_proportion
    x[male_recruit_state,i] <- fecundity[i]*(1 - female_proportion)
  }
  diag(x) <- 1
  x
}

#' Create a birth process matrix for each year.
#' 
#' @param fecundity A matrix of the fecundity rates with dimensions year and state.
#'
#' @return An array with dimensions state, state, year.
#' @export
#'
#' @examples
#' #' if (interactive()) {
#'   birth_year(matrix(c(0, 0.2, 0, 0.3, 0,
#'                       0, 0.3, 0, 0.35, 0,
#'                       0, 0.25, 0, 0.3, 0), ncol = 5, byrow = TRUE))
#' }
#' 
matrix_birth_year <- function(fecundity, female_recruit_state = 1, male_recruit_state = 2, female_proportion = 0.5){
  dims <- dim(fecundity)
  nyear <- dims[1]
  nstate <- dims[2]
  x <- array(0, dim = c(nstate, nstate, nyear))
  for(year in 1:nyear){
    x[,,year] <- matrix_birth(fecundity[year,], 
                              female_recruit_state = female_recruit_state,
                              male_recruit_state = male_recruit_state,
                              female_proportion = female_proportion)
  }
  x
}
