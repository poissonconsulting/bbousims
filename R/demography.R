initial_population <- function(adult_females, 
                               stable_stage_dist){
  
  n_af <- adult_females
  dist_a <- stable_stage_dist[3]
  dist_y <- stable_stage_dist[2]
  dist_c <- stable_stage_dist[1]

  n <- n_af / dist_a
  n_yf <- n * dist_y
  n_cf <- n * dist_c
  
  round(c("female_calf" = n_cf, 
          "female_yearling" = n_yf, 
          "female_adult" = n_af))
}

#' Demographic summary
#' 
#' Generate calf-cow ratio, DeCesare recruitment, lambda, leslie matrix and stable-stage distribution 
#' from demographic parameters. 
#' 
#' @inheritParams params
#' @param sex_ratio A number between 0 and 1 of the proportion of females at birth and yearlings. 
#' @param survival_adult_female A number between 0 and 1 of the annual adult female survival. 
#' @param survival_calf number between 0 and 1 of the annual calf survival. 
#' @param survival_yearling A number between 0 and 1 of the annual yearling survival. 
#' 
#' @return A named list of the calf-cow ratio, DeCesare recruitment, leslie matrix, lambda estimate and stable-stage distribution. 
#' @export
#'
bb_demographic_summary <- function(sex_ratio, 
                                   calves_per_adult_female, 
                                   survival_adult_female, 
                                   survival_calf, 
                                   survival_yearling = survival_adult_female){
  
  calf_cow <- (calves_per_adult_female * survival_calf) / (survival_adult_female + 0.5 * survival_yearling)
  
  #DeCesare Recruitment.
  recruitment <- calf_cow * sex_ratio / (1 + calf_cow * sex_ratio)
  
  female_calves <- sex_ratio * survival_adult_female * calves_per_adult_female
  leslie <- matrix(c(0,  0,  female_calves,
                     survival_calf, 0,  0,
                     0,  survival_yearling, survival_adult_female), 
                   nrow = 3, byrow = TRUE)
  
  #lambda is dominant eigenvalue-close to HB estimate
  lambda <- popbio::lambda(leslie)
  stage_dist <- popbio::stable.stage(leslie)
  
  list(calf_cow_ratio = calf_cow,
       recruitment = recruitment,
       leslie_matrix = leslie,
       lambda = lambda,
       stable_stage_dist = stage_dist)
}