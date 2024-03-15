initial_population <- function(adult_females, 
                               stable_stage_dist, 
                               proportion_adult_female, 
                               proportion_yearling_female){
  
  n_af <- adult_females
  dist_a <- stable_stage_dist[3]
  dist_y <- stable_stage_dist[2]
  dist_c <- stable_stage_dist[1]

  n <- n_af / dist_a
  n_yf <- n * dist_y
  n_cf <- n * dist_c
  n_am <- n_af/proportion_adult_female - n_af
  n_ym <- n_yf/proportion_yearling_female - n_yf
  # assuming proportion yearling female same as proportion calf female
  n_cm <- n_cf/proportion_yearling_female - n_cf
  
  round(c(n_cf, n_cm, n_yf, n_ym, n_af, n_am))
}

#' Demographic summary
#' 
#' Generate calf-cow ratio, DeCesare recruitment, lambda, leslie matrix and stable-stage distribution 
#' from demographic parameters. 
#' 
#' @inheritParams params
#' @param survival_adult_female A number between 0 and 1 of the annual adult female survival. 
#' @param survival_calf number between 0 and 1 of the annual calf survival. 
#' @param survival_yearling A number between 0 and 1 of the annual yearling survival. 
#' 
#' @return A named list of the calf-cow ratio, DeCesare recruitment, leslie matrix, lambda estimate and stable-stage distribution. 
#' @export
#'
bb_demographic_summary <- function(calves_per_adult_female, 
                                   proportion_adult_female,
                                   proportion_yearling_female,
                                   survival_adult_female, 
                                   survival_calf, 
                                   survival_yearling = survival_adult_female){
  
  calf_cow <- (calves_per_adult_female * survival_calf) / (survival_adult_female + 0.5 * survival_yearling_female)
  
  #DeCesare Recruitment.
  recruitment <- calf_cow * proportion_yearling_female / (1 + calf_cow * proportion_yearling_female)
  
  female_calves <- proportion_yearling_female * survival_adult_female * calves_per_adult_female
  leslie <- matrix(c(0,  0,  female_calves,
                     survival_calf, 0,  0,
                     0,  survival_yearling_female, survival_adult_female), 
                   nrow = 3, byrow = TRUE)
  
  ev <- eigen(leslie)
  
  #lambda is dominant eigenvalue-close to HB estimate
  lmax <- which.max(Re(ev$values))
  lambda <- Re(ev$values)[lmax]
  
  stage_dist <- popbio::eigen.analysis(leslie)$stable.stage
  
  list(calf_cow_ratio = calf_cow,
       recruitment = recruitment,
       leslie_matrix = leslie,
       lambda = lambda,
       stable_stage_dist = stage_dist,
       proportion_adult_female = proportion_adult_female,
       proportion_yearling_female = proportion_yearling_female)
}