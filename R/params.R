#' Parameter Descriptions for bboutools Functions
#'
#' @param ... Unused parameters.
#' @param x The object.
#' @param survival_adult_female A number between 0 and 1 of the annual female adult survival. 
#' @param survival_calf_female A number between 0 and 1 of the annual female calf survival. 
#' @param calves_per_adult_female A number of the calves per adult female. 
#' @param proportion_female A number between 0 and 1 indicating the proportion of recruits that are female.
#' @param nyear A whole number of the number of years. 
#' @param stochastic A flag indicating whether to 
#' @param female_recruit_stage A positive whole number of the stage representing female recruits.
#' @param male_recruit_stage A positive whole number of the stage representing male recruits. Ignored if NULL. 
#' @param stochastic A flag indicating whether to include demographic stochasticity.
#' @param month_collar A whole number between 1 and 12 of the collaring month. 
#' @param collared_adult_females A whole positive number of the number of collared adult females. 
#' The number of collared adult females is 'topped up' each year at `month_collar`.
#' @param probability_uncertain_mortality A number between 0 and 1 of the probability of uncertain mortality. 
#' @param probability_uncertain_survival A number between 0 and 1 of the probability of uncertain survival. 
#' @param probability_unsexed_adult_female A number between 0 and 1 of the probability that an adult female is unsexed. 
#' @param probability_unsexed_adult_male A number between 0 and 1 of the probability that an adult male is unsexed. 
#' @param adult_females A number of the initial number of adult females in the population. 
#' @param proportion_adult_female A number between 0 and 1 of the proportion of adults that are female.
#' @param proportion_yearling_female A number between 0 and 1 of the proportion of yearlings that are female.
#' @param survival_trend_adult_female A number of the effect of an increase of one year on the log-odds adult female survival. 
#' @param survival_trend_calf_female A number of the effect of an increase of one year on the log-odds calf female survival. 
#' @param survival_annual_sd_adult_females A number of the standard deviation of the annual variation in log-odds adult female survival. 
#' @param survival_annual_sd_calf_females A number of the standard deviation of the annual variation in log-odds calf female survival. 
#' @param survival_month_sd_adult_females A number of the standard deviation of the monthly variation in log-odds adult female survival. 
#' @param survival_month_sd_calf_females A number of the standard deviation of the monthly variation in log-odds calf female survival. 
#' @param survival_annual_month_sd_adult_females A number of the standard deviation of the month within annual variation in log-odds adult female survival. 
#' @param survival_annual_month_sd_calf_females A number of the standard deviation of the month within annual variation in log-odds calf female survival. 
#' @param calves_per_adult_female_trend A number of the effect of an increase of one year on the log-odds calves per adult female. 
#' @param calves_per_adult_female_annual_sd A number of the standard deviation of the annual variation on the log-odds calves per adult female.
#' @param population A matrix of the population by stage and period (output of [bbs_population()] or [bbs_population_caribou()]).
#' @param group_size_lambda A number of the lambda value of the gamma-poisson distribution to draw group sizes from. 
#' @param group_size_theta A number of the theta value of the gamma-poisson distribution to draw group sizes from. 
#' @param group_max_proportion A number between 0 and 1 of the maximum group size as proportion of the total population. 
#' @param group_min_size A whole positive number of the minimum group size.
#' @param recruit_stages A vector of whole numbers indicating the recruit stages (e.g., calf).
#' @param reproductive_female_stages A vector of whole numbers indicating the reproductive female stages (e.g., cows).
#'
#' @keywords internal
#' @name params
NULL