#' Survival of collared females.
#' 
#' The number of collared adult females is 'topped up' at each `month_collar`.
#' Survival rates in each month/year determine the number of mortalities. 
#' The probability of uncertain mortality determines the proportion of mortalities that are uncertain. 
#' The probability of uncertain survival reduces the number of collared adult females without adding mortalities. 
#' 
#' @inheritParams params
#' @param survival_adult_female_month_year A matrix of the female adult survival rates with dimensions month and year. 
#'
#' @return A data.frame of the number of collared adult females, certain mortalities and uncertain mortalities in each year and month.
#' @export
#'
bbs_survival_collared <- function(collared_adult_females,
                            survival_adult_female_month_year,
                            probability_uncertain_mortality = 0,
                            probability_uncertain_survival = 0,
                            month_collar = 3L,
                            population_name = "A"){
  starttotal <- collared_adult_females
  nyear <- ncol(survival_adult_female_month_year)
  yearmon <- tidyr::expand_grid(year = 1:nyear, month = 1:12)
  survival <- purrr::map_df(1:nrow(yearmon), ~ {
    month <- yearmon$month[.x]
    year <- yearmon$year[.x]
    phi <- survival_adult_female_month_year[month, year]
    prob_uncertain_mort <- probability_uncertain_mortality
    prob_uncertain_surv <- probability_uncertain_survival
    last <- starttotal[length(starttotal)]
    dead <- rbinom(1, last, (1 - phi))
    dead_uncertain <- rbinom(1, dead, prob_uncertain_mort)
    dead_certain <- dead - dead_uncertain
    alive_uncertain <- rbinom(1, last, prob_uncertain_surv)
    update <- last - dead - alive_uncertain
    if(month == month_collar - 1){
      starttotal <<- c(starttotal, collared_adult_females)
    } else {
      starttotal <<- c(starttotal, update)
    }
    tibble(Year = year, 
                   Month = month,
                   PopulationName = population_name,
                   StartTotal = last,
                   MortalitiesCertain = dead_certain,
                   MortalitiesUncertain = dead_uncertain)
  })
}