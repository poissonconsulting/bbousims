# Copyright 2024 Province of Alberta
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
#' @examples
#' survival <- bbs_survival_caribou(0.84)
#' survival_adult_female <- survival$eSurvival[, , 3]
#' survival_collared <- bbs_survival_collared(
#'   collared_adult_females = 30,
#'   survival_adult_female_month_year = survival_adult_female
#' )
bbs_survival_collared <- function(collared_adult_females,
                                  survival_adult_female_month_year,
                                  probability_uncertain_mortality = 0,
                                  probability_uncertain_survival = 0,
                                  month_collar = 1L,
                                  population_name = "A") {
  starttotal <- collared_adult_females
  nyear <- ncol(survival_adult_female_month_year)
  yearmon <- tidyr::expand_grid(year = 1:nyear, month = 1:12)
  # remove first months without collaring
  yearmon <- dplyr::filter(yearmon, !(.data$year == 1 & .data$month < month_collar))

  purrr::map_df(seq_len(nrow(yearmon)), ~ {
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
    if (month == month_collar - 1 || (month == 12 & month_collar == 1)) {
      starttotal <<- c(starttotal, collared_adult_females)
    } else {
      starttotal <<- c(starttotal, update)
    }
    tibble(
      Year = year,
      Month = month,
      PopulationName = population_name,
      StartTotal = last,
      MortalitiesCertain = dead_certain,
      MortalitiesUncertain = dead_uncertain
    )
  })
}
