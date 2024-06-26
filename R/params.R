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

#' Parameter Descriptions for bboutools Functions
#'
#' @param ... Unused parameters.
#' @param x The object.
#' @param population_name A string of the population name. This does not affect simulation but can be used as a unique identifier.
#' @param survival_adult_female A number between 0 and 1 of the annual female adult survival.
#' @param survival_calf_female A number between 0 and 1 of the annual female calf survival.
#' @param calves_per_adult_female A number of the calves per adult female.
#' @param proportion_female A number between 0 and 1 indicating the proportion of recruits that are female.
#' @param nyear A whole number of the number of years.
#' @param nperiod_within_year A whole number of the number of periods in a year.
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
#' @param population A matrix of the population by stage and period (output of [bbs_population()] or [bbs_population_caribou()]).
#' @param group_size_lambda A number of the lambda value of the gamma-poisson distribution to draw group sizes from.
#' @param group_size_theta A number of the theta value of the gamma-poisson distribution to draw group sizes from.
#' @param group_max_proportion A number between 0 and 1 of the maximum group size as proportion of the total population.
#' @param group_min_size A whole positive number of the minimum group size.
#' @param month_composition A whole number between 1 and 12 of the month that composition surveys take place, relative to the start of the biological year.
#' @param group_coverage A number between 0 and 1 of the proportion of groups sampled.
#' @param recruit_stages A vector of whole numbers indicating the recruit stages (e.g., calf).
#' @param reproductive_female_stages A vector of whole numbers indicating the reproductive female stages (e.g., cows).
#' @param survival A list of the Caribou survival rates (output of [bbs_survival_caribou()])
#' @param fecundity A list of the Caribou fecundity rates (output of [bbs_fecundity_caribou()])
#'
#' @keywords internal
#' @name params
NULL
