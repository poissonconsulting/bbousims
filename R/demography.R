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

initial_population <- function(adult_females,
                               stable_stage_dist) {
  n_af <- adult_females
  dist_a <- stable_stage_dist[3]
  dist_y <- stable_stage_dist[2]
  dist_c <- stable_stage_dist[1]

  n <- n_af / dist_a
  n_yf <- n * dist_y
  n_cf <- n * dist_c

  round(c(
    "female_calf" = n_cf,
    "female_yearling" = n_yf,
    "female_adult" = n_af
  ))
}

female_calves <- function(proportion_female, survival_adult_female, calves_per_adult_female) {
  proportion_female * survival_adult_female * calves_per_adult_female
}

calf_cow_ratio <- function(calves_per_adult_female,
                           survival_adult_female,
                           survival_calf,
                           survival_yearling) {
  (calves_per_adult_female * survival_calf) / (survival_adult_female + 0.5 * survival_yearling)
}

decesare_recruitment <- function(calf_cow, proportion_female) {
  calf_cow * proportion_female / (1 + calf_cow * proportion_female)
}

leslie_matrix <- function(female_calves, survival_calf, survival_yearling, survival_adult_female) {
  matrix(
    c(
      0, 0, female_calves,
      survival_calf, 0, 0,
      0, survival_yearling, survival_adult_female
    ),
    nrow = 3, byrow = TRUE
  )
}

stable_stage_distribution <- function(calves_per_adult_female,
                                      survival_adult_female,
                                      survival_calf,
                                      survival_yearling,
                                      proportion_female) {
  female_calves <- female_calves(
    proportion_female = proportion_female,
    survival_adult_female = survival_adult_female,
    calves_per_adult_female = calves_per_adult_female
  )

  leslie <- leslie_matrix(
    female_calves = female_calves,
    survival_calf = survival_calf,
    survival_yearling = survival_yearling,
    survival_adult_female = survival_adult_female
  )

  popbio::stable.stage(leslie)
}

estimate_lambda <- function(calves_per_adult_female,
                            survival_adult_female,
                            survival_calf,
                            survival_yearling,
                            proportion_female) {
  female_calves <- female_calves(
    proportion_female = proportion_female,
    survival_adult_female = survival_adult_female,
    calves_per_adult_female = calves_per_adult_female
  )

  leslie <- leslie_matrix(
    female_calves = female_calves,
    survival_calf = survival_calf,
    survival_yearling = survival_yearling,
    survival_adult_female = survival_adult_female
  )

  popbio::lambda(leslie)
}

#' Demographic summary
#'
#' Generate calf-cow ratio, DeCesare recruitment, lambda, leslie matrix and stable-stage distribution
#' from demographic parameters.
#'
#' @inheritParams params
#' @param survival_calf number between 0 and 1 of the annual calf survival.
#' @param survival_yearling A number between 0 and 1 of the annual yearling survival.
#'
#' @return A named list of the calf-cow ratio, DeCesare recruitment, leslie matrix, lambda estimate and stable-stage distribution.
#' @export
#' @examples
#' bbs_demographic_summary()
bbs_demographic_summary <- function(calves_per_adult_female = 0.7,
                                    survival_adult_female = 0.85,
                                    survival_calf = 0.5,
                                    survival_yearling = survival_adult_female,
                                    proportion_female = 0.5) {
  calf_cow <- calf_cow_ratio(
    calves_per_adult_female = calves_per_adult_female,
    survival_adult_female = survival_adult_female,
    survival_calf = survival_calf,
    survival_yearling = survival_yearling
  )

  recruitment <- decesare_recruitment(calf_cow,
    proportion_female = proportion_female
  )

  stage_dist <- stable_stage_distribution(
    calves_per_adult_female = calves_per_adult_female,
    survival_adult_female = survival_adult_female,
    survival_calf = survival_calf,
    survival_yearling = survival_yearling,
    proportion_female = proportion_female
  )

  lambda <- estimate_lambda(
    calves_per_adult_female = calves_per_adult_female,
    survival_adult_female = survival_adult_female,
    survival_calf = survival_calf,
    survival_yearling = survival_yearling,
    proportion_female = proportion_female
  )

  list(
    calf_cow_ratio = calf_cow,
    recruitment = recruitment,
    lambda = lambda,
    stable_stage_dist = stage_dist
  )
}
