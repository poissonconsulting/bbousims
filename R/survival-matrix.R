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

#' Create a survival process matrix.
#'
#' @param survival A vector of the survival rates in each stage.
#'
#' @return A matrix of the survival subprocess.
#' @export
#'
#' @examples
#' bbs_matrix_survival(c(0.5, 0.83, 0.84)) %*% rep(100, 3)
bbs_matrix_survival <- function(survival) {
  chk_numeric(survival)
  chk_range(survival)

  x <- empty_matrix(length(survival))
  diag(x) <- survival
  x
}

#' Create survival matrix for each year and period.
#'
#' @param survival An array of the survival rates with dimensions period, year and stage. Period represents any subdivision of a year (i.e., week, month, season).
#'
#' @return An array of survival process matrices with dimensions stage, stage, year, period.
#' @export
#'
#' @examples
#' survival_rates <- bbs_survival(logit(c(0.94, 0.98)), nyear = 2, nperiod_within_year = 1)
#' bbs_matrix_survival_period(survival_rates$eSurvival)
bbs_matrix_survival_period <- function(survival) {
  chk_is(survival, "array")
  chk_length(dim(survival), 3L)

  dims <- dim(survival)
  nperiod <- dims[1]
  nyear <- dims[2]
  nstate <- dims[3]
  x <- array(0, dim = c(nstate, nstate, nyear, nperiod))
  for (year in 1:nyear) {
    for (period in 1:nperiod) {
      x[, , year, period] <- bbs_matrix_survival(survival[period, year, ])
    }
  }
  x
}
