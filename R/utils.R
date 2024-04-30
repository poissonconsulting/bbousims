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

year_period <- function(year, period, max_period) {
  ((year - 1) * max_period) + period
}

period_to_year <- function(x, nperiod_within_year = 12) {
  ceiling(x / nperiod_within_year)
}

period_to_month <- function(x) {
  year <- period_to_year(x)
  x - ((year - 1) * 12)
}

.center <- function(x) {
  scale(x, center = TRUE, scale = FALSE)
}

empty_matrix <- function(n, value = 0) {
  matrix(rep(value, n * n), ncol = n)
}

population_tbl <- function(x, nperiod_within_year = 12) {
  colnames(x) <- seq_len(ncol(x))
  nstep <- ncol(x) + 1
  x %>%
    as.data.frame() %>%
    mutate(Stage = factor(seq_len(nrow(x)))) %>%
    pivot_longer(-all_of(nstep), names_to = "Period", values_to = "Abundance") %>%
    mutate(
      Period = as.integer(.data$Period) - 1,
      Year = period_to_year(.data$Period, nperiod_within_year = nperiod_within_year)
    )
}
