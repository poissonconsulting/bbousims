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

test_that("demographic summary works", {
  calves_per_adult_female <- 0.9
  proportion_female <- 0.5
  survival_adult_female <- 0.85
  survival_calf <- 0.5
  survival_yearling <- survival_adult_female

  x <- bbs_demographic_summary(
    proportion_female = proportion_female,
    calves_per_adult_female = calves_per_adult_female,
    survival_adult_female = survival_adult_female,
    survival_calf = survival_calf,
    survival_yearling = survival_yearling
  )
  expect_snapshot({
    print(x)
  })
})

test_that("initial population works", {
  calves_per_adult_female <- 0.9
  proportion_female <- 0.5
  survival_adult_female <- 0.85
  survival_calf <- 0.5
  survival_yearling <- survival_adult_female

  ss <- stable_stage_distribution(
    calves_per_adult_female = calves_per_adult_female,
    proportion_female = proportion_female,
    survival_adult_female = survival_adult_female,
    survival_calf = survival_calf,
    survival_yearling = survival_yearling
  )

  x <- initial_population(1000, stable_stage_dist = ss)
  expect_snapshot({
    print(x)
  })
})
