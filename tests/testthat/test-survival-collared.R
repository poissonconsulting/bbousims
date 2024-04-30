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

test_that("survival collared works", {
  withr::with_seed(10, {
    ncollar <- 30
    phi <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(ncollar,
      survival_adult_female_month_year = saf
    )
    expect_true(all(x$StartTotal[x$Month == 1] == ncollar))
    expect_snapshot({
      print(x)
    })
  })
})

test_that("survival collared works with different collar month", {
  withr::with_seed(10, {
    month_collar <- 3
    ncollar <- 30
    phi <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(ncollar,
      month_collar = month_collar,
      survival_adult_female_month_year = saf
    )
    expect_true(all(x$StartTotal[x$Month == month_collar] == ncollar))
    expect_snapshot({
      print(x)
    })
  })
})

test_that("survival collared works with uncertain mort", {
  withr::with_seed(10, {
    phi <- bbs_survival_caribou(0.84, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(30,
      survival_adult_female_month_year = saf,
      probability_uncertain_mortality = 0.5
    )
    expect_snapshot({
      print(x)
    })
  })
})

test_that("survival collared low survival works", {
  withr::with_seed(10, {
    phi <- bbs_survival_caribou(0.1, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(30,
      survival_adult_female_month_year = saf,
      probability_uncertain_mortality = 0.05
    )
    expect_snapshot({
      print(x)
    })
  })
})

test_that("survival collared low collars works", {
  withr::with_seed(10, {
    phi <- bbs_survival_caribou(0.1, survival_calf_female = 0.5)
    saf <- phi$eSurvival[, , 3]
    x <- bbs_survival_collared(5,
      survival_adult_female_month_year = saf,
      probability_uncertain_mortality = 0.05
    )
    expect_snapshot({
      print(x)
    })
  })
})
