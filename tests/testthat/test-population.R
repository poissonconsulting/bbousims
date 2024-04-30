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

test_that("bbs_population works", {
  withr::with_seed(10, {
    nyear <- 10
    pop0 <- c(100, 100)
    survival <- bbs_survival(
      intercept = logit(c(0.94, 0.98)),
      trend = c(0, 0.3),
      annual_sd = rep(0.05, 2),
      period_sd = rep(0.1, 2),
      nyear = nyear,
      nperiod_within_year = 4
    )

    fecundity <- bbs_fecundity(
      intercept = c(NA, logit(0.4)),
      trend = c(0, -0.2),
      annual_sd = c(0, 0.1),
      nyear = nyear
    )
    survival_mat <- bbs_matrix_survival_period(survival$eSurvival)
    birth_mat <- bbs_matrix_birth_year(fecundity$eFecundity, female_recruit_stage = 1, male_recruit_stage = NULL)
    age_mat <- bbs_matrix_age(c(2, 2))
    x <- bbs_population(pop0,
      birth = birth_mat,
      age = age_mat,
      survival = survival_mat
    )

    expect_s3_class(x, "bbou_population")
    expect_equal(x[c(1, 2), 1], pop0)
    nstep <- as.integer(nyear * 4)
    expect_equal(dim(x), c(2, nstep + 1))
    expect_snapshot({
      print(x)
    })
  })
})

test_that("bbs_population_caribou works", {
  withr::with_seed(10, {
    nyear <- 10
    survival <- bbs_survival_caribou(
      survival_adult_female = 0.84,
      nyear = nyear
    )
    fecundity <- bbs_fecundity_caribou(0.2, nyear = nyear)

    x <- bbs_population_caribou(
      survival = survival,
      fecundity = fecundity
    )
    chk_whole_numeric(x)
    expect_snapshot({
      print(x)
    })
  })
})

test_that("bbs_population_caribou fails with different number of years", {
  withr::with_seed(10, {
    nyear <- 10
    survival <- bbs_survival_caribou(
      survival_adult_female = 0.84,
      nyear = nyear
    )
    fecundity <- bbs_fecundity_caribou(0.2, nyear = 3)
    
    expect_chk_error(bbs_population_caribou(
      survival = survival,
      fecundity = fecundity
    ), regexp = "must have the same number of years") 
  })
})
