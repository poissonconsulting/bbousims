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

test_that("birth process matrix works", {
  rates <- c(0, 0, 0.2)
  x <- bbs_matrix_birth(rates)
  expect_snapshot({
    print(x)
  })

  expect_identical(dim(x), c(3L, 3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[, 1], c(110, 100, 100))
})

test_that("birth process matrix can be matrix multiplied", {
  rates <- c(0, 0, 0.2)
  x <- bbs_matrix_birth(rates)
  y <- x %*% c(100, 100, 100)
  expect_snapshot({
    print(y)
  })
})

test_that("birth process matrix with male recruit works", {
  rates <- c(0, 0, 0.4)
  x <- bbs_matrix_birth(rates, male_recruit_stage = 2L)
  expect_snapshot({
    print(x)
  })

  expect_identical(dim(x), c(3L, 3L))
  y <- x %*% c(100, 100, 100)
  expect_identical(y[, 1], c(120, 120, 100))
})

test_that("can change female proportion", {
  rates <- c(0, 0, 0.4)
  x <- bbs_matrix_birth(rates, proportion_female = 0.75)
  expect_snapshot({
    print(x)
  })
})

test_that("can change calf stage indices", {
  rates <- c(0.4, 0, 0)
  x <- bbs_matrix_birth(rates, male_recruit_stage = 3, female_recruit_stage = 2, proportion_female = 0.75)
  expect_snapshot({
    print(x)
  })
})

test_that("birth process matrices work", {
  # 4 seasons, 2 years, 2 stages
  rates <- matrix(c(
    0, 0, 0.2,
    0, 0, 0.3
  ), nrow = 2, byrow = TRUE)
  x <- bbs_matrix_birth_year(rates)
  expect_identical(dim(x), c(3L, 3L, 2L))
  expect_snapshot({
    print(x)
  })
})

test_that("works with bbs_fecundity", {
  b0 <- 0.4
  rates <- bbs_fecundity(intercept = c(NA, logit(b0)), nyear = 5)
  x <- bbs_matrix_birth_year(rates$eFecundity)
  expect_identical(dim(x), c(2L, 2L, 5L))
  expect_snapshot({
    print(x)
  })
})

test_that("works with bbs_fecundity_caribou", {
  b0 <- 0.8
  rates <- bbs_fecundity_caribou(
    calves_per_adult_female = b0,
    trend = 0.2,
    nyear = 2
  )
  x <- bbs_matrix_birth_year(rates$eFecundity)
  expect_snapshot({
    print(x)
  })
})
