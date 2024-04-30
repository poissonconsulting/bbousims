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

test_that("assign population to groups", {
  withr::with_seed(10, {
    survival <- bbs_survival_caribou(0.84)
    fecundity <- bbs_fecundity_caribou(0.2)
    population <- bbs_population_caribou(survival = survival, fecundity = fecundity, adult_females = 1000)

    min_size <- 2
    max_proportion <- 0.75
    lambda <- 6
    theta <- 1
    group <- bbs_population_groups(
      population,
      group_size_lambda = lambda,
      group_size_theta = theta,
      group_max_proportion = max_proportion,
      group_min_size = min_size
    )
    expect_snapshot({
      print(group)
    })
  })

  nstep <- ncol(population)
  expect_identical(length(group), nstep)

  # check same individuals as in population for each period when unlist groups
  individuals <-
    purrr::map(seq_len(ncol(population)), ~ sort(population_individuals(population[, .x])))
  for (i in seq_along(individuals)) {
    expect_identical(individuals[[i]], sort(unlist(group[[i]])))
  }

  # check min and max group sizes
  sizes <- lapply(group, function(x) {
    sapply(x, length)
  })
  expect_true(min(unlist(sizes)) >= min_size)
  totals <- lapply(sizes, function(x) {
    sum(x) * max_proportion
  })
  for (i in seq_along(totals)) {
    expect_true(all(sizes[[i]] <= totals[[i]]))
  }
})

test_that("sample groups from population", {
  withr::with_seed(10, {
    survival <- bbs_survival_caribou(0.84)
    fecundity <- bbs_fecundity_caribou(0.2)
    population <- bbs_population_caribou(survival = survival, fecundity = fecundity, adult_females = 1000)

    min_size <- 2
    max_proportion <- 0.75
    lambda <- 6
    theta <- 1
    group <- bbs_population_groups_survey(
      population,
      group_size_lambda = lambda,
      group_size_theta = theta,
      group_max_proportion = max_proportion,
      group_min_size = min_size
    )

    expect_snapshot({
      print(group)
    })
  })

  nstep <- ncol(population)
  expect_equal(length(group), (nstep - 1) / 12)

  sizes <- lapply(group, function(x) {
    sapply(x, length)
  })
  expect_true(min(unlist(sizes)) >= min_size)
  totals <- lapply(sizes, function(x) {
    sum(x) * max_proportion
  })
  for (i in seq_along(totals)) {
    expect_true(all(sizes[[i]] <= totals[[i]]))
  }
})

test_that("assign population to groups by pairs", {
  withr::with_seed(10, {
    survival <- bbs_survival_caribou(0.84)
    fecundity <- bbs_fecundity_caribou(0.2)
    population <- bbs_population_caribou(survival = survival, fecundity = fecundity, adult_females = 1000)

    min_size <- 2
    max_proportion <- 0.75
    lambda <- 6
    theta <- 1
    group <- bbs_population_groups_pairs(
      population,
      group_size_lambda = lambda,
      group_size_theta = theta,
      group_max_proportion = max_proportion,
      group_min_size = min_size
    )

    expect_snapshot({
      print(group)
    })
  })

  nstep <- ncol(population)

  individuals <-
    purrr::map(seq_len(ncol(population)), ~ sort(population_individuals(population[, .x])))
  for (i in seq_along(individuals)) {
    expect_identical(individuals[[i]], sort(unlist(group[[i]])))
  }

  sizes <- lapply(group, function(x) {
    sapply(x, length)
  })
  expect_true(min(unlist(sizes)) >= min_size)
  totals <- lapply(sizes, function(x) {
    sum(x) * max_proportion
  })
  for (i in seq_along(totals)) {
    expect_true(all(sizes[[i]] <= totals[[i]]))
  }
})

test_that("assign population to groups in declining population to 0", {
  withr::with_seed(101, {
    survival <- bbs_survival_caribou(0.84)
    fecundity <- bbs_fecundity_caribou(0.2)
    population <- bbs_population_caribou(survival = survival, fecundity = fecundity, adult_females = 1000)
    min_size <- 2
    max_proportion <- 1
    lambda <- 6
    theta <- 1
    group <- bbs_population_groups(
      population,
      group_size_lambda = lambda,
      group_size_theta = theta,
      group_max_proportion = max_proportion,
      group_min_size = min_size
    )

    expect_snapshot({
      print(group)
    })
  })

  nstep <- ncol(population)
  expect_identical(length(group), nstep)

  # check same individuals as in population for each period when unlist groups
  individuals <-
    purrr::map(seq_len(ncol(population)), ~ sort(population_individuals(population[, .x])))
  for (i in seq_along(individuals)) {
    expect_identical(individuals[[i]], sort(unlist(group[[i]])))
  }

  # check min and max group sizes
  sizes <- lapply(group, function(x) {
    sapply(x, length)
  })
  expect_true(min(unlist(sizes)) >= min_size | min(unlist(sizes)) == 0)
})
