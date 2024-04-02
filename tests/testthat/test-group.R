test_that("assign population to groups", {
  set.seed(101)
  population <- bbs_population_caribou(nyear = 10, 
                                       adult_females = 1000)
  
  nstep <- ncol(population)
  # check different values ----
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
  expect_true(is.list(group))
  expect_identical(length(group), nstep)
  expect_true(is.list(group[[1]]))
  
  # check same individuals as in population for each period when unlist groups
  individuals <-
    purrr::map(1:ncol(population), ~ sort(population_individuals(population[, .x])))
  for (i in seq_along(individuals)) {
    expect_identical(individuals[[i]], sort(unlist(group[[i]])))
  }
  
  # check min and max group sizes
  sizes <- lapply(group, function(x)
    sapply(x, length))
  expect_true(min(unlist(sizes)) >= min_size)
  totals <- lapply(sizes, function(x)
    sum(x) * max_proportion)
  for (i in seq_along(totals)) {
    expect_true(all(sizes[[i]] <= totals[[i]]))
  }
})

test_that("sample groups from population", {
  set.seed(101)
  population <- bbs_population_caribou(nyear = 10, 
                                       adult_females = 1000)
  
  nstep <- ncol(population)
  # check different values ----
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
  expect_true(is.list(group))
  expect_equal(length(group), (nstep - 1)/12)
  expect_true(is.list(group[[1]]))
  
  sizes <- lapply(group, function(x)
    sapply(x, length))
  expect_true(min(unlist(sizes)) >= min_size)
  totals <- lapply(sizes, function(x)
    sum(x) * max_proportion)
  for (i in seq_along(totals)) {
    expect_true(all(sizes[[i]] <= totals[[i]]))
  }
})

test_that("assign population to groups by pairs", {
  set.seed(101)
  population <- bbs_population_caribou(nyear = 10, 
                                       adult_females = 1000)
  
  nstep <- ncol(population)
  # check different values ----
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
  expect_true(is.list(group))
  expect_identical(length(group), nstep)
  expect_true(is.list(group[[1]]))
  
  individuals <-
    purrr::map(1:ncol(population), ~ sort(population_individuals(population[, .x])))
  for (i in seq_along(individuals)) {
    expect_identical(individuals[[i]], sort(unlist(group[[i]])))
  }
  
  sizes <- lapply(group, function(x)
    sapply(x, length))
  expect_true(min(unlist(sizes)) >= min_size)
  totals <- lapply(sizes, function(x)
    sum(x) * max_proportion)
  for (i in seq_along(totals)) {
    expect_true(all(sizes[[i]] <= totals[[i]]))
  }
})

test_that("assign population to groups in declining population", {
  set.seed(101)
  population <- bbs_population_caribou(nyear = 10, 
                                       adult_females = 1000, 
                                       survival_trend_adult_female = -0.5)
  
  nstep <- ncol(population)
  # check different values ----
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
  expect_true(is.list(group))
  expect_identical(length(group), nstep)
  expect_true(is.list(group[[1]]))
  
  # check same individuals as in population for each period when unlist groups
  individuals <-
    purrr::map(1:ncol(population), ~ sort(population_individuals(population[, .x])))
  for (i in seq_along(individuals)) {
    expect_identical(individuals[[i]], sort(unlist(group[[i]])))
  }
  
  # check min and max group sizes
  sizes <- lapply(group, function(x)
    sapply(x, length))
  expect_true(min(unlist(sizes)) >= min_size | min(unlist(sizes)) == 0)
})
