test_that("assign population to groups", {
  nstage <- 6L
  nsims <- 10L
  nyear <- 5L
  nperiod_within_year <- 12L
  pop0 <- rep(100, 6)
  nstep <- as.integer(nyear * nperiod_within_year + 1)
  
  phi <- survival_period(
    intercept = 4.45,
    stage = c(-0.1, -0.1, 0, 0, 0.1, 0.1),
    trend = 0.1,
    annual_sd = 0.3,
    period_sd = 0.2,
    annual_period_sd = 0.1,
    nyear = nyear,
    nperiod_within_year = nperiod_within_year
  )
  
  survival <- matrix_survival_period(phi)
  
  fec <- fecundity_year(
    intercept = -1.3,
    stage = c(NA, NA, -0.1, NA, 0.05, NA),
    trend = -0.05,
    annual_sd = 0.1,
    nyear = 5
  )
  
  birth <- matrix_birth_year(fec, female_proportion = 0.6)
  
  age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  
  set.seed(101)
  x <-
    simulate_population(pop0,
                        birth = birth,
                        age = age,
                        survival = survival)
  
  # check different values ----
  min_size <- 2
  max_proportion <- 0.75
  lambda <- 6
  theta <- 1
  
  group <- population_groups(
    x,
    group_size_lambda = lambda,
    group_size_theta = theta,
    max_group_proportion = max_proportion,
    min_group_size = min_size
  )
  expect_true(is.list(group))
  expect_identical(length(group), nstep)
  expect_true(is.list(group[[1]]))
  
  individuals <-
    purrr::map(1:ncol(x), ~ sort(population_individuals(x[, .x])))
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

test_that("assign population to groups by pairs", {
  nstage <- 6L
  nsims <- 10L
  nyear <- 5L
  nperiod_within_year <- 12L
  pop0 <- rep(100, 6)
  nstep <- as.integer(nyear * nperiod_within_year + 1)
  
  phi <- survival_period(
    intercept = 4.45,
    stage = c(-0.1, -0.1, 0, 0, 0.1, 0.1),
    trend = 0.1,
    annual_sd = 0.3,
    period_sd = 0.2,
    annual_period_sd = 0.1,
    nyear = nyear,
    nperiod_within_year = nperiod_within_year
  )
  
  survival <- matrix_survival_period(phi)
  
  fec <- fecundity_year(
    intercept = -1.3,
    stage = c(NA, NA, -0.1, NA, 0.05, NA),
    trend = -0.05,
    annual_sd = 0.1,
    nyear = 5
  )
  
  birth <- matrix_birth_year(fec, female_proportion = 0.6)
  
  age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  
  set.seed(101)
  x <-
    simulate_population(pop0,
                        birth = birth,
                        age = age,
                        survival = survival)
  
  # check different values ----
  min_size <- 5
  max_proportion <- 0.5
  lambda <- 10
  theta <- 2
  
  group <- population_groups_pairs(
    x,
    group_size_lambda = lambda,
    group_size_theta = theta,
    max_group_proportion = max_proportion,
    min_group_size = min_size
  )
  expect_true(is.list(group))
  expect_identical(length(group), nstep)
  expect_true(is.list(group[[1]]))
  
  individuals <-
    purrr::map(1:ncol(x), ~ sort(population_individuals(x[, .x])))
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
