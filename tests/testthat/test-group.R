test_that("assign population to groups", {
  nstage <- 6L
  nsims <- 10L
  nyear <- 5L
  nperiod_within_year <- 12L
  pop0 <- rep(100, 6)
  nstep <- as.integer(nyear*nperiod_within_year+1)
  
  phi <- survival_period(
    intercept = 4.45, 
    stage = c(-0.1, -0.1, 0, 0, 0.1, 0.1), 
    trend = 0.1,
    annual_sd = 0.3, 
    period_sd = 0.2,
    annual_period_sd = 0.1, 
    nyear = nyear, 
    nperiod_within_year = nperiod_within_year)
  
  survival <- matrix_survival_period(phi)
  
  fec <- fecundity_year(
    intercept = -1.3, 
    stage = c(NA, NA, -0.1, NA, 0.05, NA), 
    trend = -0.05,
    annual_sd = 0.1,
    nyear = 5)
  
  birth <- matrix_birth_year(fec, female_proportion = 0.6)
  
  age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  
  set.seed(101)
  x <- simulate_population(pop0, birth = birth, age = age, survival = survival)
  
  population <- x[,5]
  set.seed(101)
  individuals <- population_individuals(population)
  
  min_size <- 2
  max_proportion <- 0.25
  lambda <- 5
  theta <- 1
  
  # random - no pairs ----
  for(i in 1:100){
    set.seed(i)
    group1 <- population1_groups(population, 
                                 group_size_lambda = lambda, 
                                 group_size_theta = theta, 
                                 max_group_proportion = max_proportion, 
                                 min_group_size = min_size)
    
    expect_true(is.list(group1))
    expect_identical(sort(individuals), sort(unlist(group1)))
    sizes <- sapply(group1, length)
    expect_true(min(sizes) >= min_size)
    expect_true(max(sizes) <= sum(population)*max_proportion)
  }
  
  # pairs ----
  for(i in 1:100){
    set.seed(i)
    # print(i)
    group1 <- population1_groups_pairs(population, 
                                 group_size_lambda = lambda, 
                                 group_size_theta = theta, 
                                 max_group_proportion = max_proportion, 
                                 min_group_size = min_size,
                                 recruit_stages = c(1, 2),
                                 reproductive_female_stages = c(3, 5))
    
    expect_true(is.list(group1))
    expect_identical(sort(individuals), sort(unlist(group1)))
    sizes <- sapply(group1, length)
    expect_true(min(sizes) >= min_size)
    expect_true(max(sizes) <= sum(population)*max_proportion)
  }
  
  # check different values ----
  min_size <- 10
  max_proportion <- 0.5
  lambda <- 20
  theta <- 2
  for(i in 1:100){
    set.seed(i)
    group1 <- population1_groups(population, 
                                 group_size_lambda = lambda, 
                                 group_size_theta = theta, 
                                 max_group_proportion = max_proportion, 
                                 min_group_size = min_size)
    
    expect_true(is.list(group1))
    expect_identical(sort(individuals), sort(unlist(group1)))
    sizes <- sapply(group1, length)
    expect_true(min(sizes) >= min_size)
    expect_true(max(sizes) <= sum(population)*max_proportion)
  }
  
  # all periods ----
  set.seed(101)
  group <- population_groups(x, 
                             group_size_lambda = lambda, 
                             group_size_theta = theta, 
                             max_group_proportion = max_proportion, 
                             min_group_size = min_size)
  expect_true(is.list(group))
  expect_identical(length(group), nstep)
  expect_true(is.list(group[[1]]))

  individuals <- purrr::map(1:ncol(x), ~ sort(population_individuals(x[,.x])))
  for(i in seq_along(individuals)){
    expect_identical(individuals[[i]], sort(unlist(group[[i]])))
  }
  
  # all periods pairs ----
  set.seed(101)
  group <- population_groups_pairs(x, 
                             group_size_lambda = lambda, 
                             group_size_theta = theta, 
                             max_group_proportion = max_proportion, 
                             min_group_size = min_size)
  expect_true(is.list(group))
  expect_identical(length(group), nstep)
  expect_true(is.list(group[[1]]))
  
  individuals <- purrr::map(1:ncol(x), ~ sort(population_individuals(x[,.x])))
  for(i in seq_along(individuals)){
    expect_identical(individuals[[i]], sort(unlist(group[[i]])))
  }
})
