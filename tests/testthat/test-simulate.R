test_that("simulating population with constant survival/fecundity rates works", {
  nperiod <- 10L
  nstage <- 6L
  nsims <- 10L
  pop0 <- rep(100, nstage)
  # varying survival rate by stage
  survival <- matrix_survival(rbeta(nstage, 84, 16))
  age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  birth <- matrix_birth(c(0, 0, 0.2, 0, 0.25, 0))
  
  x <- simulate_population_constant(pop0, birth = birth, age = age, survival = survival, nsims = nsims)
  
  expect_s3_class(x, "nlists")
  expect_length(x, 10L)
  expect_identical(dim(x[[1]]$abundance), c(nstage, (nperiod + 1L)))
})

test_that("simulating population with varying survival/fecundity rates works", {
  nstage <- 6L
  nsims <- 10L
  pop0 <- rep(100, nstage)
  
  phi <- survival_period(
    intercept = 4.45, 
    stage = c(-0.1, -0.1, 0, 0, 0.1, 0.1), 
    trend = 0.1,
    annual_sd = 0.3, 
    period_sd = 0.2,
    annual_period_sd = 0.1, 
    nyear = 5, 
    nperiod_within_year = 12)
  
  survival <- matrix_survival_period(phi)
  
  fec <- fecundity_year(
    intercept = -1.3, 
    stage = c(NA, NA, -0.1, NA, 0.05, NA), 
    trend = -0.05,
    annual_sd = 0.1,
    nyear = 5)
  
  birth <- matrix_birth_year(fec, female_proportion = 0.6)
  
  age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  
  x <- simulate_population(pop0, birth = birth, age = age, survival = survival, nsims = nsims)
  
  expect_s3_class(x, "nlists")
  expect_length(x, nsims)
  expect_identical(dim(x[[1]]$abundance), c(nstage, (dim(survival)[3] * dim(survival)[4] + 1L)))
})


