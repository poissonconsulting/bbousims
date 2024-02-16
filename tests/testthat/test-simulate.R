test_that("simulating population with constant survival/fecundity rates works", {
  nperiod <- 10L
  nstage <- 6L
  nsims <- 10L
  pop0 <- rep(100, nstage)
  # varying survival rate by stage
  survival <- matrix_survival(rbeta(nstage, 84, 16))
  age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  birth <- matrix_birth(c(0, 0, 0.2, 0, 0.25, 0))
  
  x <- simulate_population(pop0, birth = birth, age = age, survival = survival, nsims = nsims)
  
  expect_s3_class(x, "nlists")
  expect_length(x, 10L)
  expect_identical(dim(x[[1]]$abundance), c(nstage, (nperiod + 1L)))
})

test_that("simulating population with varying survival/fecundity rates works", {
  nstage <- 6L
  nsims <- 10L
  pop0 <- rep(100, nstage)
  
  survival_rates <- lapply(c(-0.01, 0.01, 0, -0.02, 0.01, 0.01), function(x){
    c(rep(0.98, 4), rep(0.97, 4), rep(0.96, 4)) + x
  })
  survival_rates <- array(unlist(survival_rates), dim = c(4, 3, 6))
  
  survival <- matrix_survival_period(survival_rates)
  
  birth_rates <- matrix(c(0, 0, 0.2, 0, 0.3, 0,
                          0, 0,  0.3, 0, 0.35, 0,
                          0, 0, 0.2, 0, 0.3, 0), ncol = 6, byrow = TRUE)
  birth <- matrix_birth_year(birth_rates)
  
  age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  
  x <- simulate_population_period(pop0, birth = birth, age = age, survival = survival, nsims = nsims)
  
  expect_s3_class(x, "nlists")
  expect_length(x, nsims)
  expect_identical(dim(x[[1]]$abundance), c(nstage, (dim(survival)[3] * dim(survival)[4] + 1L)))
})
