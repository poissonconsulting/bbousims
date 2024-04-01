test_that("simulating population base", {
  nyear = 10
  
  survival <- bbs_survival(intercept = logit(c(0.94, 0.98)),
                           trend = c(0, 0.3),
                           annual_sd = rep(0.05, 2),
                           period_sd = rep(0.1, 2),
                           nyear = nyear, 
                           nperiod_within_year = 4)
  
  fecundity <- bbs_fecundity(intercept = c(NA, logit(0.4)),
                             trend = c(0, -0.2),
                             annual_sd = c(0, 0.1),
                             nyear = nyear)
  
  survival_mat <- bbs_matrix_survival_period(survival)
  birth_mat <- bbs_matrix_birth_year(fecundity, female_recruit_stage = 1, male_recruit_stage = NULL)
  age_mat <- bbs_matrix_age(c(2, 2))
  
  pop0 <- c(100, 100)
  
  population <- bbs_population(pop0, 
                               birth = birth_mat, 
                               age = age_mat, 
                               survival = survival_mat)
 
  expect_equal(population[c(1, 2),1], pop0)
  expect_true(is.matrix(population))
  nstep <- as.integer(nyear*4 + 1)
  expect_equal(dim(population), c(2, nstep))
  expect_snapshot(population)
})

test_that("bb_simulate_population deterministic works", {
  population <- bbs_population_caribou(nyear = 50, 
                                       survival_adult_female = 0.85,
                                       survival_calf_female = 0.5,
                                       calves_per_adult_female = 0.72, 
                                       survival_trend_adult_female = 0,
                                       survival_annual_sd_adult_female = 0.2)
  
  expect_true(is.matrix(population))
  chk_whole_numeric(population)
  expect_snapshot(population)
})
