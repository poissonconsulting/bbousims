test_that("multiplication works", {
  # ensure same calf assigment in demographic stochasticity
  # vector of 1/0 to indicate whether ages or remains
  
  # # 6 states two sexes, constant rates -------------------------------------------------------------
  # # female yearlings and adults have separate fecundity rates
  # # sex is assigned to each calf state after birth based on sex ratio
  # # state 1 = calf female
  # # state 2 = calf male
  # # state 2 = yearling female
  # # state 3 = yearling male
  # # state 4 = adult female
  # # state 5 = adult male
  # #
  # # # annual survival - constant rates
  # 
  # population0 <- c(100, 100, 100, 100, 200, 200)
  # # varying survival rate by state
  # survival <- matrix_survival(c(0.84, 0.84, 0.86, 0.86, 0.88, 0.87))
  # survival
  # age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  # age
  # # female yearling fecundity 0.2, adult fecundity 0.25
  # birth <- matrix_birth(c(0, 0, 0.2, 0, 0.25, 0), female_proportion = 0.6)
  # birth
  # 
  # survival %*b% population0
  # age %*% population0
  # birth %*% population0
  # 
  # population2 <- project_population_bas(population0, survival = survival, age = age, birth = birth, nperiod = 10)
  # population
  # population2
  # # gp <- plot_population(population, states = c("calf", "yearling female", "yearling male", "adult female", "adult male"))
  # # sbf_open_window()
  # # sbf_print(gp)
  # 
  # # 5 states, varying rates  ------------------------------------------------
  # # survival varies by month, year and state
  # # birth varies by year and state
  # # age varies by year (via sex ratio)
  # 
  # # array of survival rates by year, period, state
  # # no monthly variation here for simplicity
  # survival_rates <- lapply(c(-0.01, 0.01, 0, -0.02, 0.01, 0.01), function(x){
  #   rep(c(0.98, 0.97, 0.96), 4) + x
  # })
  # survival_rates <- array(unlist(survival_rates), dim = c(3, 4, 6))
  # 
  # survival <- matrix_survival_period(survival_rates)
  # 
  # birth_rates <- matrix(c(0, 0, 0.2, 0, 0.3, 0,
  #                         0, 0,  0.3, 0, 0.35, 0,
  #                         0, 0, 0.2, 0, 0.3, 0), ncol = 6, byrow = TRUE)
  # birth <- matrix_birth_year(birth_rates)
  # 
  # age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  # 
  # # message("double check order of year,period in survival makes sense")
  # x <- project_population_bas_period(population0, survival = survival, birth = birth, age = age)
  # x

})
