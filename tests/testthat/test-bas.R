test_that("multiplication works", {
  # ensure same calf assigment in demographic stochasticity
  # vector of 1/0 to indicate whether ages or remains
  
  # # 5 states two sexes, constant rates -------------------------------------------------------------
  # # calves have 1 survival rate, otherwise yearling and adult have separate survival by sex
  # # female yearlings and adults have separate fecundity rates
  # # sex is assigned to each calf after birth based on ratio
  # # state 1 = calf
  # # state 2 = yearling female
  # # state 3 = yearling male
  # # state 4 = adult female
  # # state 5 = adult male
  #
  # # annual survival - constant rates
  
  population0 <- c(200, 100, 100, 200, 200)
  # varying survival rate by state
  survival <- survival_matrix(c(0.84, 0.86, 0.86, 0.88, 0.87))
  age <- age_matrix(0.5, 5)
  # female yearling fecundity 0.2, adult fecundity 0.25
  birth <- birth_matrix(c(0, 0.2, 0, 0.25, 0))
  
  survival %*% population0
  age %*% population0
  birth %*% population0
  
  population <- bas_population_growth(population0, survival = survival, age = age, birth = birth, nperiod = 5)
  population
  # gp <- plot_population(population, states = c("calf", "yearling female", "yearling male", "adult female", "adult male"))
  # sbf_open_window()
  # sbf_print(gp)
  
  # 5 states, varying rates  ------------------------------------------------
  # survival varies by month, year and state
  # birth varies by year and state
  # age varies by year (via sex ratio)
  
  # list of survival matrices for each period
  x1 <- matrix(c(rep(0.98, 12), rep(0.97, 12)), nrow = 12)
  x2 <- matrix(c(rep(0.97, 12), rep(0.96, 12)), nrow = 12)
  x3 <- matrix(c(rep(0.95, 12), rep(0.94, 12)), nrow = 12)
  x4 <- matrix(c(rep(0.99, 12), rep(0.99, 12)), nrow = 12)
  x5 <- matrix(c(rep(0.98, 12), rep(0.96, 12)), nrow = 12)
  
  survival_rates <- array(c(x1, x2, x3, x4, x5), dim = c(12, 2, 5))
  survival <- survival_year_month(survival_rates)
  
  birth_rates <- matrix(c(0, 0.2, 0, 0.3, 0,
                          0, 0.3, 0, 0.35, 0), ncol = 5, byrow = TRUE)
  birth <- birth_year(birth_rates)
  
  age <- age_year(rep(0.5, 2), nstate = 5)
  
  x <- bas_population_growth_period(population0, survival = survival, birth = birth, age = age)
  
  ageing - 1 if age to next class or 0 if dont 
  
})
