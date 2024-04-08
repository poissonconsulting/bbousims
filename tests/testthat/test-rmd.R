test_that("rmd example code works", {
  
  ### Survival and fecundity rates
  survival <- bbs_survival(intercept = logit(c(0.94, 0.98)),
                           trend = c(0, 0.3),
                           annual_sd = rep(0.05, 2),
                           period_sd = rep(0.1, 2),
                           nyear = 3, 
                           nperiod_within_year = 4)
  survival
 
  fecundity <- bbs_fecundity(intercept = c(NA, logit(0.4)),
                             trend = c(0, -0.2),
                             annual_sd = c(0, 0.1),
                             nyear = 3)
  fecundity
 
  survival_mat <- bbs_matrix_survival_period(survival)
  birth_mat <- bbs_matrix_birth_year(fecundity, female_recruit_stage = 1, male_recruit_stage = NULL)
  # first period, first year
  survival_mat[,,1,1]
  
  # first year
  birth_mat[,,1]
 
  age_mat <- bbs_matrix_age(c(2, 2))
  age_mat
 
  pop0 <- c(25, 52)
  survival_mat1 <- survival_mat[,,1,1]

  survival_mat1 %*% pop0
 
  survival_mat1 %*b% pop0
 
  population <- bbs_population(pop0, 
                               birth = birth_mat, 
                               age = age_mat, 
                               survival = survival_mat)
  population
  
  groups <- bbs_population_groups(population, group_size_lambda = 20, group_size_theta = 0)
  groups[[1]]
 
  groups <- bbs_population_groups_survey(population, group_size_lambda = 20, month_composition = 3L, group_coverage = 0.2)
  groups[[1]]
 
  set.seed(1)
  population <- bbs_population_caribou(nyear = 50, 
                                       survival_adult_female = 0.85,
                                       survival_calf_female = 0.5,
                                       calves_per_adult_female = 0.7)
  bbs_plot_population(population)
 
  set.seed(1)
  population <- bbs_population_caribou(nyear = 50, 
                                       survival_adult_female = 0.85,
                                       survival_calf_female = 0.5,
                                       calves_per_adult_female = 0.7, 
                                       survival_trend_adult_female = -0.01,
                                       survival_annual_sd_adult_female = 0.5)
  bbs_plot_population(population)
 
  data <- bbs_simulate_caribou(nyear = 30, 
                               adult_females = 500,
                               survival_adult_female = 0.85,
                               survival_calf_female = 0.5,
                               calves_per_adult_female = 0.7, 
                               survival_trend_calf_female = -0.01,
                               group_size = 15,
                               group_coverage = 0.3,
                               collared_adult_females = 30)
  
  data$survival
  
  data$recruitment
})
