# library(poispkgs)
# 
# # BAS model - First survival, then ageing, then birth
# # 3 states one sex, constant survival/recruitment --------------------------------------------------------
# nstate <- 3
# nyears <- 5
# 
# phi_calf <- 0.99
# phi_yearling <- 0.99
# phi_adult <- 0.98
# 
# fec_yearling <- 0.2
# fec_adult <- 0.2
# 
# birth_mat <- matrix(c(1, fec_yearling, fec_adult,
#                       0, 1, 0,
#                       0, 0, 1), ncol = nstate, byrow = TRUE)
# 
# age_mat <- matrix(c(0, 0, 0,
#                     1, 0, 0,
#                     0, 1, 1), ncol = nstate, byrow = TRUE)
# 
# survival_mat <- matrix(c(phi_calf, 0, 0,
#                          0, phi_yearling, 0,
#                          0, 0, phi_adult), ncol = nstate, byrow = TRUE)
# 
# # initial abundance
# population0 <- c(150, 200, 600)
# 
# nmonths <- nyears*12
# population <- matrix(NA, ncol = nmonths+1, nrow = 3)
# population[,1] <- population0
# 
# birth_mat %*% population[,1]
# 
# for(i in 1:nmonths){
#   # last month of year
#   if(i %% 12 == 0){
#     population[, i+1] <- birth_mat %*% age_mat %*% survival_mat %*% population[,i]
#   } else {
#     population[, i+1] <- survival_mat %*% population[,i]
#   }
# }
# 
# population
# 
# gp <- plot_population(population, states = c("calf", "yearling", "adult"))
# 
# sbf_open_window(6)
# sbf_print(gp)
# 
# # 5 states two sexes -------------------------------------------------------------
# # calves have 1 survival rate, otherwise yearling and adult have separate survival by sex
# # female yearlings and adults have separate fecundity rates
# # sex is assigned to each calf after birth based on ratio
# # state 1 = calf
# # state 2 = yearling female
# # state 3 = yearling male
# # state 4 = adult female
# # state 5 = adult male
# 
# nstate <- 5
# nyears <- 5
# 
# calf_female_ratio <- 0.5
# phi_calf <- 0.97
# phi_yearling_female <- 0.99
# phi_yearling_male <- 0.98
# phi_adult_female <- 0.96
# phi_adult_male <- 0.95
# 
# fec_yearling_female <- 0.1
# fec_adult_female <- 0.25
# 
# birth_mat <- matrix(c(0, fec_yearling_female, 0, fec_adult_female, 0,
#                       0, 1, 0, 0, 0,
#                       0, 0, 1, 0, 0,
#                       0, 0, 0, 1, 0,
#                       0, 0, 0, 0, 1), ncol = nstate, byrow = TRUE)
# 
# age_mat <- matrix(c(0, 0, 0, 0, 0,
#                     calf_female_ratio, 0, 0, 0, 0,
#                     (1 - calf_female_ratio), 0, 0, 0, 0,
#                     0, 1, 0, 1, 0,
#                     0, 0, 1, 0, 1), ncol = nstate, byrow = TRUE)
# 
# survival_mat <- matrix(c(phi_calf, 0, 0, 0, 0,
#                          0, phi_yearling_female, 0, 0, 0,
#                          0, 0, phi_yearling_male, 0, 0,
#                          0, 0, 0, phi_adult_female, 0,
#                          0, 0, 0, 0, phi_adult_male), ncol = nstate, byrow = TRUE)
# 
# # initial abundance
# population0 <- c(200, 100, 100, 100, 100)
# 
# nmonths <- nyears*12
# population <- matrix(NA, ncol = nmonths+1, nrow = 5)
# population[,1] <- population0
# 
# survival_mat %*% population[,1]
# 
# for(i in 1:nmonths){
#   # last month of year
#   if(i %% 12 == 0){
#     population[, i+1] <- birth_mat %*% age_mat %*% survival_mat %*% population[,i]
#   } else {
#     population[, i+1] <- survival_mat %*% population[,i]
#   }
# }
# 
# population
# 
# gp <- plot_population(population, states = c("calf", "yearling female",
#                                              "yearling male", "adult female",
#                                              "adult male"))
# 
# sbf_open_window(6)
# sbf_print(gp)
# 
# # 5 states, annual variation ----------------------------------------------
# nstate <- 5
# nyears <- 5
# 
# calf_female_ratio <- 0.5
# 
# # annual fecundity variation (vector)
# fec_yearling_female <- runif(5, min = 0.08, max = 0.15)
# fec_adult_female <- runif(5, min = 0.2, max = 0.3)
# 
# # monthly and annual survival (matrix with 12 rows and nyear columns)
# phi_calf <- rbeta(5, shape1 = 97, shape2 = 3)
# phi_calf <- matrix(rep(phi_calf, 12), nrow = 12, byrow = TRUE)
# phi_yearling_female <- rbeta(5, shape1 = 99, shape2 = 1)
# phi_yearling_female <- matrix(rep(phi_yearling_female, 12), nrow = 12, byrow = TRUE)
# phi_yearling_male <- rbeta(5, shape1 = 98, shape2 = 2)
# phi_yearling_male <- matrix(rep(phi_yearling_male, 12), nrow = 12, byrow = TRUE)
# phi_adult_female <- rbeta(5, shape1 = 96, shape2 = 4)
# phi_adult_female <- matrix(rep(phi_adult_female, 12), nrow = 12, byrow = TRUE)
# phi_adult_male <- rbeta(5, shape1 = 95, shape2 = 5)
# phi_adult_male <- matrix(rep(phi_adult_male, 12), nrow = 12, byrow = TRUE)
# 
# # creates a list of birth matrices for each year
# # yearling_female and adult_female are vectors of annual rates
# fecundity_matrices <- function(yearling_female, adult_female){
#   chk_identical(length(yearling_female), length(adult_female))
#   map(seq_along(yearling_female), function(x){
#     matrix(c(0, yearling_female[x], 0, adult_female[x], 0,
#              0, 1, 0, 0, 0,
#              0, 0, 1, 0, 0,
#              0, 0, 0, 1, 0,
#              0, 0, 0, 0, 1), ncol = 5, byrow = TRUE)
#   })
# }
# 
# # creates a list of birth matrices for each year
# # yearling_female and adult_female are vectors of annual rates
# fecundity_matrices <- function(yearling_female, adult_female){
#   chk_identical(length(yearling_female), length(adult_female))
#   map(seq_along(yearling_female), function(x){
#     matrix(c(0, yearling_female[x], 0, adult_female[x], 0,
#              0, 1, 0, 0, 0,
#              0, 0, 1, 0, 0,
#              0, 0, 0, 1, 0,
#              0, 0, 0, 0, 1), ncol = 5, byrow = TRUE)
#   })
# }
# 
# # create list of lists of survival matrices for each period, i.e. list[[1]][[2]] is second month in first year
# # each argument is a matrix of month x year survival (always 12 rows)
# survival_matrices <- function(calf, yearling_female, yearling_male, adult_female, adult_male){
#   map(1:ncol(calf), function(x){
#     map(seq_along(calf[,x]), function(y){
#       matrix(c(calf[y, x], 0, 0, 0, 0,
#                0, yearling_female[y, x], 0, 0, 0,
#                0, 0, yearling_male[y, x], 0, 0,
#                0, 0, 0, adult_female[y, x], 0,
#                0, 0, 0, 0, adult_male[y, x]), ncol = 5, byrow = TRUE)
#     })
#   })
# }
# 
# age_matrices <- function(calf_female_ratio, nyears){
#   map(1:nyears, function(x){
#     matrix(c(0, 0, 0, 0, 0,
#              calf_female_ratio, 0, 0, 0, 0,
#              (1 - calf_female_ratio), 0, 0, 0, 0,
#              0, 1, 0, 1, 0,
#              0, 0, 1, 0, 1), ncol = nstate, byrow = TRUE)
#   })
# }
# 
# birth_mats <- fecundity_matrices(fec_yearling_female, fec_adult_female)
# survival_mats <- survival_matrices(phi_calf, phi_yearling_female, phi_yearling_male,
#                                    phi_adult_female, phi_adult_male)
# age_mats <- age_matrices(0.5, 5)
# 
# # initial abundance
# population0 <- c(100, 100, 100, 200, 200)
# 
# nyears <- 5
# nperiods <- nyears*12
# population <- matrix(NA, ncol = nperiods+1, nrow = 5)
# population[,1] <- population0
# 
# for(year in 1:nyears){
#   for(month in 1:12){
#     period <- yearmon_period(year, month)
#     if(month == 12){
#       population[, period+1] <- birth_mats[[year]] %*% age_mats[[year]] %*% survival_mats[[year]][[month]] %*% population[,period]
#     } else {
#       population[, period+1] <- survival_mats[[year]][[month]] %*% population[,period]
#     }
#   }
# }
# 
# population
# 
# gp <- plot_population(population, states = c("calf", "yearling female",
#                                              "yearling male", "adult female",
#                                              "adult male"))
# 
# sbf_open_window(6)
# sbf_print(gp)