test_that("bb_sims works", {
  nstage <- 6L
  nyear <- 5L
  nperiod_within_year <- 12L
  pop0 <- rep(1000, nstage)
  
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
    intercept = -0.8, 
    stage = c(NA, NA, -1.5, NA, 0, NA), 
    trend = -0.05,
    annual_sd = 0.1,
    nyear = 5)
  
  birth <- matrix_birth_year(fec, female_proportion = 0.6)
  
  age <- matrix_age(c(3, 4, 5, 6, 5, 6))
  
  set.seed(101)
  x <- simulate_population(pop0, birth = birth, age = age, survival = survival)
  
})

test_that("bb_sims works", {
  nyear = 10
  month_composition = 9
  adults = 1000
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival = 0.985
  # survival_calves = 0.83
  calves_per_adult_female = 0.3
  collared_adult_females = 30
  groups_coverage = 0.1
  survival_trend = 0.1
  survival_annual_sd = 0.25
  survival_month_sd = 0.25
  survival_annual_month_sd = 0
  # survival_yearling_effect = 0.5
  # survival_male_effect = 0.1
  # survival_calf_effect = 0.1
  calves_per_adult_female_trend = 0.1
  calves_per_adult_female_annual_sd = 0.1
  probability_uncertain_mortality = 0
  # probability_uncertain_survivor = 0
  group_size = 5
  group_max_proportion = 0.2
  group_min_size = 2
  probability_unsexed_adult_female = 0
  probability_unsexed_adult_male = 0
  
  # no yearling calves
  fec <- fecundity_year(
    intercept = log(calves_per_adult_female),
    stage = c(NA, NA, NA, NA, 0, NA),
    trend = calves_per_adult_female_trend,
    annual_sd = calves_per_adult_female_annual_sd,
    nyear = nyear)
  
  phi <- survival_period(
    intercept = logit(survival),
    stage = rep(0, 6),
    trend = survival_trend,
    annual_sd = survival_annual_sd,
    period_sd = survival_month_sd,
    annual_period_sd = survival_annual_month_sd,
    nyear = nyear)
  
  # how to set initial population size?
  # John suggesting we just do female_adults
  adultf <- rbinom(1, adults, proportion_adult_female)
  adultm <- adults - adultf
  yearlingf <- rbinom(1, adults, proportion_yearling_female)
  yearlingm <- adults - yearlingf
  calff <- rbinom(1, adults, proportion_yearling_female)
  calfm <- adults - calff
  pop0 <- c(calff, calfm, yearlingf, yearlingm, adultf, adultm)
  
  survival_matrices <- matrix_survival_period(phi)
  birth_matrices <- matrix_birth_year(fec)
  age_matrix <- matrix_age()
  
  population <- simulate_population(pop0, 
                                    birth = birth_matrices, 
                                    survival = survival_matrices,
                                    age = age_matrix)
  
  nstep <- ncol(population)
  composition_ind <- month_composition + 1
  survey <- c(composition_ind, composition_ind + 12 * 1:nyear)
  survey <- survey[survey <= nstep]
  group_size_lambda_year <- rep(group_size, nyear)
  group_size_theta_year <- rep(0, nyear)
  groups_coverage_year <- rep(groups_coverage, nyear)
  
  groups <- purrr::map(seq_along(survey), ~ {
    pop <- population[,survey[.x]]
    y <- population1_groups(population = pop,
                            group_size_lambda = group_size_lambda_year[.x],
                            group_size_theta = group_size_theta_year[.x],
                            group_max_proportion = group_max_proportion,
                            group_min_size = group_min_size)
    sample(y, round(groups_coverage_year[.x] * length(y)))
  })
  
  abundance <- abundance_tbl(population)
  
  probability_unsexed_adult_male_year <- rep(probability_unsexed_adult_male, nyear)
  probability_unsexed_adult_female_year <- rep(probability_unsexed_adult_female, nyear)
  recruitment <- recruitment_tbl(groups, 
                                 month_composition = month_composition,
                                 probability_unsexed_adult_male_year = probability_unsexed_adult_male_year,
                                 probability_unsexed_adult_female_year = probability_unsexed_adult_female_year)
  
  ### survival of collared female adults
  # possible for user to add new collars?
  # what is probability_uncertain_survival?
  # only subtract certain morts from starttotal?
  survival_female_adults_month_year <- phi[,,5]
  probability_uncertain_mortality_month_year <- matrix(probability_uncertain_mortality, 12, nyear)
  starttotal <- collared_adult_females
  yearmon <- tidyr::expand_grid(year = 1:nyear, month = 1:12)
  survival <- purrr::map_df(1:nrow(yearmon), ~ {
    month <- yearmon$month[.x]
    year <- yearmon$year[.x]
    phi <- survival_female_adults_month_year[month, year]
    prob_uncertain <- probability_uncertain_mortality_month_year[month, year]
    last <- starttotal[length(starttotal)]
    dead <- rbinom(1, last, (1 - phi))
    dead_uncertain <- rbinom(1, dead, prob_uncertain)
    dead_certain <- dead - dead_uncertain
    update <- last - dead_certain
    starttotal <<- c(starttotal, update)
    tibble::tibble(Year = year, 
                   Month = month,
                   StartTotal = last,
                   MortalitiesCertain = dead_certain,
                   MortalitiesUncertain = dead_uncertain)
  })
  
# simulate population a ---------------------------------------------------
#   library(bboutools)
# fit_rec <- bboutools::bb_fit_recruitment(data = bboudata::bbourecruit_a, adult_female_proportion = 0.65, year_trend = TRUE)
# fit_surv <- bboutools::bb_fit_survival(data = bboudata::bbousurv_a, year_trend = TRUE)
library(bboutools)
popa <- bb_sims(nyear = 10, month_composition = 9L, adults = 2000, 
                proportion_adult_female = 0.65, 
                survival = ilogit(4.5), 
                survival_annual_sd = 0.35, 
                survival_month_sd = 0.25,
                survival_annual_month_sd = 0, 
                survival_trend = 0.5, 
                calves_per_adult_female = exp(-1.6), 
                calves_per_adult_female_trend = 0.5,
                calves_per_adult_female_annual_sd = 0.25,
                collared_adult_females = 50, 
                group_size = 6, 
                group_min_size = 2, 
                group_max_proportion = 0.1,
                groups_coverage = 0.2)

fit_r <- bboutools::bb_fit_recruitment(popa$recruitment, 
                                       adult_female_proportion = NULL, 
                                       yearling_female_proportion = 0.5, 
                                       year_trend = TRUE)

View(coef(fit_r))

fit_s <- bboutools::bb_fit_survival(popa$survival, 
                                       year_trend = TRUE)

View(coef(fit_s))
  
})

