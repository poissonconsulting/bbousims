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
  nyear = 20
  month_composition = 9
  adult_females = 1000
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_adult_female = 0.985
  survival_calf = 0.985
  calves_per_adult_female = 0.3
  collared_adult_females = 30
  survival_trend = 0.5
  survival_annual_sd = 0
  survival_month_sd = 0
  survival_annual_month_sd = 0
  probability_uncertain_mortality = 0
  calves_per_adult_female_trend = 0
  calves_per_adult_female_annual_sd = 0
  probability_uncertain_survivor = 0
  group_size = 5
  group_max_proportion = 0.2
  group_min_size = 2
  groups_coverage = 0.1
  probability_unsexed_adult_female = 0
  probability_unsexed_adult_male = 0
  
  adults_female = 1000
  calves_per_adult_female = 0.9
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_adult_female = 0.85
  survival_calf = 0.5
  survival_yearling = survival_adult_female
  
  dem <- bb_demographic_summary(calves_per_adult_female = calves_per_adult_female,
                                proportion_adult_female = proportion_adult_female,
                                proportion_yearling_female = proportion_yearling_female,
                                survival_adult_female = survival_adult_female,
                                survival_calf = survival_calf,
                                survival_yearling = survival_yearling)
 
  
  pop0 <- 
  
 
  
  n_af <- adult_females
  n <- n_af / age_dist[3]
  n_yf <- n * age_dist[2]
  n_cf <- n * age_dist[1]
  n_am <- n_af/proportion_adult_female - n_af
  n_ym <- n_yf/proportion_yearling_female - n_yf
  # assuming proportion yearling female same as proportion calf female
  n_cm <- n_cf/proportion_yearling_female - n_cf
  
  pop0 <- round(c(n_cf, n_cm, 
            n_yf, n_ym, 
            n_af, n_am))
  
  # no yearling calves
  # matrix fecundity rates year x stage
  fec <- fecundity_year(
    intercept = log(calves_per_adult_female),
    stage = c(NA, NA, NA, NA, 0, NA),
    trend = calves_per_adult_female_trend,
    annual_sd = calves_per_adult_female_annual_sd,
    nyear = nyear)
  
  # array survival rates, month x year x stage
  phi <- survival_period(
    intercept = logit(survival_adult_female),
    stage = rep(0, 6),
    trend = survival_trend,
    annual_sd = survival_annual_sd,
    period_sd = survival_month_sd,
    annual_period_sd = survival_annual_month_sd,
    nyear = nyear)
  
  survival_matrices <- matrix_survival_period(phi)
  birth_matrices <- matrix_birth_year(fec)
  age_matrix <- matrix_age()
  
  # survival then ageing then birth (BAS model)
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
  
  # list length year with list of groups in each year
  # groups sizes drawn from gamma-poisson dist
  # get group is composition month surveys and sample based on coverage
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

fit_r <- bboutools::bb_fit_recruitment(recruitment, 
                                       adult_female_proportion = NULL, 
                                       yearling_female_proportion = 0.5, 
                                       year_trend = TRUE)

View(coef(fit_r, include_random = FALSE))

survival$PopulationName <- "A"
fit_s <- bboutools::bb_fit_survival(survival, 
                                       year_trend = TRUE)

View(coef(fit_s, include_random = FALSE))
  
})

test_that("simulate_population works", {
  nyear = 20
  month_composition = 9
  adult_females = 1000
  proportion_adult_female = 0.65
  proportion_yearling_female = 0.5
  survival_adult_female = 0.985
  survival_calf = 0.5
  calves_per_adult_female = 0.9
  collared_adult_females = 30
  survival_trend = 0.5
  survival_annual_sd = 0
  survival_month_sd = 0
  survival_annual_month_sd = 0
  probability_uncertain_mortality = 0
  # survival_yearling_effect = 0.5
  # survival_male_effect = 0.1
  # survival_calf_effect = 0.1
  calves_per_adult_female_trend = 0
  calves_per_adult_female_annual_sd = 0
  # probability_uncertain_survivor = 0
  group_size = 5
  group_max_proportion = 0.2
  group_min_size = 2
  groups_coverage = 0.1
  probability_unsexed_adult_female = 0
  probability_unsexed_adult_male = 0
  
  # with 0 trend population should remain stable
  # proportion female should remain the same? - need to adjust adult male survival?
  
 pop <- simulate_population2(nyear = 20,
                             adult_females = 1000,
                             proportion_adult_female = 0.65,
                             proportion_yearling_female = 0.5,
                             survival_adult_female = 0.985,
                             survival_calf = 0.985,
                             calves_per_adult_female = 0.3,
                             survival_trend = 0,
                             survival_annual_sd = 0,
                             survival_month_sd = 0,
                             survival_annual_month_sd = 0,
                             calves_per_adult_female_trend = 0,
                             calves_per_adult_female_annual_sd = 0)
 plot_population(pop)
})

