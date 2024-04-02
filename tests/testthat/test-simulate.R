test_that("bbs_simulate_caribou works", {
  x <- bbs_simulate_caribou(nyear = 10)
  expect_true(is.list(x))
  expect_identical(names(x), c("survival", "recruitment", "abundance"))
  expect_true(all(unlist(lapply(x, is.data.frame))))
  
  expect_equal(max(x$survival$Year), 10)
})

test_that("bbs_simulate_caribou works with bboutools", {
  survival_adult_female <- 0.85
  survival_trend <- -0.01
  nyear <- 30L
  
  x <- bbs_simulate_caribou(adult_females = 500, 
                            proportion_adult_female = 0.65, 
                            survival_adult_female = survival_adult_female,
                            survival_trend_adult_female = survival_trend, 
                            nyear = nyear,
                            group_coverage = 0.2, 
                            group_size = 15,
                            collared_adult_females = 30)
  
  fit_r <- bboutools::bb_fit_recruitment(x$recruitment, 
                                         adult_female_proportion = NULL, 
                                         yearling_female_proportion = 0.5, 
                                         year_trend = TRUE)
  
  View(coef(fit_r, include_random = FALSE))
  
  fit_s <- bboutools::bb_fit_survival(x$survival, 
                                      min_random_year = Inf,
                                      year_trend = TRUE, 
                                      nthin = 10L, year_start = 1L)

  
  # actual
  # sims trend is effect of one year change on monthly log-odds prob
  # bboutools trend is effect of scaled year change
  # sims intercept is first year
  # bboutools intercept is mean year
  x <- ilogit(logit(ilogit(logit(survival_adult_female^(1/12)) + survival_trend * 0:(nyear - 1))^12))
  year <- bb_predict_survival_trend(fit_s)
  year$actual <- x
  
  library(ggplot2)
  ggplot(data = year) +
    geom_ribbon(aes(x = CaribouYear, y = estimate, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(aes(x = CaribouYear, y = actual), color = "red") +
    geom_line(aes(x = CaribouYear, y = estimate), color = "black") 

})

test_that("bb_sims works", {
  x <- bb_sims(nyear = 10,
          month_composition = 9,
          adult_females = 1000,
          proportion_adult_female = 0.65,
          proportion_yearling_female = 0.5,
          survival_adult_female = 0.84,
          survival_calf = 0.5,
          calves_per_adult_female = 0.5,
          collared_adult_females = 30,
          month_collar = 3L,
          survival_trend = 0.2,
          survival_annual_sd = 0.1,
          survival_month_sd = 0.1,
          survival_annual_month_sd = 0.1,
          survival_yearling_effect = 0.05,
          calves_per_adult_female_trend = 0.1,
          calves_per_adult_female_annual_sd = 0.1,
          probability_uncertain_survivor = 0,
          probability_uncertain_mortality = 0,
          group_size = 5,
          group_coverage = 0.1,
          probability_unsexed_adult_female = 0,
          probability_unsexed_adult_male = 0)
  
  expect_true(is.list(x))
  expect_identical(names(x), c("survival", "recruitment", "abundance"))
  expect_true(all(unlist(lapply(x, is.data.frame))))
  
})

test_that("can recover values", {
# simulate population a ---------------------------------------------------
#   library(bboutools)
# fit_rec <- bboutools::bb_fit_recruitment(data = bboudata::bbourecruit_a, adult_female_proportion = 0.65, year_trend = TRUE)
# fit_surv <- bboutools::bb_fit_survival(data = bboudata::bbousurv_a, year_trend = TRUE)
library(bboutools)
set.seed(101)
x <- bb_sims(nyear = 10,
             month_composition = 9,
             adult_females = 2000,
             proportion_adult_female = 0.65,
             proportion_yearling_female = 0.5,
             survival_adult_female = 0.85,
             survival_calf = 0.85,
             calves_per_adult_female = 0.5,
             collared_adult_females = 30,
             month_collar = 1L,
             survival_trend = -0.1,
             survival_annual_sd = 0.2,
             survival_month_sd = 0.2,
             survival_annual_month_sd = 0.01,
             survival_yearling_effect = 0,
             calves_per_adult_female_trend = 0.1,
             calves_per_adult_female_annual_sd = 0.1,
             probability_uncertain_survivor = 0.01,
             probability_uncertain_mortality = 0.05,
             group_size = 15,
             group_coverage = 0.5,
             probability_unsexed_adult_female = 0,
             probability_unsexed_adult_male = 0)

fit_r <- bboutools::bb_fit_recruitment(x$recruitment, 
                                       adult_female_proportion = NULL, 
                                       yearling_female_proportion = 0.5, 
                                       year_trend = TRUE)

View(coef(fit_r, include_random = FALSE))

x$survival$PopulationName <- "A"
fit_s <- bboutools::bb_fit_survival(x$survival, 
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

