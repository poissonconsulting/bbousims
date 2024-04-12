library(ggplot2)
library(bboutools)

set.seed(101)
# basics
nsims <- 1
month_collar = 3L
month_composition = 9L
nyear <- 10
# 90 observed, assume ~ 20% coverage?
adult_females = 500
group_coverage = 0.2
message("dig into example datasets")
group_size = 15

proportion_adult_female = 0.65
proportion_yearling_female = 0.5
probability_unsexed_adult_female = 0.03
probability_unsexed_adult_male = 0.03

survival_calf_female = 0.5
calves_per_adult_female = 0.7

probability_uncertain_survival = 0.01
probability_uncertain_mortality = 0.01

# 2 recruitment scenarios
# 2 sample size scenarios
# 4 simulations total
survival_adult_female <- c(0.6, 0.85)
survival_trend_adult_female <- 0
coverage <- 1:2
group_coverage <- c(0.1, 0.6)
collared_adult_females <- c(10, 50)
scenario <- tidyr::expand_grid(survival_adult_female,
                               survival_trend_adult_female,
                               coverage) %>%
  dplyr::mutate(group_coverage = dplyr::case_when(coverage == 1 ~ group_coverage[1],
                                                  coverage == 2 ~ group_coverage[2]),
                collared_adult_females = dplyr::case_when(coverage == 1 ~ collared_adult_females[1],
                                                          coverage == 2 ~ collared_adult_females[2]))

# actual
# sims trend is effect of one year change on monthly log-odds prob
# bboutools trend is effect of scaled year change
# sims intercept is first year
# bboutools intercept is mean year
actual <- ilogit(logit(ilogit(logit(survival_adult_female[2]^(1/12)) + survival_trend_adult_female * 0:(nyear - 1))^12))
purrr::map_dbl(actual, ~ {
  bbousims:::bbs_estimate_lambda(calves_per_adult_female = 0.8,
                                 survival_adult_female = .x,
                                 survival_calf = survival_calf_female,
                                 survival_yearling = .x,
                                 proportion_female = 0.5)
  
})

x <- purrr::map(1:nrow(scenario), ~ {
  saf <- scenario$survival_adult_female[.x]
  cov <- scenario$group_coverage[.x]
  collar <- scenario$collared_adult_females[.x]
  purrr::map(1:nsims, ~ {
    bbs_simulate_caribou(survival_adult_female = saf,
                         survival_trend_adult_female = survival_trend_adult_female,
                         collared_adult_females = collar,
                         group_coverage = cov,
                         group_size = group_size,
                         nyear = nyear,
                         month_composition = month_composition,
                         adult_females = adult_females,
                         proportion_adult_female = proportion_adult_female,
                         proportion_yearling_female = proportion_yearling_female,
                         month_collar = month_collar,
                         survival_calf_female = survival_calf_female,
                         survival_annual_sd_adult_female = 0.1,
                         survival_month_sd_adult_female = 0.05,
                         probability_uncertain_survival = probability_uncertain_survival,
                         probability_uncertain_mortality = probability_uncertain_mortality,
                         probability_unsexed_adult_female = probability_unsexed_adult_female,
                         probability_unsexed_adult_male = probability_unsexed_adult_male)
  })
})

# scenario 3 - 0.85 and low coverage  (0.1 group cvrg and 10 collars)
x1 <- x[[3]][[1]]
bbs_plot_population(x1)
library(bboutools)
fit1 <- bboutools::bb_fit_survival(x1$survival,
                                   year_trend = TRUE,
                                   nthin = 10, year_start = 1)
glance(fit)

# scenario 4 - 0.85 survival and high coverage
x2 <- x[[4]][[1]]
bbs_plot_population(x2)
fit2 <- bboutools::bb_fit_survival(x2$survival,
                                   year_trend = TRUE,
                                   nthin = 10, year_start = 1)
glance(fit2)


year1 <- bb_predict_survival_trend(fit1)
year1$actual <- actual

year2 <- bb_predict_survival_trend(fit2)
year2$actual <- actual

plot_compare_prediction <- function(x){
  ggplot(data = x) +
    geom_ribbon(aes(x = CaribouYear, y = estimate, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(aes(x = CaribouYear, y = actual), color = "red") +
    geom_line(aes(x = CaribouYear, y = estimate), color = "black")
}

plot_compare_prediction(year1)
plot_compare_prediction(year2)

# check all values within CI
check_within_ci <- function(actual, pred){
  all(actual >= pred$lower & actual <= pred$upper)
}

check_within_ci(actual, year1)

# 100 datasets
# what proportion are within bounds?
# is direction same?
