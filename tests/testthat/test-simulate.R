test_that("bbs_simulate_caribou works", {
  x <- bbs_simulate_caribou(nyear = 10)
  expect_true(is.list(x))
  expect_identical(names(x), c("survival", "recruitment", "abundance"))
  expect_true(all(unlist(lapply(x, is.data.frame))))
  
  expect_equal(max(x$survival$Year), 10)
})

test_that("bbs_simulate_caribou works with bboutools", {
  skip()
  survival_adult_female <- 0.85
  survival_calf_female <- 0.5
  survival_trend <- 0
  nyear <- 30L
  calves_per_adult_female = 0.22
  
  dem <- bbs_demographic_summary(calves_per_adult_female = calves_per_adult_female,
                                 survival_adult_female = survival_adult_female,
                                 survival_calf = survival_calf_female, 
                                 proportion_female = 0.5, 
                                 survival_yearling = survival_adult_female)
  dem
  
  x <- bbs_simulate_caribou(adult_females = 500, 
                            proportion_adult_female = 0.65, 
                            survival_adult_female = survival_adult_female,
                            survival_trend_adult_female = survival_trend, 
                            nyear = nyear,
                            calves_per_adult_female = calves_per_adult_female,
                            survival_calf_female = survival_calf_female,
                            group_coverage = 0.2, 
                            group_size = 15,
                            collared_adult_females = 30)
  
  library(bboutools)
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

