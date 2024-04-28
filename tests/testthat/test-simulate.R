test_that("bbs_simulate_caribou works", {
  withr::with_seed(10, {
    nsims <- 3
    nyear <- 5
    survival <- bbs_survival_caribou(0.84, nyear = nyear)
    fecundity <- bbs_fecundity_caribou(0.7, nyear = nyear)
    x <- bbs_simulate_caribou(survival, fecundity = fecundity, nsims = nsims)
    expect_s3_class(x, "bbou_simulation")
    expect_length(x, nsims)
    expect_snapshot({
      print(x)
    })
  })
})

test_that("bbs_simulate_caribou works with bboutools and values can be recovered", {
  skip()
  library(ggplot2)
  library(bboutools)

  survival_adult_female <- 0.85
  survival_calf_female <- 0.5
  trend_adult_female <- 0
  annual_sd_adult_female <- 0.3
  nyear <- 20L
  calves_per_adult_female <- 0.7
  nsims <- 1

  survival <- bbs_survival_caribou(
    nyear = nyear,
    survival_adult_female = survival_adult_female,
    annual_sd_adult_female = annual_sd_adult_female,
    survival_calf_female = survival_calf_female,
    trend_adult_female = trend_adult_female
  )

  fecundity <- bbs_fecundity_caribou(
    calves_per_adult_female = calves_per_adult_female,
    nyear = nyear
  )

  x <- bbs_simulate_caribou(survival,
    fecundity = fecundity,
    nsims = nsims,
    adult_females = 500,
    proportion_adult_female = 0.65,
    group_coverage = 1,
    group_size = 6,
    collared_adult_females = 200
  )

  fit_r <- bboutools::bb_fit_recruitment(x[[1]]$recruitment,
    adult_female_proportion = NULL,
    yearling_female_proportion = 0.5
  )

  fit_s <- bboutools::bb_fit_survival(x[[1]]$survival,
    min_random_year = Inf,
    nthin = 30L, year_start = 1L
  )

  saf <- survival$eSurvival[, , 3]
  nyear <- dim(saf)[2]
  saf_annual <- vector(length = nyear)
  for (yr in 1:nyear) {
    saf_annual[yr] <- prod(saf[, yr])
  }

  fecundity$eFecundity

  # actual
  # sims trend is effect of one year change on monthly log-odds prob
  # bboutools trend is effect of scaled year change
  # sims intercept is first year
  # bboutools intercept is mean year
  year <- bb_predict_survival(fit_s)
  year$actual <- saf_annual

  ggplot(data = year) +
    geom_pointrange(aes(x = CaribouYear, y = estimate, ymin = lower, ymax = upper), alpha = 0.5) +
    geom_point(aes(x = CaribouYear, y = actual), size = 2, color = "red")
})
