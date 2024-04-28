test_that("can plot population data.frame", {
  withr::with_seed(10, {
    survival <- bbs_survival_caribou(0.84)
    fecundity <- bbs_fecundity_caribou(0.7)
    x <- bbs_simulate_caribou(survival, fecundity)
    gp <- bbs_plot_population(x[[1]]$abundance)
    expect_s3_class(gp, "ggplot")
    expect_snapshot_plot(gp, "population_dataframe")
  })
})

test_that("can plot population data.frame monthly", {
  withr::with_seed(10, {
    survival <- bbs_survival_caribou(0.84)
    fecundity <- bbs_fecundity_caribou(0.7)
    x <- bbs_simulate_caribou(survival, fecundity)
    gp <- bbs_plot_population(x[[1]]$abundance, annual = FALSE)
    expect_s3_class(gp, "ggplot")
    expect_snapshot_plot(gp, "population_dataframe_monthly")
  })
})

test_that("can plot population matrix", {
  withr::with_seed(10, {
    survival <- bbs_survival_caribou(0.84)
    fecundity <- bbs_fecundity_caribou(0.7)
    x <- bbs_population_caribou(survival, fecundity)
    gp <- bbs_plot_population(x)
    expect_s3_class(gp, "ggplot")
    expect_snapshot_plot(gp, "population_caribou")
  })
})

test_that("can plot population matrix 2 stages and 4 periods", {
  withr::with_seed(10, {
    survival <- bbs_survival(
      intercept = logit(c(0.94, 0.98)),
      trend = c(0, 0.3),
      annual_sd = rep(0.05, 2),
      period_sd = rep(0.1, 2),
      nyear = 5,
      nperiod_within_year = 4
    )
    fecundity <- bbs_fecundity(
      intercept = c(NA, logit(0.4)),
      trend = c(0, -0.2),
      annual_sd = c(0, 0.1),
      nyear = 5
    )
    survival_mat <- bbs_matrix_survival_period(survival$eSurvival)
    birth_mat <-
      bbs_matrix_birth_year(fecundity$eFecundity,
        female_recruit_stage = 1,
        male_recruit_stage = NULL
      )
    age_mat <- bbs_matrix_age(c(2, 2))
    pop0 <- c(105, 220)
    x <- bbs_population(pop0,
      birth = birth_mat,
      age = age_mat,
      survival = survival_mat
    )
    gp <- bbs_plot_population(x, nperiod_within_year = 4)
    expect_s3_class(gp, "ggplot")
    expect_snapshot_plot(gp, "population_stage")
  })
})

test_that("can plot population bbou simulation", {
  withr::with_seed(10, {
    survival <- bbs_survival_caribou(0.84)
    fecundity <- bbs_fecundity_caribou(0.7)
    x <- bbs_simulate_caribou(survival, fecundity, nsims = 2)
    gp <- bbs_plot_population(x)
    expect_s3_class(gp, "ggplot")
    expect_snapshot_plot(gp, "population_bbou_simulation")
  })
})
