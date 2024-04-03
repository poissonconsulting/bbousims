test_that("can plot population data.frame", {
  set.seed(101)
  x <- bbs_simulate_caribou()
  gp <- bbs_plot_population(x$abundance)
  expect_s3_class(gp, "ggplot")
  expect_snapshot_plot(gp, "population_dataframe")
})

test_that("can plot population data.frame", {
  set.seed(101)
  x <- bbs_simulate_caribou()
  gp <- bbs_plot_population(x$abundance, annual = FALSE)
  expect_s3_class(gp, "ggplot")
  expect_snapshot_plot(gp, "population_dataframe_monthly")
})

test_that("can plot population matrix", {
  set.seed(101)
  x <- bbs_population_caribou()
  gp <- bbs_plot_population(x)
  expect_s3_class(gp, "ggplot")
  expect_snapshot_plot(gp, "population_matrix")
})

test_that("can plot population matrix 2 stages and 4 periods", {
  set.seed(101)
  survival <- bbs_survival(intercept = logit(c(0.94, 0.98)),
                           trend = c(0, 0.3),
                           annual_sd = rep(0.05, 2),
                           period_sd = rep(0.1, 2),
                           nyear = 5)
  set.seed(101)
  fecundity <- bbs_fecundity(intercept = c(NA, logit(0.4)),
                             trend = c(0, -0.2),
                             annual_sd = c(0, 0.1),
                             nyear = 5)
  survival_mat <- bbs_matrix_survival_period(survival)
  birth_mat <- bbs_matrix_birth_year(fecundity, female_recruit_stage = 1, male_recruit_stage = NULL)
  age_mat <- bbs_matrix_age(c(2, 2))
  pop0 <- c(105, 220)
  set.seed(101)
  x <- bbs_population(pop0, 
                               birth = birth_mat, 
                               age = age_mat, 
                               survival = survival_mat)
  gp <- bbs_plot_population(x)
  expect_s3_class(gp, "ggplot")
  expect_snapshot_plot(gp, "population_matrix_stage")
})

test_that("can plot population list", {
  set.seed(101)
  x <- bbs_simulate_caribou()
  gp <- bbs_plot_population(x)
  expect_s3_class(gp, "ggplot")
  expect_snapshot_plot(gp, "population_list")
})


