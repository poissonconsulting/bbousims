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

test_that("can plot population list", {
  set.seed(101)
  x <- bbs_simulate_caribou()
  gp <- bbs_plot_population(x)
  expect_s3_class(gp, "ggplot")
  expect_snapshot_plot(gp, "population_list")
})


