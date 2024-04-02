#' Plot Population 
#'
#' Plots population abundance by period and stage.
#'
#' @inheritParams params
#' @export
bbs_plot_population <- function(x, ...) {
  UseMethod("bbs_plot_population")
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a data frame (output of [bbs_simulate_caribou()$abundance]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
bbs_plot_population.data.frame <- function(x, annual = TRUE, ...) {
  chk_unused(...)
  chk_flag(annual)
  check_data(x, values = list(
    Stage = "",
    Year = c(0),
    Abundance = c(0)
  ))
 
  if (annual) {
    x <-
      x %>%
      dplyr::arrange(Month) %>%
      dplyr::group_by(Year, Stage) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }
  ggplot2::ggplot(data = x) +
    ggplot2::geom_line(ggplot2::aes(x = Year, y = Abundance, color = Stage)) 
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a matrix (output of [bbs_population_caribou()]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
bbs_plot_population.matrix <- function(x, annual = TRUE, ...) {
  chk_unused(...)
  chk_flag(annual)
  x <- abundance_tbl(x)
  bbs_plot_population(x)
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a matrix (output of [bbs_simulate_caribou()]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
bbs_plot_population.list <- function(x, annual = TRUE, ...) {
  chk_unused(...)
  chk_flag(annual)
  bbs_plot_population(x$abundance)
}
