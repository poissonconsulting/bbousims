#' Plot Population 
#'
#' Plots population abundance by period and stage.
#'
#' @inheritParams params
#' @export
bbs_plot_population <- function(x, ...) {
  UseMethod("bbs_plot_population")
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a data frame (abundance data.frame in output [bbs_simulate_caribou()]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
bbs_plot_population.data.frame <- function(x, annual = TRUE, ...) {
  chk_unused(...)
  chk_flag(annual)
  check_data(x, values = list(
    Stage = factor(""),
    Year = c(0),
    Abundance = c(0)
  ))
  
  if (annual) {
    x <-
      x %>%
      group_by(.data$Year, .data$Stage) %>%
      slice(1) %>%
      ungroup() 
    
    gp <- ggplot(data = x) +
      geom_line(aes(x = .data$Year, y = .data$Abundance, color = .data$Stage))
  } else {
    gp <- ggplot(data = x) +
      geom_line(aes(x = .data$Period, y = .data$Abundance, color = .data$Stage))
  }
  gp + scale_color_manual(values = c("#000000", "#3063A3", "#E8613C", "#F7B500", "#821C65", "#63BB42"))
  
  
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a matrix (output of [bbs_population_caribou()]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
bbs_plot_population.bbou_population <- function(x, annual = TRUE, nperiod_within_year, ...) {
  chk_unused(...)
  chk_flag(annual)
  chk_whole_number(nperiod_within_year)
  chk_gt(nperiod_within_year)
  x <- population_tbl(x, nperiod_within_year = nperiod_within_year)
  bbs_plot_population(x, annual = annual)
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a matrix (output of [bbs_simulate_caribou()]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
bbs_plot_population.bbou_simulation <- function(x, annual = TRUE, ...) {
  chk_unused(...)
  chk_flag(annual)
  bbs_plot_population(x$abundance, annual = annual)
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a matrix (output of [bbs_simulate_caribou()]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
bbs_plot_population.bbou_population_caribou <- function(x, annual = TRUE, ...) {
  chk_unused(...)
  chk_flag(annual)
  x <- abundance_tbl(x)
  bbs_plot_population(x, annual = annual)
}
