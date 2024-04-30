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
#' @examples
#' survival <- bbs_survival_caribou(0.84)
#' fecundity <- bbs_fecundity_caribou(0.7)
#' x <- bbs_simulate_caribou(survival, fecundity = fecundity)
#' bbs_plot_population(x[[1]]$abundance)
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
#' @examples
#' pop0 <- c(100, 200)
#' survival <- bbs_survival(intercept = logit(c(0.95, 0.98)))
#' fecundity <- bbs_fecundity(intercept = c(NA, logit(0.4)))
#' survival_mat <- bbs_matrix_survival_period(survival$eSurvival)
#' birth_mat <- bbs_matrix_birth_year(fecundity$eFecundity)
#' age_mat <- bbs_matrix_age(c(2, 2))
#' x <- bbs_population(pop0,
#'   birth = birth_mat,
#'   age = age_mat,
#'   survival = survival_mat
#' )
#' bbs_plot_population(x)
bbs_plot_population.bbou_population <- function(x, annual = TRUE, nperiod_within_year = 12, ...) {
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
#' @param alpha A number between 0 and 1 of the point transparency.
#' @return A ggplot object.
#' @export
#' @examples
#' survival <- bbs_survival_caribou(0.84)
#' fecundity <- bbs_fecundity_caribou(0.7)
#' x <- bbs_simulate_caribou(survival, fecundity = fecundity, nsims = 3)
#' bbs_plot_population(x, alpha = 0.7)
bbs_plot_population.bbou_simulation <- function(x, annual = TRUE, alpha = 0.5, ...) {
  chk_unused(...)
  chk_flag(annual)
  chk_range(alpha)

  x <-
    dplyr::bind_rows(purrr::map(x, ~ .x$abundance), .id = "sim") %>%
    mutate(group = paste(.data$Stage, .data$sim))

  if (annual) {
    x <-
      x %>%
      group_by(.data$Year, .data$Stage, .data$sim) %>%
      slice(1) %>%
      ungroup()

    gp <- ggplot(data = x) +
      geom_line(aes(x = factor(.data$Year), y = .data$Abundance, color = .data$Stage, group = .data$group), alpha = alpha) +
      xlab("Year")
  } else {
    gp <- ggplot(data = x) +
      geom_line(aes(x = .data$Period, y = .data$Abundance, color = .data$Stage, group = .data$group), alpha = alpha)
  }
  gp + scale_color_manual(values = c("#000000", "#3063A3", "#E8613C", "#F7B500", "#821C65", "#63BB42"))
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a matrix (output of [bbs_simulate_caribou()]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
#' @examples
#' survival <- bbs_survival_caribou(0.84)
#' fecundity <- bbs_fecundity_caribou(0.7)
#' x <- bbs_population_caribou(survival, fecundity = fecundity)
#' bbs_plot_population(x)
bbs_plot_population.bbou_population_caribou <- function(x, annual = TRUE, ...) {
  chk_unused(...)
  chk_flag(annual)
  x <- abundance_tbl(x)
  bbs_plot_population(x, annual = annual)
}
