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
    Stage = "",
    Year = c(0),
    Abundance = c(0)
  ))
  
  if(length(unique(x$Stage)) == 6){
    x <- 
      x %>% mutate(Stage = factor(.data$Stage, levels = c("Female Adult", 
                                                          "Male Adult", 
                                                          "Female Yearling", 
                                                          "Male Yearling", 
                                                          "Female Calf", 
                                                          "Male Calf")))
  }

  if (annual) {
    x <-
      x %>%
      arrange(.data$Month) %>%
      group_by(.data$Year, .data$Stage) %>%
      slice(1) %>%
      ungroup()
  }
  ggplot(data = x) +
    geom_line(aes(x = .data$Year, y = .data$Abundance, color = .data$Stage)) +
    scale_color_manual(values = c("#000000", "#3063A3", "#E8613C", "#F7B500", "#821C65", "#63BB42"))
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
  bbs_plot_population(x, annual = annual)
}

#' @describeIn bbs_plot_population Plot population abundance by period and stage for a matrix (output of [bbs_simulate_caribou()]).
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#' @return A ggplot object.
#' @export
bbs_plot_population.list <- function(x, annual = TRUE, ...) {
  chk_unused(...)
  chk_flag(annual)
  bbs_plot_population(x$abundance, annual = annual)
}
