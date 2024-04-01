#' Plot population
#'
#' Plot simulated population (i.e., from output of `bb_simulate_population()`).
#'
#' @inheritParams params
#' @param annual A flag indicating whether to show annual population (as opposed to monthly).
#'
#' @return A nlist object with projected abundance at each stage, period and simulation.
#' @export
#'
bbs_plot_population <- function(population, annual = TRUE) {
  library(ggplot2)
  x <- as.data.frame(t(population))
  colnames(x) <- 1:6
  x$Period <- 1:nrow(x)
  x$Year <- period_to_year(x$Period)
  x <-
    x %>%
    tidyr::pivot_longer(1:6, names_to = "Stage", values_to = "Abundance") %>%
    dplyr::mutate(
      Stage = dplyr::case_when(
        Stage == 1 ~ "Female Calf",
        Stage == 2 ~ "Male Calf",
        Stage == 3 ~ "Female Yearling",
        Stage == 4 ~ "Male Yearling",
        Stage == 5 ~ "Female Adult",
        Stage == 6 ~ "Male Adult"
      ),
      Stage = factor(
        Stage,
        levels = c(
          "Female Calf",
          "Male Calf",
          "Female Yearling",
          "Male Yearling",
          "Female Adult",
          "Male Adult"
        )
      )
    )
  if (annual) {
    x <-
      x %>%
      dplyr::arrange(Period) %>%
      dplyr::group_by(Year, Stage) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  }
  ggplot2::ggplot(data = x) +
    ggplot2::geom_line(ggplot2::aes(x = Year, y = Abundance, color = Stage)) +
    poispalette::scale_color_disc_poisson()
}
