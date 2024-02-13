yearmon_period <- function(year, month){
  ((year-1) * 12) + month
}

population_to_df <- function(population, states = 1:nrow(population)){
  chk_is(population, "matrix")
  as_tibble(population) %>%
    mutate(state = forcats::fct_inorder(states)) %>%
    pivot_longer(starts_with("V"), names_to = "period", values_to = "abundance", names_transform = function(x) as.integer(gsub("V", "", x)))
}

plot_population <- function(population, states = 1:nrow(population)){
  population_to_df(population, states = states) %>%
    ggplot(data = .) +
    aes(x = period, y = abundance, group = state, color = state) +
    geom_line()
}
