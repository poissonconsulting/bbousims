year_period <- function(year, period, max_period){
  ((year-1) * max_period) + period
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

# ncol = nrow
empty_matrix <- function(n, value = 0){
  x <- matrix(rep(value, n*n), ncol = n)
}
