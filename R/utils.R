year_period <- function(year, period, max_period){
  ((year-1) * max_period) + period
}

period_to_year <- function(x, nperiod_within_year = 12){
  ceiling(x/nperiod_within_year)
}

period_to_month <- function(x){
  year <- period_to_year(x)
  x - ((year-1) * 12)
}

.center <- function(x){
  scale(x, center = TRUE, scale = FALSE)
}

# ncol = nrow
empty_matrix <- function(n, value = 0){
  x <- matrix(rep(value, n*n), ncol = n)
}

population_tbl <- function(x, nperiod_within_year = 12){
  colnames(x) <- 1:ncol(x)
  x %>%
    as.data.frame() %>%
    mutate(Stage = factor(1:nrow(x))) %>%
    pivot_longer(-ncol(.), names_to = "Period", values_to = "Abundance") %>%
    mutate(Period = as.integer(Period), 
           Year = period_to_year(.data$Period, nperiod_within_year = nperiod_within_year))
}

