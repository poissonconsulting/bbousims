# drawn from gamma poisson
ran_group_size <- function(lambda, theta, max_size, min_size){
  inside <- FALSE
  while(!inside){
    size <- extras::ran_gamma_pois(n = 1, lambda = lambda, theta = theta)
    inside <- size < max_size & size > min_size
  }
  size
}

# sample groups until total reached - return as cumulative sum including 0 and total, for use as index
sample_groups <- function(total, lambda, theta, max_size, min_size, cumulative = TRUE){
  sizes <- vector()
  capacity <- FALSE
  while(!capacity){
    size <- ran_group_size(lambda, theta, max_size, min_size)
    sizes <- c(sizes, size)
    capacity <- sum(sizes) >= total
  }
  sizes <- sizes[-length(sizes)]
  # if remainder is too small, consumed by previous
  if(total - sum(sizes) < min_size)
    sizes <- sizes[-length(sizes)]
    
  if(cumulative){
    sizes <- c(0, cumsum(sizes), total)
  } else {
    sizes <- c(sizes, total-sum(sizes))
  }
  sizes
}

sample_1 <- function(x){
  has1 <- FALSE
  while(!has1){
    y <- sample(x, size = 1)
    has1 <- length(unlist(y)) == 1
  }
  y
}

# if all remaining picks are pairs but need an individual to fill group
break_up_pair <- function(x){
  last <- length(x) + 1
  x[last] <- x[[1]][1]
  names(x)[last] <- paste0(names(x)[1], "-2")
  x[1] <- x[[1]][2]
  x
}

# from stage totals create vector of individuals numbered by stage
population_individuals <- function(population, shuffle = TRUE){
  x <- unlist(purrr::map(seq_along(population), function(x) rep(x, population[x])))
  if(shuffle)
    x <- sample(x)
  x
}

# create recruit-female pairs, #pairs = min(recruits, females)
individuals_to_pairs <- function(x, recruit_stages, reproductive_female_stages){
  index <- 1:length(x)
  recruits <- index[x %in% recruit_stages]
  females <- index[x %in% reproductive_female_stages]
  min_n <- min(length(females), length(recruits))
  recruit_female <- purrr::map(seq_len(min_n), ~ c(recruits[.x], females[.x]))
  remaining <- as.list(index[!(index %in% unlist(recruit_female))])
  y <- c(remaining, recruit_female)
  lapply(y, function(i){x[i]})
}

# population in a single period
population1_groups <- function(population,
                               group_size_lambda,
                               group_size_theta,
                               group_max_proportion,
                               group_min_size){
  
  total <- sum(population)
  individuals <- population_individuals(population, shuffle = TRUE)
  index <- 1:total
  
  max_size <- total*group_max_proportion
  min_size = group_min_size
  
  if((floor(max_size) - 1) <= min_size){
    return(list(individuals))
  } else {
    sizes <- sample_groups(total, 
                           lambda = group_size_lambda,
                           theta = group_size_theta,
                           max_size = max_size,
                           min_size = group_min_size)
  }
  
  purrr::map(2:length(sizes), function(x){
    ind <- index[(sizes[x-1] + 1):sizes[x]]
    individuals[ind]
  })
}

population1_groups_pairs <- function(population,
                                     group_size_lambda,
                                     group_size_theta,
                                     group_max_proportion,
                                     group_min_size,
                                     recruit_stages,
                                     reproductive_female_stages){
  
  total <- sum(population)
  individuals <- population_individuals(population)
  pairs <- individuals_to_pairs(individuals, 
                                recruit_stages = recruit_stages,
                                reproductive_female_stages = reproductive_female_stages)
  names(pairs) <- 1:length(pairs)
  
  sizes <- sample_groups(total, 
                         lambda = group_size_lambda,
                         theta = group_size_theta,
                         max_size = total*group_max_proportion,
                         min_size = group_min_size,
                         cumulative = FALSE)
  
  purrr::map(sizes, function(x){
    sub <- list()
    
    remaining <- TRUE
    while(remaining){
      if(x - length(sub) == 1){
        if(all(purrr::map_lgl(pairs, ~ length(.x) == 2))){
          pairs <- break_up_pair(pairs)
        }
        pick <- sample_1(pairs)
      } else {
        pick <- sample(pairs, size = 1)
      }
      
      sub <- unlist(c(sub, pick))
      names(sub) <- NULL
      pairs <<- pairs[names(pairs) != names(pick)]
      remaining <- x - length(sub) > 0
    }
    sub
  })
}

#' Assign population into groups
#' 
#' In each period the population is assigned into groups. 
#' Group sizes are drawn randomly from a gamma-poisson distribution with specified lambda and theta. 
#' Group sizes are drawn until the cumulative size exceeds the total individuals. 
#' The remaining individuals comprise the final group. 
#' If the remaining number of individuals is < min_size specified, these will be added to the previous group. 
#' Groups are filled by drawing individuals randomly. 
#' 
#' @inheritParams params
#'
#' @return A list of groups in each period.
#' @export
#'
bbs_population_groups <- function(population,
                              group_size_lambda = 5,
                              group_size_theta = 2,
                              group_max_proportion = 1/4,
                              group_min_size = 2){
  chk_matrix(population)
  chk_whole_numeric(population)
  chk_number(group_size_lambda)
  chk_gt(group_size_lambda)
  chk_number(group_size_theta)
  chk_gte(group_size_theta)
  chk_number(group_max_proportion)
  chk_range(group_max_proportion)
  chk_whole_number(group_min_size)
  chk_gte(group_min_size)
  
  population <- as.matrix(population)
  nstep <- ncol(population)
  purrr::map(seq_len(nstep), ~ {
    population1_groups(population[,.x], 
                       group_size_lambda = group_size_lambda,
                       group_size_theta = group_size_theta,
                       group_max_proportion = group_max_proportion,
                       group_min_size = group_min_size)
  })
}

#' Assign population into groups by pairs
#' 
#' In each period the population is assigned into groups. 
#' Calf-cow pairs are created and assigned to groups together. 
#' Group sizes are drawn randomly from a gamma-poisson distribution with specified lambda and theta. 
#' Group sizes are drawn until the cumulative size exceeds the total individuals. The remaining individuals comprise the final group. 
#' If the remaining number of individuals is < min_size specified, these will be added to the previous group. 
#' Groups are filled by drawing pairs or individuals from the pool.
#' If there is only space for one individual left in a group, the pool of individuals (i.e., not pairs) will be sampled. 
#' If there are only pairs left in the pool and one individual is required to fill a group, a pair will be broken up. 
#'
#' @inheritParams params
#'
#' @return A list of groups for each period.
#' @export
#'
bbs_population_groups_pairs <- function(population,
                              group_size_lambda = 5,
                              group_size_theta = 2,
                              group_max_proportion = 1/4,
                              group_min_size = 2,
                              recruit_stages = c(1, 2),
                              reproductive_female_stages = c(3, 5)){
  chk_matrix(population)
  chk_whole_numeric(population)
  chk_number(group_size_lambda)
  chk_gt(group_size_lambda)
  chk_number(group_size_theta)
  chk_gte(group_size_theta)
  chk_number(group_max_proportion)
  chk_range(group_max_proportion)
  chk_whole_number(group_min_size)
  chk_gte(group_min_size)
  nstage <- nrow(population)
  chk_whole_numeric(recruit_stages)
  chk_range(recruit_stages, range = c(1, nstage))
  chk_whole_numeric(reproductive_female_stages)
  chk_range(recruit_stages, range = c(1, nstage))
  
  population <- as.matrix(population)
  nstep <- ncol(population)
  purrr::map(seq_len(nstep), function(x){
    population1_groups_pairs(population[,x], 
                       group_size_lambda = group_size_lambda,
                       group_size_theta = group_size_theta,
                       group_max_proportion = group_max_proportion,
                       group_min_size = group_min_size,
                       recruit_stages = recruit_stages,
                       reproductive_female_stages = reproductive_female_stages)
  })
}


