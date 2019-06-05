#' Estimate count sum
#'@description Estimate count sum from percent data
#'@param x data.frame of percent data in long format
#'@param percent_col character giving name of column with percent data
#'@param taxon_col character giving name of column with taxon names
#'@param ID_cols character vector giving name of one or more columns with sample IDs
#'@param digits numeric; how many decimal places are the percent reported to
#'@param nmin integer; possible count sum to start direct search at
#'@param nmax integer; possible count sum to end direct seach at
#'
#'@details Estimates the count sum with two methods.
#' The first estimate is 100/p_min where p_min is the mimumum percent.
#' The second estimate is the value N within the range nmin:nmax for which all p/100 * N is an integer for all percent p. The proportion of taxa for which this is an integer is given by the score. A warning is given if the score is below 1. This can happen nmax is less than the count sum, or if the percent have been incorrectly calculated (perhaps an error in rounding.
#' 
#'  @return A nested tibble
#'  #' \itemize{
#'   \item ID_cols - one or more user specified columns identifying the assemblages 
#'   \item data - nested raw data from each assemblage
#'   \item direct_search - nested raw output of the direct_search method
#'   \item direct_search_est - nested with the value(s) of \code{est_n_direct} with the highest \code{score}. If the score is <1 a warning is given. This can contain more than row, especially if the precision of the percent data is low and the estimated count sum is high.
#'   \item minpc - the minimum percentage for each assemblage
#'   \item est_n_minpc estimate count sum for the minimum percent method
#'   \item est_min_minpc - lowest possible count sum given precision of the percentage data 
#'   \item est_max_minpc - ditto for hightest possible count sum
#'   \item n_taxa - number of taxa
#' } 
#'@references Telford (2019) Tools for identifying unexpectely low microfossil count sums. Preprint. 
#'@examples 
#'require("dplyr")
#' data(last_chance)
#' last_chance0  %>% 
#'   tidyr::gather(key = taxon, value = percent, -age_calBP, -totcaps) %>% 
#'   estimate_n(digits = 2, ID_cols = c("age_calBP", "totcaps"))
#' 
#'@importFrom dplyr rename mutate group_by_at vars one_of n slice summarise
#'@importFrom tidyr nest unnest
#'@importFrom purrr map
#'@importFrom magrittr %>%
#'@importFrom rlang !! set_names .data
#'@importFrom utils data
#'@importFrom assertr assert in_set just_warn within_bounds
#'@export
estimate_n <- function(x, percent_col = "percent", taxon_col = "taxon",
                       ID_cols, digits = 2, nmin = 1L, nmax = 1000L){

  digits <- digits + 2 #analyse proportions
  possible_n <- x %>% 
    rename(.percent = !!percent_col, .taxon = !!taxon_col) %>%
    #check percent within 0- 100
    assert(within_bounds(0, 100), .data$.percent) %>% 
    #remove any 0 values - give Inf estimates
    filter(.data$.percent > 0) %>% 
    mutate(
      p = .data$.percent / 100,
      p_min = .data$p - 0.5 * 10 ^ -digits,
      p_max = .data$p + 0.5 * 10 ^ -digits
    ) %>%
    group_by_at(vars(one_of(ID_cols))) %>% 
    nest() %>% 
    mutate(direct_search = map(.data$data, ~{
      #direct search
      low <- nmin:nmax %*% t(.$p_min)
      high <- nmin:nmax %*% t(.$p_max)
      .taxon <- .$.taxon
      inc_integer <- (
        #span includes integer
        floor(low) != floor(high) | 
        #catch case where low is exact integer (within tolerance)  
        abs(low - round(low)) < sqrt(.Machine$double.eps)
       ) %>% 
        as.data.frame() %>% 
        set_names(.taxon) %>% 
        mutate(
          score = rowMeans(.),
          n = nmin:nmax
        )
      inc_integer

      })) %>% 
    mutate(
      #summarise direct_search
      direct_search_est = map(
        .data$direct_search, 
        ~slice(., which.max(score):n()) %>% 
          slice(1:(which.min(score == max(score)) - 1)) %>% 
          select(est_n_direct = n, score) 
          ),
      
      #minimum percent method
      minimum_percent = map(
        data,
        ~ summarise(.,
          minpc = min(.data$p) * 100,
          est_n_minpc = 1 / min(.data$p),
          est_min_minpc = 1 / min(.data$p_max),
          est_max_minpc = 1 / min(.data$p_min),
          n_taxa = n()#richness
        )
      )
      
    ) %>% 
    unnest(.data$minimum_percent)

  #check scores == 1
  possible_n %>% 
    unnest(.data$direct_search_est) %>% 
    ungroup() %>% 
    assert(in_set(1), .data$score, error_fun = just_warn)
  
  return(possible_n)
}



