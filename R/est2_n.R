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
#'@examples 
#'require("dplyr")
#' data(last_chance)
#' last_chance0  %>% 
#'   tidyr::gather(key = taxon, value = percent, -age_calBP, -totcaps) %>% 
#'   estimate_n(digits = 2, ID_cols = c("age_calBP", "age_calBP"))
#' 
#'@importFrom dplyr rename mutate group_by_at vars one_of n slice summarise
#'@importFrom tidyr nest unnest
#'@importFrom purrr map
#'@importFrom magrittr %>%
#'@importFrom rlang !! set_names .data
#'@importFrom utils data
#'@export
estimate_n <- function(x, percent_col = "percent", taxon_col = "taxon", ID_cols, digits = 2, nmin = 1L, nmax = 1000L){

  digits <- digits + 2 #analyse proportions
  possible_n <- x %>%
    rename(.percent = !!percent_col, .taxon = !!taxon_col) %>%
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
      inc_integer <- (floor(low) != floor(high)) %>% 
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
          rename(est_n_direct = n)
          ),
      
      #minimum percent method
      minimum_percent = map(
        data,
        ~ summarise(.,
          est_n_minpc = 1 / min(.data$p),
          est_min_minpc = 1 / min(.data$p_max),
          est_max_minpc = 1 / min(.data$p_min),
          n_taxa = n()#richness
        )
      )
      
    ) %>% 
    unnest(.data$minimum_percent)

  return(possible_n)
}

# #test
# tibble(sampleID = "a", count = c(5, 17, 36), percent = count/sum(count) * 100, taxon = letters[1:3]) %>% 
#   mutate(percent = round(percent, 2)) %>%
#   estimate_n2(digits = 2, ID_cols = "sampleID")

