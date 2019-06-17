#' Percent checker
#' @description check percent in species file are plausible
#' @param spp data.frame of species percent data, optionally with columns for site and count sum
#' @param digits integer giving the precision of the percent data
#' @param site_column character giving name of column with site/sample names. If missing uses rownames instead.
#' @param count_column character giving name of column with count sums. If missing count sum is estimated from smallest percentage.
#' @importFrom magrittr %>%
#' @importFrom dplyr rename_ filter group_by mutate case_when ungroup
#' @importFrom tidyr gather
#' @importFrom tibble rowid_to_column
#' @importFrom rlang .data
#' @examples 
#' data(last_chance)
#' last_chance <- dplyr::select(last_chance0, -totcaps)
#' percent_checker(spp = last_chance, digits = 2, site_column = "age_calBP")
#' @export

percent_checker <- function(spp, digits, site_column, count_column){
  if(missing(site_column)){
    spp <- spp %>% 
      rowid_to_column(var = "site")
  } else {
    spp <- spp %>% 
      rename_(site = site_column)
  }
  
  if(missing(count_column)){
    spp2 <- spp %>% 
      gather(key = "species", value = "percent", -.data$site) %>% 
      filter(.data$percent > 0) %>% 
      group_by(.data$site) %>% 
      mutate(one = min(.data$percent),
             one_max = .data$one + 0.5 * 10 ^ -digits,
             one_min = .data$one - 0.5 * 10 ^ -digits,
             est_n = 100/.data$one,
             est_min = 100/.data$one_max,
             est_max = 100/.data$one_min
      )
  } else {
    spp2 <- spp %>% 
      rename_(count_sum = count_column) %>% 
      gather(key = "species",
             value = "percent",
             -.data$site, -.data$count_sum) %>% 
      filter(.data$percent > 0) %>% 
      group_by(.data$site) %>% 
      mutate(one = 100/.data$count_sum,
             one_max = .data$one,
             one_min = .data$one)
  }
  
  spp2 %>% 
    mutate(
      count_est = .data$percent/.data$one,
      count_min = (.data$percent - 0.5 * 10 ^ -digits)/(.data$one_max),
      count_max = (.data$percent + 0.5 * 10 ^ -digits)/(.data$one_min)
    ) %>%  
    mutate(
      precision = case_when(
        count_max - count_min < 0.01 ~ "very high",
        count_max - count_min < 0.1 ~ "high",
        count_max - count_min < 0.5 ~ "moderate",
        count_max - count_min < 1 ~ "low",
        TRUE ~ "none"
      ),
      contains = case_when(
        floor(count_min) < floor(count_max) ~ "integer",
        floor(count_min - 0.5) < floor(count_max - 0.5) ~ "?half",
        TRUE ~ "weird"
      ),
      precision = factor(
        x = .data$precision, 
        levels = c("very high", "high", "moderate", "low", "none"), 
        ordered = TRUE)
    ) %>% 
    ungroup()
}
