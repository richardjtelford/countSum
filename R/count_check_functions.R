#' Percent checker
#' @description check percent in species file are plausible
#' @param spp data.frame of species percent data, optionally with columns for site and count sum
#' @param digits integer giving the precision of the percent data
#' @param site_column character giving name of column with site/sample names. If missing uses rownames instead.
#' @param count_column character giving name of column with count sums. If missing count sum is estimated from smallest percentage.
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom tibble rowid_to_column
#' @examples 
#' require("dplyr")
#' data(last_chance)
#' last_chance <- select(last_chance0, -totcaps)
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
      gather(key = "species", value = "percent", -site) %>% 
      filter(percent > 0) %>% 
      group_by(site) %>% 
      mutate(one = min(percent),
             one_max = one + 0.5 * 10 ^ -digits,
             one_min = one - 0.5 * 10 ^ -digits,
             est_n = 100/one,
             est_min = 100/one_max,
             est_max = 100/one_min
      )
  } else {
    spp2 <- spp %>% 
      rename_(count_sum = count_column) %>% 
      gather(key = "species", value = "percent", -site, -count_sum) %>% 
      filter(percent > 0) %>% 
      group_by(site) %>% 
      mutate(one = 100/count_sum,
             one_max = one,
             one_min = one)
  }
  
  spp2 %>% 
    mutate(
      count_est = percent/one,
      count_min = (percent - 0.5 * 10 ^ -digits)/(one_max),
      count_max = (percent + 0.5 * 10 ^ -digits)/(one_min)
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
      precision = factor(precision, levels = c("very high", "high", "moderate", "low", "none"), ordered = TRUE)
    ) %>% 
    ungroup()
}

#' Count estimator
#' @description Estimates count sums of species percent data
#' @param spp data.frame of species percent data
#' @param digits integer giving precision of species data
#' @importFrom dplyr data_frame
#' @examples 
#' require("dplyr")
#' data(last_chance)
#' last_chance <- select(last_chance0, -age_calBP, -totcaps)
#' estimate_n(spp = last_chance, digits = 2)
#' @export
estimate_n <- function(spp, digits){
  minp <- function(x){min(x[x > 0])}
  data_frame(mn = apply(spp, 1, minp),
             est_n = 100 / mn,
             est_min = 100 / (mn + 0.5 * 10 ^ -digits),
             est_max = 100 / (mn - 0.5 * 10 ^ -digits)
  )
}
