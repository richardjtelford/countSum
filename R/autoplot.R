#' Autoplot estimate count sum
#'@description Estimate count sum from percent data
#'
#'@param x output of estimate count sum
#'@param row integer giving name of row with sample to plot
#'@param n_lim Limits on range of n.
#'Defaults to full range available which can cause problems when by_taxa is TRUE
#' as lines may fail to show because of screen resolution.
#'@param threshold numeric giving minimum proportion of taxa that
#' need to be concordant with count sum.
#'@param by_taxa logical giving name of column with taxon names
#'@param show_score logical. Show scores instead of main plot
#'@return ggplot object.
#'@references Telford (2019) Tools for identifying unexpectedly low
#' microfossil count sums. Preprint.
#'@examples
#'require("dplyr")
#'require("ggplot2")
#' data(last_chance)
#' last_chance_est <- last_chance0  %>%
#'   tidyr::pivot_longer(cols = -c(age_calBP, totcaps),
#'                       names_to = "taxon", values_to = "percent") %>%
#'   estimate_n(digits = 2, ID_cols = c("age_calBP", "totcaps"))
#'autoplot(last_chance_est, row = 10, by_taxa = FALSE)
#'autoplot(last_chance_est, row = 10, show_score = TRUE)
#'
#'@importFrom dplyr mutate n slice select filter left_join if_else
#'@importFrom tidyr unnest pivot_longer
#'@importFrom purrr map
#'@importFrom magrittr %>%
#'@importFrom rlang !! set_names .data
#'@importFrom ggplot2 ggplot aes geom_raster geom_point scale_x_log10 autoplot
#'scale_fill_discrete scale_x_continuous scale_y_continuous geom_hline geom_line
#' coord_flip labs
#'@export

autoplot.possible_n <- function(x, row = 1, threshold = 1, n_lim,
                                by_taxa = TRUE, show_score = FALSE) {

   dat0 <-  x %>%
    ungroup() %>%
    slice(row)

   dat2 <- dat0 %>%
     select(-data) %>%
     unnest(cols = .data$direct_search)

   if (missing(n_lim)) {
     n_lim <- range(dat2$n)
   }

  if (isTRUE(show_score)) {
    p2 <- ggplot(dat2, aes(x = .data$n, y = .data$score)) +
      geom_line() +
      coord_flip() +
      scale_x_continuous(expand = c(0, 0), limits = n_lim) +
      scale_y_continuous(breaks = c(0, 0.5, 1)) +
      labs(x = "Score", y = "Putative count sum")
    p2

  } else {
    # extract data
    dat1 <- dat0 %>%
      select(-data) %>%
      mutate(direct_search = map(
        .data$direct_search,
        ~pivot_longer(.x, cols = -c(.data$score, .data$n),
                      names_to = "taxa", values_to = "value"))) %>%
      unnest(cols = .data$direct_search) %>%
      mutate(value = if_else(.data$value, .data$value, NA))

    if (isTRUE(by_taxa)) {
      # raster by taxa
      p1 <- ggplot(dat1,
                   aes(x = .data$taxa, y = .data$n,
                       fill = .data$value * .data$score >= threshold)) +
        geom_raster(show.legend = FALSE) +
        scale_fill_discrete(na.value = NA) +
        labs(x = "")
    } else {
      # points by percent
      dat1_percent <- dat0 %>%
        select(.data$data) %>%
        unnest(cols = .data$data) %>%
        group_by(.data$.percent) %>%
        mutate(.n_same = n()) %>%
        left_join(dat1, by = c(.taxon = "taxa"))

      p1 <- ggplot(dat1_percent %>% filter(.data$value),
                   aes(x = .data$.percent, y = .data$n,
                       colour = .data$.n_same)) +
        geom_hline(aes(yintercept = .data$n),
                   data = dat1_percent %>% filter(.data$score >= threshold)) +
        geom_point() +
        geom_point(data = dat1_percent %>%
                     filter(is.na(.data$value), .data$score > threshold),
                   shape = 3) +
        scale_x_log10() +
        labs(x = "Percent", colour = "No taxa")
    }

    p1 <- p1 +
      scale_y_continuous(expand = c(0, 0), limits = n_lim) +
      labs(y = "Putative count sum")
    p1
  }
}
