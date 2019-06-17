context("estimate_n")


test_that("estimate is correct", {
  df <- tibble(taxon = letters[1:10], ID = 1, count = 1:10) %>% 
    mutate(
      percent = count/sum(count) * 100, 
      percent = round(percent, 2)
    )
  res <- estimate_n(df, ID_cols = "ID")
  expect_equal(res$minpc, round(1/sum(1:10) * 100, 2))
  expect_equal(res$est_n_minpc, 100/round(1/sum(1:10) * 100, 2))
  expect_equal(unnest(res, direct_search_est)$score, 1)
  expect_equal(unnest(res, direct_search_est)$est_n_direct, sum(1:10))
})

test_that("Warning when no solution within range n_min:n_max", {
  df <- tibble(taxon = letters[1:10], ID = 1, count = 1:10) %>% 
    mutate(
      percent = count/sum(count) * 100, 
      percent = floor(percent * 100)/100 #floor rather than round
    )
   expect_output(suppressWarnings(estimate_n(df, ID_cols = "ID")), "Column 'score' violates assertion")
   expect_warning(estimate_n(df, ID_cols = "ID"))
  
    expect_lt(suppressWarnings(estimate_n(df, ID_cols = "ID")) %>% 
              unnest(direct_search_est) %>% 
              pull(score), 1)
})

test_that("error with percent out of bounds 0-100", {
  df <- tibble(taxon = letters[1:3], ID = 1, percent = c(-1, 50, 50))
  expect_error(estimate_n(df, ID_cols = "ID"))

  df <- tibble(taxon = letters[1:3], ID = 1, percent = c(101, 50, 50))
  expect_error(estimate_n(df, ID_cols = "ID"))
})

