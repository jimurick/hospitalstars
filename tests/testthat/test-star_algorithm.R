
library(dplyr, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)



test_that("Algorithm v202104 agrees with SAS", {

  # Output from the SAS package released April 2021, starting from the sample
  # input data. The SAS output was copied into the dataframe `output_df`
  sas_df <- hospitalstars::sas_package$v202104$output_df

  # Output using this R package with the same input data
  r_df <-
    hospitalstars::sas_package$v202104$input_df %>%
    hospitalstars::compute_star_scores(star_version = "202104")

  # They should have the same number of rows
  expect_equal(dim(sas_df)[1], dim(r_df)[1])

  # They should have the same hospital counts by star rating
  sas_star_counts <- sas_df %>% count(star) %>% arrange(star) %>% pull(n)
  r_star_counts <- r_df %>% count(stars) %>% arrange(stars) %>% pull(n)
  expect_equal(sas_star_counts, r_star_counts)

  # The summary scores should be within 0.01 of each other
  sas_scores_df <- sas_df %>%
    select(PROVIDER_ID, sas_score = summary_score) %>%
    filter(!is.na(sas_score)) %>%
    arrange(PROVIDER_ID)
  r_scores_df <- r_df %>%
    select(PROVIDER_ID, r_score = summary_score) %>%
    filter(!is.na(r_score)) %>%
    arrange(PROVIDER_ID)

  expect_equal(sas_scores_df$PROVIDER_ID, r_scores_df$PROVIDER_ID)
  different_df <-
    sas_scores_df %>%
    inner_join(r_scores_df, by = "PROVIDER_ID") %>%
    filter(abs(sas_score - r_score) > 0.01)
  expect_equal(dim(different_df)[1], 0)
})


test_that("Algorithm v202207 agrees with SAS", {

  # Output from the SAS package released July 2022, starting from the sample
  # input data. The SAS output was copied into the dataframe `output_df`
  sas_df <- hospitalstars::sas_package$v202207$output_df

  # Output using this R package with the same input data
  r_df <-
    hospitalstars::sas_package$v202207$input_df %>%
    hospitalstars::compute_star_scores(star_version = "202207")

  # They should have the same number of rows
  expect_equal(dim(sas_df)[1], dim(r_df)[1])

  # They should have the same hospital counts by star rating
  sas_star_counts <- sas_df %>% count(star) %>% arrange(star) %>% pull(n)
  r_star_counts <- r_df %>% count(stars) %>% arrange(stars) %>% pull(n)
  expect_equal(sas_star_counts, r_star_counts)

  # The summary scores should be within 0.01 of each other
  sas_scores_df <- sas_df %>%
    select(PROVIDER_ID, sas_score = summary_score) %>%
    filter(!is.na(sas_score)) %>%
    arrange(PROVIDER_ID)
  r_scores_df <- r_df %>%
    select(PROVIDER_ID, r_score = summary_score) %>%
    filter(!is.na(r_score)) %>%
    arrange(PROVIDER_ID)

  expect_equal(sas_scores_df$PROVIDER_ID, r_scores_df$PROVIDER_ID)
  different_df <-
    sas_scores_df %>%
    inner_join(r_scores_df, by = "PROVIDER_ID") %>%
    filter(abs(sas_score - r_score) > 0.01)
  expect_equal(dim(different_df)[1], 0)
})



