


clean_dataframe <- function(df) {
  for (var in colnames(df)) {
    for (l in names(attributes(df[[var]]))) {
      attr(df[[var]], l) <- NULL
    }
  }
  df
}


get_measure_ref_dt <- function(star_version) {
  m_dt <- measure_ref$measure_dt
  if (star_version == "202104") {
    m_dt
  } else if (star_version == "202207") {
    m_dt[!m_dt$retired_2021]
  }
}


#' Step 1 of the star rating algorithm - standardize measure scores
#'
#' @param sas_input_dt Input data, same format as the 2021-04 SAS example input
#' @param star_version Either "202207" (the default) or "202104"
#'
#' @return a data.table
#' @export
sas_step1_standardize <- function(sas_input_dt, star_version = "202207") {

  if (!data.table::is.data.table(sas_input_dt)) {
    sas_input_dt <- data.table::data.table(sas_input_dt)
  }

  measure_ref_dt <- get_measure_ref_dt(star_version)
  measures_all <- measure_ref_dt$measure_id
  measures_all <- measures_all[measures_all %in% colnames(sas_input_dt)]

  measure_volume <-
    colSums(
      sas_input_dt[
        , ..measures_all
      ][
        , (measures_all) := lapply(.SD, function(x) as.integer(!is.na(x))),
        .SDcols = measures_all
      ][]
    )

  measures_in  <- names(measure_volume[measure_volume >  100])
  measures_out <- names(measure_volume[measure_volume <= 100])
  sign_flips <- setNames(measure_ref_dt$flip_sign,
                         measure_ref_dt$measure_id)
  columns_in <-
    colnames(sas_input_dt)[!(colnames(sas_input_dt) %in% measures_out)]

  scale_and_flip <- function(cname) {
    scale(sas_input_dt[[cname]]) * sign_flips[cname]
  }

  list(
    'measure_ids' = measures_in,
    'dt' =
      sas_input_dt[
        , ..columns_in
      ][
        , (measures_in) := lapply(names(.SD), scale_and_flip)
        , .SDcols = measures_in
      ][],
    'dropped_measure_ids' = measures_out
  )
}



#' Step 2 of the star rating algorithm - Calculate group-level scores
#'
#' @param std_data_dt cleaned & standardized data.table from step 1
#' @param measure_ids the measure_ids with enough observed values to be included
#'
#' @return a data.table
#' @export
sas_step2_groups <- function(std_data_dt, measure_ids) {

  temp_measures_dt <-
    data.table::merge.data.table(
      measure_ref$measure_dt[, c("measure_id", "group_name")],
      measure_ref$group_dt, by = "group_name"
    )

  keep_cols <- c("PROVIDER_ID", measure_ids)
  group_score_not_std_dt <-
    data.table::melt.data.table(
      std_data_dt[, ..keep_cols], "PROVIDER_ID",
      variable.name = "measure_id", value.name = "value", na.rm = TRUE
    )[
      temp_measures_dt, on = "measure_id", nomatch = 0
    ][
      , .(
        score_before_std = mean(value),
        N = .N,
        include_group = as.integer(.N >= 3),
        include_mortsafe = as.integer((sum(mortsafe_flag == 1)) >= 3)
      )
      , by = c("PROVIDER_ID", "group_name", "weight")
    ]

  group_stats_dt <-
    group_score_not_std_dt[
      , .(score_avg = mean(score_before_std),
          score_sd = sd(score_before_std)), by = "group_name"
    ]

  group_scores_dt <-
    data.table::merge.data.table(
      group_score_not_std_dt, group_stats_dt, by = c("group_name")
    )
  group_scores_dt[
    , group_score := (score_before_std - score_avg) / score_sd
  ][]
}





compute_star_clusters <- function(summary_score) {

  quantiles <- stats::quantile(summary_score, probs = 0.2*(1:4), type=1)

  initial_centers_dt <-
    data.table::data.table(summary_score = summary_score)[
      , grp := as.integer(cut(summary_score, breaks = c(-Inf, quantiles, Inf)))
    ][
      order(grp),
      .(summary_score_median = median(summary_score)),
      by = "grp"
    ]

  second_centers <-
    sas_proc_fastclus(
      summary_score, initial_centers_dt$summary_score_median, iter.max = 1000
    )$centers

  km <- sas_proc_fastclus(
    summary_score, second_centers, iter.max = 1000, strict = 1
  )
  abs(km$cluster)
}



#' Step 3 of the star rating algorithm - use clustering to compute stars
#'
#' @param group_scores_dt a data.table
#'
#' @return a data.table
#' @export
sas_step3_stars <- function(group_scores_dt) {

  group_scores_pivoted_dt <-
    data.table::dcast(
      group_scores_dt, PROVIDER_ID ~ group_name,
      value.var = c("group_score", "N")
    )

  temp_summary_scores_dt <-
    group_scores_dt[
      order(PROVIDER_ID), .(
        n_groups = sum(include_group),
        n_mortsafe = sum(include_mortsafe),
        report_indicator = as.integer(sum(include_group) >= 3) *
          as.integer(sum(include_mortsafe) >= 1),
        summary_score = sum(weight * group_score) / sum(weight)
      ), by = "PROVIDER_ID"
    ]

  temp_summary_scores_dt[
    report_indicator == 1,
    stars := compute_star_clusters(summary_score),
    by = "n_groups"
  ]

  data.table::merge.data.table(
    group_scores_pivoted_dt, temp_summary_scores_dt,
    by = "PROVIDER_ID", all.x = TRUE
  )
}



#' Compute star scores
#'
#' @param sas_input_df Input data, with approximate the same format as in the
#'     example files for the 2021-04 or 2022-07 versions of the SAS package
#' @param star_version Either "202207" (the default) or "202104"
#'
#' @return The input table with star computation results added in the far right
#'     columns
#' @export
compute_star_scores <- function(sas_input_df, star_version = "202207") {

  sas_input_dt <- data.table::as.data.table(sas_input_df)
  init_list <- sas_step1_standardize(sas_input_dt, star_version)
  group_scores_dt <- sas_step2_groups(init_list$dt,
                                             init_list$measure_ids)
  final_dt <- sas_step3_stars(group_scores_dt)
  data.table::merge.data.table(sas_input_dt, final_dt, by = "PROVIDER_ID",
                               all.x = TRUE) %>%
    as.data.frame() %>%
    clean_dataframe()
}



