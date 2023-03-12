

#' Get star rating algorithm input data from a specific Care Compare report
#'
#' @param report_date The first day of the month that a Care Compare report
#'   was released
#'
#' @return a data.frame of data that can be used as input to ``
#' @export
#'
#' @examples
star_rating_input <- function(report_date) {
  df <- hospitalstars::star_algorithm_input_df
  df[df$report_date == report_date, -1]
}


#' All historical data for specific measures and hospitals
#'
#'
#'
#' @param measure_ids A vector of measure_id values to include. If NULL (the
#'     default), include all measure_ids.
#' @param provider_ids A vector of PROVIDER_ID values to include. If NULL (the
#'     default), include all PROVIDER_IDs.
#' @return A data.frame with Care Compare data for the given measures and
#'     providers, with columns report_date, PROVIDER_ID, measure_id, score.
#' @export
#'
#' @examples
historical_measure_data <- function(measure_ids = NULL, provider_ids = NULL) {
  all_measure_cols <- colnames(hospitalstars::star_algorithm_input_df)[-(1:2)]
  all_score_cols <-
    all_measure_cols[
      !grepl("(_DEN(_PRED|_VOL)?|_COMP|_RATE_P)$", all_measure_cols)
    ]
  if (is.null(measure_ids)) {
    measure_cols <- all_score_cols
  } else {
    measure_cols <- measure_ids[measure_ids %in% all_measure_cols]
  }
  if (length(measure_ids) == 0) {
    stop(paste("Measure IDs don't exist:", paste(measure_ids, collapse = ", ")))
  }
  if (is.null(provider_ids)) {
    provider_ids <- hospital_info_df$PROVIDER_ID
  }
  df <- hospitalstars::star_algorithm_input_df
  df <- df[df$PROVIDER_ID %in% provider_ids, ]
  data.table::melt(
    data.table(df[, c("report_date", "PROVIDER_ID", measure_cols)]),
     id.vars = c("report_date", "PROVIDER_ID"),
     variable.name = "measure_id", value.name = "score"
  )
}



