
#' Star Rating Algorithm Input Data
#'
#' A data.frame that's a union of SAS Package input tables for each Care Compare
#' report posted by CMS since January 2018.
#'
#' @format A data.frame with 98,055 rows and 96 columns
#' \describe{
#'   \item{report_date}{The first of the month that the Care Compare report
#'     with this data was released}
#'   \item{PROVIDER_ID}{The 6-digit hospital identifier from CMS}
#'   \item{94 measure columns}{The hospital's measure scores and denominators from
#'     the given report_date}
#' }
"star_algorithm_input_df"




#' Hospital information
#'
#' One row per hospital
#'
#' @format A data.frame with 5813 rows and 39 columns
#' \describe{
#'   \item{PROVIDER_ID}{The 6-digit hospital identifier from CMS}
#'   \item{hospital_name}{The hospital's name, in caps-lock}
#'   \item{hospital_type}{One of 6 hospital types}
#'   \item{address}{Street address}
#'   \item{city}{City}
#'   \item{state}{State}
#'   \item{zip}{Zip code}
#'   \item{county}{County}
#'   \item{phone}{Phone number}
#'   \item{star_rating_yyyyMM}{The hospital's star rating listed in the Care
#'     Compare report released in the month yyyy-MM}
#'   \item{peer_group_yyyyMM}{The peer group the hospital was in for the star
#'     rating listed in the Care Compare report released in the month yyyy-MM.
#'     If the hospital had a star rating, this is a count of the number of
#'     measure groups with at least 3 measures reported.}
#' }
"hospital_info_df"




#' Measure information
#'
#' One row per measure and Care Compare report the measure appears in
#'
#' @format A data.frame of
#' \describe{
#'   \item{report_date}{}
#'   \item{measure_group}{The domain the measure is grouped in: Mortality,
#'     Readmission, Patient Experience, Safety or Process}
#'   \item{measure_id}{The measure's current ID}
#'   \item{measure_name}{A descriptive name for the measure, taken from the Care Compare reports}
#'   \item{higher_is_better}{TRUE if higher score values are better, otherwise FALSE}
#'   \item{data_start_date}{The start date of the period the measure is reporting on}
#'   \item{data_end_date}{The end date of the period the measure is reporting on}
#'   \item{data_start_quarter}{The start quarter of the period the measure is reporting on}
#'   \item{data_end_quarter}{The end quarter of the period the measure is reporting on}
#'   \item{source_filename}{The name of the file in the Care Compare report
#'     that this measure's data was found in.}
#' }
"measure_info_df"



#' SAS Package input & output data
#'
#' When new star ratings are issued, CMS posts the algorithm in the SAS Package
#' here:
#' \url{https://qualitynet.cms.gov/inpatient/public-reporting/overall-ratings/sas}
#'
#' This list contains the input and output data for the April 2021 and July 2022
#' releases of the SAS package.
#'
#' @format A list of
#' \describe{
#'   \item{key}{The month that the Care Compare report with this data was released}
#'   \item{value}{A data.frame to be used as input to the star rating algorithm,
#'     containing all hospitals' measure data from the given Care Compare report}
#' }
"sas_package"

