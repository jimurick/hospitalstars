% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_computation.R
\name{compute_star_scores}
\alias{compute_star_scores}
\title{Compute star scores}
\usage{
compute_star_scores(sas_input_df, star_version = "202207")
}
\arguments{
\item{sas_input_df}{Input data, with approximate the same format as in the
example files for the 2021-04 or 2022-07 versions of the SAS package}

\item{star_version}{Either "202207" (the default) or "202104"}
}
\value{
The input table with star computation results added in the far right
    columns
}
\description{
Compute star scores
}
