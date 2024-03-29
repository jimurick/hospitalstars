% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/star_computation.R
\name{sas_step2_groups}
\alias{sas_step2_groups}
\title{Step 2 of the star rating algorithm - Calculate group-level scores}
\usage{
sas_step2_groups(std_data_dt, measure_ids, star_version = "v202307")
}
\arguments{
\item{std_data_dt}{cleaned & standardized data.table from step 1}

\item{measure_ids}{the measure_ids with enough observed values to be included}

\item{star_version}{One of "v202307" (the default), "v202207" or "v202104"}
}
\value{
a data.table
}
\description{
Step 2 of the star rating algorithm - Calculate group-level scores
}
