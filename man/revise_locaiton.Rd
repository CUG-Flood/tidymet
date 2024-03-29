% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st-revise_locaiton_multi.R
\name{revise_locaiton}
\alias{revise_locaiton}
\alias{revise_locaiton_multi}
\title{Revise meteorological station's location error}
\usage{
revise_locaiton(
  d,
  prefix = "",
  dist_max = 100,
  score_min = 2,
  vars_rm = c("date_begin", "date_end", "n_all"),
  verbose = 2
)

revise_locaiton_multi(info, dist_max = 50)
}
\arguments{
\item{d}{A data.frame with the columns at least of `dist`, `lon`, `lat`, `alt`
and `n_period`, `QC`}

\item{dist_max}{If the site moving distance beyond the `dist_max`, it will
be regarded as outlier and will be fixed by [revise_location()].}

\item{score_min}{if distance score lower than `score_min`, will not fix}

\item{vars_rm}{variables not shown in the console}

\item{info}{site moving info returned by [get_moveInfo()]}
}
\value{
- sites with not outliers, QC = '';
- sites with outliers, for outliers, QC equals:
  score = 0: unfixed
  score = 1: marginal quality fixed
  score = 2: good quality fixed
}
\description{
Revise meteorological station's location error
}
\details{
Continuous moving location outliers are also considered at here.
}
\examples{
d <- st_moveInfo[site == 50772]
revise_locaiton_multi(d)

}
