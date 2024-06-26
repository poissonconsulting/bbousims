% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/demography.R
\name{bbs_demographic_summary}
\alias{bbs_demographic_summary}
\title{Demographic summary}
\usage{
bbs_demographic_summary(
  calves_per_adult_female = 0.7,
  survival_adult_female = 0.85,
  survival_calf = 0.5,
  survival_yearling = survival_adult_female,
  proportion_female = 0.5
)
}
\arguments{
\item{calves_per_adult_female}{A number of the calves per adult female.}

\item{survival_adult_female}{A number between 0 and 1 of the annual female adult survival.}

\item{survival_calf}{number between 0 and 1 of the annual calf survival.}

\item{survival_yearling}{A number between 0 and 1 of the annual yearling survival.}

\item{proportion_female}{A number between 0 and 1 indicating the proportion of recruits that are female.}
}
\value{
A named list of the calf-cow ratio, DeCesare recruitment, leslie matrix, lambda estimate and stable-stage distribution.
}
\description{
Generate calf-cow ratio, DeCesare recruitment, lambda, leslie matrix and stable-stage distribution
from demographic parameters.
}
\examples{
bbs_demographic_summary()
}
