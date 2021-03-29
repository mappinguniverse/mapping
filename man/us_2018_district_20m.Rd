\name{us_2018}
\alias{us_2018_district_20m}
\alias{us_2018_county_20m}
\alias{us_2018_region_20m}
\alias{us_2018_state_20m}
\alias{us_2018_district_20m}
\alias{us_2018_state56_5m}

\docType{data}
\title{USA Coordinates}

\description{Names and coordinates for USA.}


\usage{
data("us_2018_district_20m")
data("us_2018_county_20m")
data("us_2018_region_20m")
data("us_2018_state_20m")
data("us_2018_district_20m")
data("us_2018_state56_5m")
}
\format{
  A data.frame with  7 variables.
  \describe{
    \item{\code{state_id}}{states levels }
    \item{\code{state}}{states}
    \item{\code{state_number}}{states number}
    \item{\code{district}}{a character vector}
    \item{\code{region}}{a factor with region levels: \code{Midwest}, \code{Northeast}, \code{South}, \code{West}}
    \item{\code{division}}{a factor with division levels \code{East North Central}, \code{East South Central}, \code{Middle Atlantic}, \code{Mountain}, \code{New England}, \code{Pacific}, \code{South Atlantic}, \code{West North Central}, \code{West South Central}}
    \item{\code{geometry}}{a sfc_GEOMETRY}
  }
}

\source{
\url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}}

\keyword{datasets}
