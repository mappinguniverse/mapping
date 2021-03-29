\name{eu_2021_nuts3_60m}

\alias{eu_2021_nuts0_60m}
\alias{eu_2021_nuts1_60m}
\alias{eu_2021_nuts2_60m}
\alias{eu_2021_nuts3_60m}

\docType{data}
\title{European Coordinates}

\description{Names and coordinate for European countries.}

\usage{
data("eu_2021_nuts0_60m")
data("eu_2021_nuts1_60m")
data("eu_2021_nuts2_60m")
data("eu_2021_nuts3_60m")}

\format{
  A data frame with 14 variables.
  \describe{
    \item{\code{id}}{a character vector}
    \item{\code{country}}{a factor with country names}
    \item{\code{iso2}}{a factor with iso2 }
    \item{\code{iso3}}{a factor with iso3 }
    \item{\code{country_code}}{a numeric vector}
    \item{\code{nuts0_id}}{a factor with nuts0 ids}
    \item{\code{nuts1_id}}{a factor with nuts1 ids}
    \item{\code{nuts2_id}}{a factor with nuts2 ids}
    \item{\code{nuts3_id}}{a factor with nuts3 ids}
    \item{\code{nuts0}}{a factor with nuts0 names}
    \item{\code{nuts1}}{a factor with nuts1 names}
    \item{\code{nuts2}}{a factor with nuts2 names}
    \item{\code{nuts3}}{a factor with nuts3 names}
    \item{\code{geometry}}{a sfc_GEOMETRY}
  }
}

\source{
 \url{https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries}}


\keyword{datasets}
