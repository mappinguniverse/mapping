\name{it_2021}

\alias{it_2021_provincia}
\alias{it_2021_regione}
\alias{it_2021_ripartizione}

\docType{data}
\title{Italian Coordinates}
\description{Names and coordinates for Italy.}

\usage{
data("it_2021_provincia")
data("it_2021_regione")
data("it_2021_ripartizione")}

\format{
  A data.frame with 7 variables.
  \describe{
    \item{\code{ripartizione}}{a character vector}
    \item{\code{regione}}{a character vector}
    \item{\code{codice.ripartizione}}{a numeric vector}
    \item{\code{codice.regione}}{a numeric vector}
    \item{\code{provincia}}{a character vector}
    \item{\code{sigla}}{a character vector}
    \item{\code{geometry}}{a sfc_MULTIPOLYGON}
  }
}

\source{Istat}


\keyword{datasets}
