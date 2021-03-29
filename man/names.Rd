\name{names}
\alias{namesWR}
\alias{namesEU}
\alias{namesIT}
\alias{namesUS}

\docType{data}

\title{Statistical Unit Names}
\description{Statistical unit names.}

\usage{
data("namesWR")
data("namesEU")
data("namesIT")
data("namesUS")
}


\format{A list with all names divided for year and type of units.}

\details{Look at \code{\link{getNamesWR}}, \code{\link{getNamesEU}}, \code{\link{getNamesIT}}, and \code{\link{getNamesUS}}}

\source{
\url{https://datacatalog.worldbank.org}, \url{https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries}, \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}, \url{https://www.istat.it/it/archivio/222527}
}


\examples{
data(namesWR)
str(namesWR)

data(namesEU)
str(namesEU)

data(namesIT)
str(namesIT)

data(namesUS)
str(namesUS)
}

\keyword{datasets}