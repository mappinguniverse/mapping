\name{world_low}
\alias{world_low}

\docType{data}

\title{Worldwide Coordinates}
\description{Dataset containing worldwide names and coordinates.}

\usage{data("world_low")}
\format{
  A data.frame with 251 observations on the following 20 variables.
  \describe{
    \item{\code{country}}{a character vector}
    \item{\code{name_formal}}{a character vector}
    \item{\code{name_wb}}{a character vector}
    \item{\code{iso2}}{a character vector}
    \item{\code{iso3}}{a character vector}
    \item{\code{iso3_eh}}{a character vector}
    \item{\code{iso3_numeric}}{a character vector}
    \item{\code{iso3_un}}{a character vector}
    \item{\code{iso2_wb}}{a character vector}
    \item{\code{iso3_wb}}{a character vector}
    \item{\code{continent}}{a factor with levels \code{Africa}
    \code{Asia} \code{Europe}, \code{North America}, \code{Oceania}, \code{Seven seas (open ocean)}, \code{South America}}
    \item{\code{region}}{a factor with levels \code{Africa}, \code{Americas} \code{Asia}, \code{Europe}, \code{Oceania}, \code{Seven seas (open ocean)}}
    \item{\code{subregion}}{a factor with levels \code{Australia and New Zealand} \code{Caribbean} \code{Central America} \code{Central Asia} \code{Eastern Africa} \code{Eastern Asia} \code{Eastern Europe} \code{Melanesia} \code{Micronesia} \code{Middle Africa} \code{Northern Africa} \code{Northern America} \code{Northern Europe} \code{Polynesia} \code{Seven seas (open ocean)} \code{South America} \code{South-Eastern Asia} \code{Southern Africa} \code{Southern Asia} \code{Southern Europe} \code{Western Africa} \code{Western Asia} \code{Western Europe}}
    \item{\code{region_wb}}{a factor with levels \code{Antarctica} \code{East Asia & Pacific} \code{Europe & Central Asia} \code{Latin America & Caribbean} \code{Middle East & North Africa} \code{North America} \code{South Asia} \code{Sub-Saharan Africa}}
    \item{\code{type}}{a factor with levels \code{Country} \code{Dependency} \code{Disputed} \code{Lease} \code{Sovereign country}}
    \item{\code{type_economy}}{a factor with levels \code{Developed region:} \code{Developed region: n} \code{Developed region: no} \code{Developing} \code{Developing region} \code{Emerging r} \code{Emerging region: } \code{Emerging region: BR} \code{Emerging region: G2} \code{Emerging region: G20} \code{Emerging region: MI} \code{Least deve} \code{Least developed reg} \code{Least developed regi}}
    \item{\code{type_income}}{a factor with levels \code{High income: nonOECD} \code{High income: OECD} \code{Low income} \code{Lower middle income} \code{Upper middle income}}
    \item{\code{type_economy_id}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7}}
    \item{\code{type_income_id}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5}}
    \item{\code{geometry}}{a sfc_GEOMETRY}
  }
}

\source{
\url{https://datacatalog.worldbank.org}
}


\keyword{datasets}
