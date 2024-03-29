% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnophelesModel_data.R
\docType{data}
\name{activity_patterns}
\alias{activity_patterns}
\title{Human and mosquito behavioural patterns}
\format{
A \code{data frame} with 7 columns consisting of:
\itemize{
\item \code{id:} database record ID (a unique value per time series)
\item \code{species:} for mosquito activity entries: name of the mosquito
species; for human activity entries: "Homo sapiens"
\item \code{sampling:} for mosquito activity entries can be one of:
HBI (human biting indoors),
HBO (human biting outdoors),
ABO (animal biting outdoors), or
HB (human biting); for human activity entries can be one of BED
(humans in bed) or IND (humans indoors)
\item \code{country:} the country where the measurements have been taken
\item \code{site:} the name of the location where the measurements have been
taken
\item \code{hour:} hour when the measurement was done
\item \code{value:} for mosquito activity entries: proportion of overall
mosquito bites at a given time in the corresponding location;
for human activity entries: proportion of
overall humans in the corresponding location at a given time.
}
}
\usage{
activity_patterns
}
\value{
activity cycle data
}
\description{
This data object contains time series of measurements of mosquito
biting behavior, as well as of human activity.
}
\examples{
# Print the first lines of the data frame
head(activity_patterns)

}
\references{
TO DO
}
\author{
Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}

Olivier Briët, \email{olivier.briet@swisstph.ch}

Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}

Tom Smith, \email{thomas.smith@swisstph.ch}
}
\keyword{datasets}
