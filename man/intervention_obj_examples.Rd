% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnophelesModel_data.R
\docType{data}
\name{intervention_obj_examples}
\alias{intervention_obj_examples}
\title{Example of interventions}
\format{
a list of three objects, where each object is a list with the
following attributes:
\itemize{
\item \code{id:} string with the intervention ID.
\item \code{description:} short string with the description of
the intervention
\item \code{parameterisation:} a string with the parameterisation code
\item \code{type:} only provided for LLINs interventions,
a string containing the LLIN type
\item \code{insecticide:} only provided for LLINs interventions,
a string containing the
insecticide type
\item \code{country:} only provided for LLINs interventions,
a string containing the country where the LLIN characteristics were measured
}
}
\usage{
intervention_obj_examples
}
\value{
intervention examples
}
\description{
Examples of intervention objects specifications for LLINs, IRS,
and hose screening.
}
\examples{
# Print the intrvention object
print(intervention_obj_examples)

}
\author{
Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
}
\keyword{datasets}
