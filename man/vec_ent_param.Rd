% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnophelesModel_data.R
\docType{data}
\name{vec_ent_param}
\alias{vec_ent_param}
\title{Vector-specific estimated entomological parameters}
\format{
A \code{data frame} with the following columns:
\itemize{
\item \code{species_name:} name of the mosquito species
\item \code{M:} parous rate (proportion of host-seeking mosquitoes that
have laid eggs at least once)
\item \code{Chi:} human blood index (proportion of blood meals derived from
humans by mosquitoes)
\item \code{A0:} sac rate (proportion of mosquitoes who laid eggs the
same day)
\item \code{zeta.3:} relative availability of different non-human hosts
\item \code{td:} proportion of a day that a mosquito actively seeks a host
\item \code{tau:} time required for a mosquito that has encountered
a host to return to host-seeking
\item \code{ts:} duration of the extrinsic incubation period (time required
for sporozoites to develop to mosquitoes)
\item \code{to:} oocyst development time
\item \code{endophily:} proportion of indoor resting mosquitoes
#' \item \code{endophagy:} proportion of indoor feeding mosquitoes
}
}
\usage{
vec_ent_param
}
\value{
vector-specific entomological parameters
}
\description{
This data object contains bionomic parameterizations for
57 Anopheles mosquito species and 17 complexes (families of species).
These parameters have been estimated using a Bayesian hierarchical model
accounting for the phylogeny of the Anopheles genus and informed by
entomological data available from the Malaria Atlas
Project and other published field studies.
}
\examples{
# Print the first lines of the data frame
head(vec_ent_param)

}
\author{
Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
}
\keyword{datasets}
