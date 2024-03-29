% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnophelesModel_xml.R
\name{get_OM_ento_snippet}
\alias{get_OM_ento_snippet}
\title{creates an xml snippet to be included in an OpenMalaria scenario}
\usage{
get_OM_ento_snippet(vec_params, hosts_params)
}
\arguments{
\item{vec_p}{list of vector-specific entomological parameteres (see
\code{def_vector_params} for creating this object)}

\item{hosts_p}{list of host-specific entomological parameters (see
\code{def_host_params} for creating this object)}
}
\value{
list with two xml snippets:
\itemize{
\item \code{<mosq>}: mosquito species parameters describing the
lifecycle and behaviour
\item \code{<nonHumanHosts>}: non-human parameters to allow for zoophily
}
}
\description{
\code{get_OM_ento_snippet} Once the vector and host parameters
defined, this function can be used to generate
the xml tags for the <entomology> component of the scenario xml file
required as input for running the OpenMalaria agent-based simulation model
of malaria epidemiology. It creates the <mosq> element containing
parameters describing the mosquito life cycle, as well as the <nonHumanHosts>
element with parameters for non-human hosts.
}
\examples{
vec_params = def_vector_params()
hosts_params = def_host_params()
xml_snippets = get_OM_ento_snippet(vec_params, hosts_params)
print(xml_snippets)

}
\author{
Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
}
