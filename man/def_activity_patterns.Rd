% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnophelesModel_init.R
\name{def_activity_patterns}
\alias{def_activity_patterns}
\title{defines the activity patterns of vectors and hosts}
\usage{
def_activity_patterns(activity = "default_Anopheles_gambiae")
}
\arguments{
\item{activity}{can be either a string corresponding to default entries:
"default_Anopheles_gambiae", "default_Anopheles_albimanus"
or a list object with the following attributes:
\itemize{
\item \code{HBI}: ID or proportion of human biting indoors
\item \code{HBO}: ID or propotion human biting outdoors
\item \code{humans_indoors}: ID or proportion of humans indoors
\item \code{humans_in_bed}: ID or proportion of humans in bed
}
These attribues can be either vectors with the corresponding values at
each time point, or an ID corresponding to an entry in the package
database object \code{activity_patterns}.
The function list_activity() can be used to retrieve the
available entries for all or specific geographical locations.}
}
\value{
list object with the time series attributes:
\itemize{
\item \code{HBI}: proportion of human biting indoors
\item \code{HBO}: propotion human biting outdoors
\item \code{humans_indoors}: proportion of humans indoors
\item \code{humans_in_bed}:proportion of humans in bed
}
}
\description{
\code{def_activity_patterns} This function creates an object
that contains information about mosquito and human activity:
the biting rhythms of the mosquitoes and the times when humans
are indoors or in bed.
}
\examples{
# Retrieve default human and mosquito activity patterns
default_rhythms = def_activity_patterns("default_Anopheles_gambiae")
print(default_rhythms)

}
\author{
Monica Golumbeanu, \email{monica.golumbeanu@unibas.ch}

Olivier Briët

Nakul Chitnis, \email{nakul.chitnis@unibas.ch}

Tom Smith, \email{thomas.smith@unibas.ch}
}
