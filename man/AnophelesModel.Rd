% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnophelesModel.r
\name{AnophelesModel}
\alias{AnophelesModel}
\title{main function of the AnophelesModel package}
\usage{
AnophelesModel(
  mosquito_species = "Anopheles gambiae",
  activity_patterns = "default_Anopheles_gambiae",
  n_time_points = 100,
  total_pop = 2000,
  n_vec = 10000,
  interventions = intervention_obj_examples,
  coverages = c(seq(0, 0.9, by = 0.1), 0.95, 0.99),
  plot_result = TRUE
)
}
\arguments{
\item{mosquito_species}{string corresponding to the name of the mosquito
species to load the bionomic parameters for. This argument is case
sensitive and must be one of the species provided with the package:
\emph{Anopheles gambiae}, \emph{Anopheles albimanus}, etc.
To see the available mosquito species in th package,
you can use the function \code{list_all_species()}.}

\item{activity_patterns}{can be either a string for available default values:
"default_Anopheles_gambiae", "default_Anopheles_albimanus"
or a list object with the following attributes:
\itemize{
\item \code{HBI}: proportion of mosquito biting of humans indoors
\item \code{HBO}: propotion of mosquito biting of humans outdoors
\item \code{humans_indoors}: proportion of humans indoors
\item \code{humans_in_bed}: proportion of humans in bed
}
These attribues can be either time series with the corresponding values at
different consecutive time points, or characters indicating
an entry ID in the package
database. The function \code{list_rhythms()} can be used to visualize the
available database entries with activity patterns of mosquitoes and
humans for various geographical locations.}

\item{n_time_points}{number of time points to be considered, default is 100}

\item{total_pop}{total number of hosts, default is 2000}

\item{n_vec}{number of mosquitoes born and entering the host-seeking state
per day, default is 10000}

\item{interventions}{list of intervention objects. Each intervention object
is a list itself with the following elements (attributes):
 \itemize{
 \item \code{id}: string with the intervention id. To use an intervention
 whose parameterisation is provided in the package database, \code{id} must
 be one of "IRS", "LLINs", or "House_screening".
 \item \code{description}: string containing a short description of the
 intervention, can be used to differentiate between
 interventions which have the same id;
 \item \code{parameterisation}: string corresponding to the name of the
 intervention parameterisation to be used to calculate intervention effects.
 To use a parameterisation available in the
 package database, this attribute needs to match one of the parameterisations
 (column "Parameterisation") included in the intervention summary data table,
 \code{interventions_param$interventions_summary}.
 For a parameterisation not present in the package database, this
 argument can contain any name chosen by the user and the attribute
 \code{effects} needs to be provided.
 \item \code{effects}: list of intervention survival and effects on the
 mosquito oviposition cycle; This attribute is \strong{optional}
 if \code{parameterisation} matches one of the intervention parameterisations
 available in the package database.

 This attribute allows defining intervention effects which are not in
 the package database. Most of these effects are defined at
 consecutive time points which are
 equally distributed throughout the duration of the intervention.
 The number of time points needs to match the provided argument
 \code{n_time_points}. Except for \code{survival} which is a vector,
 these effects are represented by 3-column matrices, where each column,
 \code{i=\{1, 2, 3\}}, corresponds to protected human hosts, unprotected
 human hosts and animal hosts, respectively. The effects are defined
 thorugh the following list attributes:
 \itemize{
 \item \code{Kvi}: proportion of susceptible mosquitoes that become
 infected after biting a host of type i, contains one row and 3 columns
 \item \code{alphai}: availability of host i to mosquitoes, contains
 \code{n_time_points} rows and 3 columns
 \item \code{PBi}: probability that a mosquito bites a host of type i,
 contains \code{n_time_points} rows and 3 columns
 \item \code{PCi}: probability that a mosquito finds a resting place after
biting a host of type i, contains \code{n_time_points} rows and 3 columns
 \item \code{PDi}: probability that a mosquito survives the resting phase
after biting a host of type i, contains \code{n_time_points}
rows and 3 columns
 \item \code{PEi}: probability that a mosquito lays eggs and
returns to host-seeking after biting a host of type i,
contains \code{n_time_points} rows and 3 columns
 \item \code{survival}: survival of the intervention,
 vector of length \code{n_time_points}
 }
 }
 If the attribute \code{id} is "LLINs" and a parameterisation from
 the package is used, the following additional
 attributes for the LLIN intervention object must be provided:
 \itemize{
 \item \code{type}: type of mosquito net
 \item \code{country}: country where the data for the chosen net type
 was collected.
 \item \code{insecticide}: type of insecticide
 }
 To list all available LLIN types and corresponding countries,
 use the function \code{get_net_types()}. For all available insecticide
 types, see the data table
 \code{interventions_param$LLINs_params$insecticide_c}.}

\item{coverages}{vector of deployment coverages to be considered for the
interventions, default value is \{0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
0.8, 0.9, 0.95, 0.99\}}

\item{plot_result}{if TRUE, the function plots the reduction of the vectorial
capacity for each intervention in the \code{interventions} list}
}
\value{
list object with the following attributes:
\itemize{
\item \code{vec_p}: list of vector-specific entomological parameters:
\itemize{
\item \code{species_name}: name of the mosquito species
\item \code{M}: parous rate (proportion of host seeking mosquitoes that have
laid eggs at least once)
\item \code{Chi}: human blood index (proportion of mosquito blood meals
derived from humans)
\item \code{A0}: sac rate (proportion of mosquitoes who laid eggs the same day)
\item \code{zeta.3}: relative availability of different non-human hosts
\item \code{td}: proportion of a day that a mosquito actively seeks a host
\item \code{tau}: time required for a mosquito that has encountered a host to
return to host seeking
\item \code{ts}: duration of the extrinsic incubation period (time required
for sporozoites to develop in mosquitoes)
\item \code{endophily:} proportion of indoor resting mosquitoes
\item \code{endophagy:} proportion of indoor feeding mosquitoes
}

\item \code{host_p}: list of host-specific entomological parameters:
\itemize{
\item \code{PBi}: probability that a mosquito bites host i
\item \code{PCi}: probability that a mosquito finds a resting place after
biting a host of type i
\item \code{PDi}: probability that a mosquito survives the resting phase
after biting a host of type i
\item \code{Kvi}:proportion of susceptible mosquitoes that become infected
after biting any host of type i
}
Each vector has three elements corresponding to the probability values for:
(1) humans protected by interventions, (2) humans not protected by
interventions, and (3) animals.
In absence of interventions, the first two values of each
vector are identical.

\item \code{interventions_vec}: list of intervention objects including their
specifications and effects on the vectorial capacity. It contains all the
attributes defined above for the \code{interventions} argument of the
function. In addition, the \code{effects} list contains the following
attributes:
\itemize{
\item \code{alphai_decay}: vector of length \code{n_time_points} with the
 decay of the availability to mosquitoes of humans protected by the
 intervention
 \item \code{PBi_decay}: vector of length \code{n_time_points} with the
 decay of the probability that a mosquito bites humans protected by the
 intervention (preprandial effect)
 \item \code{PCi_decay}: vector of length \code{n_time_points} with the
 decay of the probability that a mosquito finds a resting place after
 biting humans protected by the intervention (postprandial effect)
\item{vc}: matrix containing the vectorial capacity at each time point
(\code{n_time_points} rows) and for the specified intervention coverages
(number of columns equal to the length of the \code{coverages} vector)
\item{impact} matrix containing the proportion of reduction of the vectorial
capacity obtained at each time point (\code{n_time_points} rows) relative
to the vectorial capacity before intervention deployment and for the
specified intervention coverages (number of columns equal to the length of
the \code{coverages} vector)
\item{avg_vc}: average vectorial capacity across all time points and for
the specified intervention coverages
(length equal to the length of the \code{coverages} vector)
\item{avg_impact}: average vectorial capacity reduction across all
time points and for the specified intervention coverages
(length equal to the length of the \code{coverages} vector)
}
}
}
\description{
\code{AnophelesModel} calculates, for a given \emph{Anopheles}
mosquito species, the vectorial capacity reduction obtained for a given
list of interventions and their deployment coverages.
}
\examples{
AnophelesModel()

}
\author{
Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}

Olivier Briët, \email{olivier.briet@swisstph.ch}

Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}

Tom Smith, \email{thomas.smith@swisstph.ch}
}
