#' @title Vector-specific estimated entomological parameters
#' @description This data object contains bionomic parameterizations for
#' 57 Anopheles mosquito species and 17 complexes (families of species).
#' These parameters have been estimated using a Bayesian hierarchical model
#' accounting for the phylogeny of the Anopheles genus and informed by
#' entomological data available from the Malaria Atlas
#' Project and other published field studies.
#'
#' @format A \code{data frame} with the following columns:
#' \itemize{
#' \item \code{species_name:} name of the mosquito species
#' \item \code{M:} parous rate (proportion of host-seeking mosquitoes that
#' have laid eggs at least once)
#' \item \code{Chi:} human blood index (proportion of blood meals derived from
#' humans by mosquitoes)
#' \item \code{A0:} sac rate (proportion of mosquitoes who laid eggs the
#' same day)
#' \item \code{zeta.3:} relative availability of different non-human hosts
#' \item \code{td:} proportion of a day that a mosquito actively seeks a host
#' \item \code{tau:} time required for a mosquito that has encountered
#' a host to return to host-seeking
#' \item \code{ts:} duration of the extrinsic incubation period (time required
#' for sporozoites to develop to mosquitoes)
#' \item \code{to:} oocyst development time
#' \item \code{endophily:} proportion of indoor resting mosquitoes
#' #' \item \code{endophagy:} proportion of indoor feeding mosquitoes
#' }
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @examples
#' # Print the first lines of the data frame
#' head(vec_ent_param)
#'
#' @return vector-specific entomological parameters
#'
"vec_ent_param"

#' @title Human and mosquito behavioural patterns
#' @description This data object contains time series of measurements of mosquito
#' biting behavior, as well as of human activity.
#'
#' @format A \code{data frame} with 7 columns consisting of:
#' \itemize{
#' \item \code{id:} database record ID (a unique value per time series)
#' \item \code{species:} for mosquito activity entries: name of the mosquito
#' species; for human activity entries: "Homo sapiens"
#' \item \code{sampling:} for mosquito activity entries can be one of:
#' HBI (human biting indoors),
#' HBO (human biting outdoors),
#' ABO (animal biting outdoors), or
#' HB (human biting); for human activity entries can be one of BED
#' (humans in bed) or IND (humans indoors)
#' \item \code{country:} the country where the measurements have been taken
#' \item \code{site:} the name of the location where the measurements have been
#' taken
#' \item \code{hour:} hour when the measurement was done
#' \item \code{value:} for mosquito activity entries: proportion of overall
#' mosquito bites at a given time in the corresponding location;
#' for human activity entries: proportion of
#' overall humans in the corresponding location at a given time.
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @references TO DO
#' @examples
#' # Print the first lines of the data frame
#' head(activity_patterns)
#'
#' @return activity cycle data
#'
"activity_patterns"

#' @title Host-specific estimated parameters for the entomological model of
#' malaria transmission
#' @description This data object contains for each of the three host types (humans
#' protected by interventions, non-protected humans, and animals) estimated
#' parameters for the entomological model of malaria transmission.
#'
#' @format A \code{data frame} with 5 columns consisting of:
#' \itemize{
#' \item \code{species name:} name of the mosquito species
#' \item \code{probability:} probabilities that a mosquito completes the next
#' stage of the oviposition cycle in one night, conditional on having reached
#' that stage. Can only be one of the following:
#' PBi(probability that a mosquito bites host of type i), PCi(probability that a
#' mosquito finds a resting place after biting a host of type i), PDi
#' (probability that a mosquito survives the resting phase after
#' biting a host of type i), PEi(probability that a mosquito lays eggs and
#' returns to host-seeking after biting a host of type i), Kvi(proportion of
#' susceptible mosquitoes that become infected after biting a host of type i)
#' \item \code{host_type_1:} value of the probability for host type 1 (humans
#' protected by the intervention)
#' \item \code{host_type_2:} value of the probability for host type 2 (humans
#' not protected by any intervention)
#' \item \code{host_type_3:} value of the probability for host type 3 (animals)
#' }
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @references TO DO
#'
#' @examples
#' # Print the first lines of the data frame object
#' head(host_ent_param)
#'
#' @return host-specific entomological parameters
#'
"host_ent_param"


#' @title General description of IRS and LLINs interventions parameterizations
#' @description This data object contains a general description of all available
#' IRS and LLIN intervention parameterizations.
#'
#' @format A \code{data frame} with 7 columns consisting of:
#' \itemize{
#' \item \code{Parameterisation:} ID of the parameterization
#' \item \code{Intervention:} name identified of the intervention, can only take
#'  value "IRS", "LLINs" or "House screening"
#' \item \code{Active agent:} insecticide name
#' \item \code{Species:} name of the mosquito species
#' \item \code{Reference:} publication reference
#' \item \code{Method:} fitting method
#' \item \code{Duration:} duration until intervention renewal
#' }
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët,
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@unibas.ch}
#'
#' @examples
#' # Print the first lines of the data frame object
#' head(interventions_param)
#'
#' @return general intervention description
#'
"interventions_param"

#' @title Example of interventions
#' @description Examples of intervention objects specifications for LLINs, IRS,
#' and hose screening.
#'
#' @format a list of three objects, where each object is a list with the
#' following attributes:
#' \itemize{
#' \item \code{id:} string with the intervention ID.
#' \item \code{description:} short string with the description of
#' the intervention
#' \item \code{parameterisation:} a string with the parameterisation code
#' \item \code{type:} only provided for LLINs interventions,
#' a string containing the LLIN type
#' \item \code{insecticide:} only provided for LLINs interventions,
#' a string containing the
#' insecticide type
#' \item \code{country:} only provided for LLINs interventions,
#' a string containing the country where the LLIN characteristics were measured
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @examples
#' # Print the intrvention object
#' print(intervention_obj_examples)
#'
#' @return intervention examples
#'
"intervention_obj_examples"

