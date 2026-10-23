#' @title Vector-specific estimated entomological parameters
#' @description Bionomic parameterisations for 57 \emph{Anopheles} species,
#' 17 complexes and the genus, from the AnophelesBionomics package
#' (\url{https://github.com/SwissTPH/AnophelesBionomics}; "Estimating mosquito
#' bionomics parameters with a hierarchical Bayesian model", Current Research
#' in Parasitology & Vector-Borne Diseases, 2026). Each parameter is estimated
#' with a hierarchical Bayesian model (genus, complex, species) from published
#' field data; the values are posterior means and standard deviations.
#'
#' Taxa are identified by \code{species_name} \strong{and} \code{level}.
#' Complexes are named by their label (e.g. \code{"Gambiae"}; \code{"Funestus"}
#' is a group). A species alone in its complex is listed once, as a species
#' (no complex row). \emph{An. gambiae s.s.} and \emph{An. coluzzii} are
#' merged as \code{"Anopheles gambiae ss/coluzzii"}. Taxa without data for a
#' parameter carry the value imputed from their complex or the genus, flagged
#' in \code{imputed_traits}. \emph{An. freeborni} and \emph{An. nivipes} have
#' no parent complex in the AnophelesBionomics taxonomy.
#'
#' The dataset is regenerated with \code{data-raw/vec_ent_param_from_AnophelesBionomics.R}
#' (from the AnophelesBionomics posterior draws) and \code{data-raw/vec_ent_param.R};
#' its provenance is stored in \code{attr(vec_ent_param, "source")}.
#'
#' @format A \code{data frame} with the following columns:
#' \itemize{
#' \item \code{species_name:} name of the taxon
#' \item \code{level:} \code{"species"}, \code{"complex"} or \code{"genus"}
#' \item \code{complex:} parent complex of a species (\code{NA} otherwise)
#' \item \code{imputed_traits:} parameters without data for this taxon,
#' separated by \code{";"} (empty if all are estimated from its own data)
#' \item \code{M}, \code{M.sd:} parous rate (proportion of host-seeking
#' mosquitoes that have laid eggs at least once)
#' \item \code{Chi}, \code{Chi.sd:} human blood index used by the model
#' (proportion of blood meals taken on humans); by default the indoor human
#' blood index (see \code{hbi} in \code{\link{def_vector_params}})
#' \item \code{HBI_indoor}, \code{HBI_indoor.sd}, \code{HBI_outdoor},
#' \code{HBI_outdoor.sd:} indoor and outdoor human blood indices
#' \item \code{A0}, \code{A0.sd:} sac rate (proportion of mosquitoes who laid
#' eggs the same day)
#' \item \code{zeta.3:} relative availability of different non-human hosts
#' (fixed to 1)
#' \item \code{td:} proportion of a day that a mosquito actively seeks a host
#' (fixed to 0.33)
#' \item \code{tau}, \code{tau.sd:} resting duration (days), time required for
#' a mosquito that has encountered a host to return to host-seeking. Estimated
#' as \eqn{1/q}, with \eqn{q = fed / (fed + gravid)} the proportion of fed
#' females among fed and gravid females in field resting collections (i.e.
#' assuming a one-day fed stage). This may underestimate the resting duration
#' if gravid females are under-sampled indoors or if digestion lasts more than
#' a day. Rounded to whole days in the survival over the extrinsic incubation
#' period.
#' \item \code{ts:} duration of the extrinsic incubation period (fixed to 10)
#' \item \code{to:} oocyst development time (fixed to 5)
#' \item \code{endophily}, \code{endophily.sd:} proportion of indoor resting
#' mosquitoes
#' \item \code{endophagy}, \code{endophagy.sd:} proportion of indoor feeding
#' mosquitoes
#' }
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Clara Champagne, \email{clara.champagne@swisstph.ch}
#'
#' @seealso \code{\link{def_vector_params}}, \code{\link{list_all_species}},
#' \code{\link{read_bionomics_table}}
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
#' PBi(probability that a mosquito that has encountered a host of type i bites
#' it), PCi(probability that a
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


#' @title ITN efficacy parameterisations from experimental hut trials
#'
#' @description Efficacy estimates and per-component insecticide half-lives
#' for pyrethroid-only and next-generation ITNs, fitted to experimental hut
#' trial (EHT) data from multiple sites (Champagne et al. 2025).
#'
#' Efficacy components are provided for the standard three-host model
#' (\code{Repellent}, \code{Preprandialkilling}, \code{Postprandialkilling})
#' and the dummy-host model (\code{KillingDuringHostSeeking}).
#'
#' Use \code{\link{get_ITN_params}} to load a specific net type / EHT
#' combination into a format ready for \code{\link{build_intervention}}.
#'
#' @format A data frame with columns:
#' \itemize{
#'   \item \code{netType}: ITN product name
#'     (e.g. \code{"Interceptor G2"}, \code{"Pyrethroid-only"}).
#'   \item \code{EHT}: experimental hut trial site identifier.
#'   \item \code{reference}: citation for the underlying EHT data.
#'   \item \code{parameter}: efficacy component
#'     (\code{Repellent}, \code{Preprandialkilling},
#'      \code{Postprandialkilling}, \code{KillingDuringHostSeeking},
#'      \code{alpha_0}, \code{mu_0}).
#'     \code{KillingDuringHostSeeking} is relative to the control-arm
#'     availability \code{alpha_0} (rows with \code{netType == "control"});
#'     \code{\link{get_ITN_params}} rescales it to an absolute value.
#'   \item \code{Unwashed}: fitted efficacy for a new (unwashed) net.
#'   \item \code{Washed}: fitted efficacy after washing / ageing.
#'   \item \code{aged_time}: years represented by the washed arm (3, or 2
#'     for Sovegnon).
#'   \item \code{halflife_insecticide}: per-component linear insecticide
#'     decay (years), computed as \code{aged_time / (1 - Washed / Unwashed)}.
#'     Despite the name, it is the time at which the effect reaches zero under
#'     linear decay, calibrated so that efficacy after \code{aged_time} years
#'     equals \code{Washed}. Negative values indicate the component efficacy
#'     increased with washing; they are kept as fitted (the effect then
#'     increases linearly over time).
#'   \item \code{fit}: fit of the trial the row belongs to (1, or 1 and 2
#'     when a trial was fitted in several parts, each with its own control,
#'     e.g. Sovegnon); host-seeking killing is rescaled by the control
#'     \code{alpha_0} of the same fit.
#' }
#'
#' @author Clara Champagne, \email{clara.champagne@swisstph.ch}
#'
#' @references Champagne et al. (2025) — in preparation.
#'
#' @examples
#' head(ITN_parameterisations_Champagne2025)
#' list_ITN_params()
#'
#' @return ITN efficacy parameters from experimental hut trials.
#'
"ITN_parameterisations_Champagne2025"
