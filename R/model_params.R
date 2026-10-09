# This file contains all the package functions necessary for initializing the
# model parameters (vector, host and activity rhythms parameters) as well as
# defining the intervention effects

#########def_vector_params############
#' @title define the vector-specific entomological parameters
#'
#' @description \code{def_vector_params} This function can be used to load
#' entomological parameters for a mosquito taxon (species, complex or genus)
#' from the package database (or another bionomics table), or to set the
#' entomological parameters to new custom values provided by the user.
#'
#' Taxa are identified by their name \strong{and} level: the same name can
#' denote a species and a complex. \emph{An. gambiae s.s.} and \emph{An.
#' coluzzii} are merged in the data as \code{"Anopheles gambiae ss/coluzzii"};
#' the names \code{"Anopheles gambiae"}, \code{"Anopheles gambiae s.s."} and
#' \code{"Anopheles coluzzii"} are accepted for it with a warning. Complexes
#' are stored under their label (e.g. \code{"Gambiae"}); \code{"Anopheles
#' gambiae complex"} is also accepted with \code{level = "complex"}.
#'
#' @param mosquito_species string, name of the taxon. Default
#' \code{"Anopheles gambiae ss/coluzzii"}. Use \code{list_all_species()} to see
#' the available taxa.
#'
#' @param level taxonomic level of \code{mosquito_species}: \code{"species"}
#' (default), \code{"complex"} or \code{"genus"}.
#'
#' @param hbi human blood index used as \code{Chi}: \code{"indoor"} (default,
#' indoor HBI), \code{"outdoor"} (outdoor HBI) or \code{"weighted"}
#' (\code{HBI_indoor * endophagy + HBI_outdoor * (1 - endophagy)}; its
#' standard deviation is approximated assuming independent estimates). For a
#' custom \code{vector_table}, \code{Chi} is used as supplied unless
#' \code{hbi} is \code{"outdoor"} or \code{"weighted"}.
#'
#' @param vector_table data frame with custom vector entomological values.
#' Must be provided if custom parameter values should be used instead of
#' the ones in the package database.
#' The data frame should use (a subset of) the column names of the database
#' object \code{vec_ent_param}. Default value is NULL.
#'
#' @param bionomics_table optional bionomics table with the same structure as
#' \code{vec_ent_param} (e.g. from \code{read_bionomics_table()}), used instead
#' of the package database to look up \code{mosquito_species}.
#'
#' @param verbose boolean specifying whether any messages should be displayed
#' during the execution of the function. Default value is TRUE.
#'
#' @return list object with the following attributes
#' (same as \code{vec_ent_param}, plus \code{hbi_source}):
#' \itemize{
#' \item \code{species_name}, \code{level}, \code{complex}: taxon
#' \item \code{imputed_traits}: parameters without data for this taxon
#' \item \code{M}: parous rate (proportion of host seeking mosquitoes that have
#' laid eggs at least once)
#' \item \code{M.sd}: standard deviation of the parous rate
#' \item \code{Chi}: human blood index (proportion of mosquito blood meals
#' derived from humans), chosen with \code{hbi}
#' \item \code{HBI_indoor}, \code{HBI_outdoor}: indoor and outdoor human
#' blood indices
#' \item \code{A0}: sac rate (proportion of mosquitoes who laid eggs in a day)
#' \item \code{A0.sd}: standard deviation of the sac rate
#' \item \code{zeta.3}: relative availability of different non-human hosts
#' \item \code{td}: proportion of a day that a mosquito actively seeks a host
#' \item \code{tau}: resting duration, time required for a mosquito that has
#' encountered a host to return to host seeking
#' \item \code{ts}: duration of the extrinsic incubation period (time required
#' for sporozoites to develop in mosquitoes)
#' \item \code{endophily:} proportion of indoor resting mosquitoes
#' \item \code{endophily.sd:} standard deviation of the endophily
#' \item \code{endophagy:} proportion of indoor feeding mosquitoes
#' \item \code{endophagy.sd:} standard deviation of the endophagy
#' \item \code{hbi_source}: which human blood index is used as \code{Chi}
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @import checkmate
#'
#' @examples
#' # Retrieve the vector entomological parameters for An. gambiae s.s./coluzzii
#' gambiae_vec_p = def_vector_params()
#'
#' # The whole Gambiae complex, using the outdoor human blood index
#' gambiae_complex = def_vector_params("Gambiae", level = "complex",
#'                                     hbi = "outdoor")
#'
#' @export
#'
def_vector_params = function(mosquito_species = "Anopheles gambiae ss/coluzzii",
                             level = "species", hbi = "indoor",
                             vector_table = NULL, bionomics_table = NULL,
                             verbose = TRUE) {
    level = match.arg(level, .TAXON_LEVELS)
    hbi   = match.arg(hbi, c("indoor", "outdoor", "weighted"))

    if(!is.null(vector_table)) {
        # Check if input is correct
        assertCol = checkmate::makeAssertCollection()
        # Input needs to be a data frame
        checkmate::assertDataFrame(vector_table, col.names = "named",
                                   add = assertCol)
        # with column names from vec_ent_param
        checkmate::assertSubset(colnames(vector_table), colnames(vec_ent_param),
                                empty.ok = FALSE, fmatch = FALSE,
                                add = assertCol)
        # and numeric, non-missing, positive entries except for the text
        # columns (taxon name, level, complex, imputed traits)
        num_table = vector_table[setdiff(colnames(vector_table),
                                         .BIONOMICS_TEXT_COLS)]
        checkmate::assertDataFrame(num_table, types = "numeric",
                                   any.missing = FALSE, add = assertCol)
        checkmate::assertNumeric(unlist(num_table), lower = 0,
                                 add = assertCol)
        checkmate::reportAssertions(collection = assertCol)

        vec_p = as.list(vector_table)
        if (hbi != "indoor") vec_p = apply_hbi(vec_p, hbi)
        if(verbose) {
            print(paste("Object with vector entomological parameters defined",
                        "using values provided by the user."))
        }
    } else {
        tab = if (!is.null(bionomics_table)) check_bionomics_table(bionomics_table)
              else vec_ent_param
        vec_p = apply_hbi(as.list(select_taxon(tab, mosquito_species, level)), hbi)
    }
    return(vec_p)
}


#########def_host_params#####
#' @title define host-specific entomological parameters
#'
#' @description \code{def_host_params} This function can be used to load
#' host-specific entomological parameters available in the package or to set
#' the entomological parameters to new custom values provided by the user.
#' Host parameters (biting, survival, and infection probabilities for humans
#' and animals) are species-independent: the same values apply regardless of
#' mosquito species.
#' @param host_table data frame with custom host-specific values.
#' Must be provided if custom parameter values should be used instead of
#' the ones in the package database.
#' The provided data frame should have the same structure (column names)
#' as the database object \code{host_ent_param}. Default value is NULL.
#' @param verbose boolean specifying whether any messages should be displayed
#' during the execution of the function. Default value is TRUE.
#'
#' @return list object with the following vector attributes:
#' \itemize{
#' \item \code{species_name}: name of the mosquito species
#' \item \code{host}: vector of hosts (should be \code{c("human", "animal")})
#' \item \code{PBi}: probability that a mosquito bites host i
#' \item \code{PCi}: probability that a mosquito finds a resting place after
#' biting a host of type i
#' \item \code{PDi}: probability that a mosquito survives the resting phase
#' after biting a host of type i
#' \item \code{Kvi}: proportion of susceptible mosquitoes that become infected
#' after biting any host of type i
#' }
#' Each vector has three elements corresponding to the probability values for:
#' (1) humans protected by interventions, (2) humans not protected by
#' interventions, and (3) animals.
#' In absence of interventions, the first two values of each
#' vector are identical.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët
#' @author Tom Smith
#' @author Nakul Chitnis
#'
#' @import checkmate
#'
#' @examples
#' # Retrieve the host-specific entomological parameters
#' hosts_p = def_host_params()
#'
#' # Print all the parameters
#' print(hosts_p)
#' @export
#'
def_host_params = function(host_table = NULL, verbose = TRUE) {
    # Parameters specified by the user
    if(!is.null(host_table)) {
        # Check if input is correct
        assertCol = checkmate::makeAssertCollection()
        # Input needs to be a data frame with two rows
        checkmate::assertDataFrame(host_table, any.missing = FALSE,
                                   col.names = "named", nrows = 2,
                                   add = assertCol)
        # with the same column names as host_ent_param
        checkmate::assertSubset(colnames(host_table), colnames(host_ent_param),
                                empty.ok = FALSE, fmatch = FALSE)
        # and numeric, positive entries less than 1 except for the name of
        # the species and the type of host
        checkmate::assertDataFrame(subset(host_table,
                                          select = -c(species_name, host)),
                                   types = "numeric", add = assertCol)
        checkmate::assertNumeric(unlist(subset(host_table, select =
                                                   -c(species_name, host))),
                                 lower = 0, upper = 1,
                                 add = assertCol)
        # with values provided for both human and animal hosts
        checkmate::assertSetEqual(host_table$host, c("human", "animal"),
                                  add = assertCol)
        checkmate::reportAssertions(collection = assertCol)

        # Model calculations use positional host parameters: humans first,
        # animals second, regardless of the input table's row order.
        host_table = host_table[order(match(host_table$host, c("human", "animal"))), ]

        if (verbose) {
            print(paste("Using host-specific entomological parameters",
                        "provided by the user."))
        }
        host_param = as.list(host_table)
    }
    else {
        # Load the first human and animal rows (values are species-independent)
        host_p = host_ent_param[host_ent_param$host %in% c("human", "animal"), ]
        host_p = host_p[!duplicated(host_p$host), ]
        host_p = host_p[order(match(host_p$host, c("human", "animal"))), ]
        host_param = as.list(host_p)
    }

    return(host_param)
}

#########def_activity_patterns#####
#' @title defines the activity patterns of vectors and hosts
#'
#' @description \code{def_activity_patterns} This function creates an object
#' that contains information about mosquito and human activity:
#' the biting rhythms of the mosquitoes and the times when humans
#' are indoors or in bed. Pass the result to \code{calculate_exposure()} to
#' derive the exposure factor applied to coverage.
#' @param activity can be either a string corresponding to default entries:
#' "default_Anopheles_gambiae", "default_Anopheles_albimanus"
#' or a list object with the following attributes:
#' \itemize{
#' \item \code{HBI}: ID or proportion of human biting indoors
#' \item \code{HBO}: ID or propotion human biting outdoors
#' \item \code{humans_indoors}: ID or proportion of humans indoors
#' \item \code{humans_in_bed}: ID or proportion of humans in bed
#' }
#' These attribues can be either vectors with the corresponding values at
#' each time point, or an ID corresponding to an entry in the package
#' database object \code{activity_patterns}.
#' The function list_activity() can be used to retrieve the
#' available entries for all or specific geographical locations.
#'
#' @return list object with the time series attributes:
#' \itemize{
#' \item \code{HBI}: proportion of human biting indoors
#' \item \code{HBO}: propotion human biting outdoors
#' \item \code{humans_indoors}: proportion of humans indoors
#' \item \code{humans_in_bed}:proportion of humans in bed
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@unibas.ch}
#' @author Olivier Briët
#' @author Nakul Chitnis, \email{nakul.chitnis@unibas.ch}
#' @author Tom Smith, \email{thomas.smith@unibas.ch}
#'
#' @import checkmate
#'
#' @examples
#' # Retrieve default human and mosquito activity patterns
#' default_rhythms = def_activity_patterns("default_Anopheles_gambiae")
#' print(default_rhythms)
#'
#' @export
#'
def_activity_patterns = function(activity = "default_Anopheles_gambiae") {
    activity_patterns = NULL
    if(is.character(activity)) {
        # Use default activity patterns
        if ("default_Anopheles_gambiae" %in% activity) {
            activity_patterns$HBI = define_activity(62)
            activity_patterns$HBO = define_activity(193)
            activity_patterns$humans_indoors = define_activity(4)
            activity_patterns$humans_in_bed = define_activity(24)
        } else if ("default_Anopheles_albimanus" %in% activity) {
            activity_patterns$HBI = define_activity(319)
            activity_patterns$HBO = define_activity(340)
            activity_patterns$humans_indoors = define_activity(4)
            activity_patterns$humans_in_bed = define_activity(24)
        } else {
            stop(paste("Unknown set of activity patterns named", activity))
        }
    } else {
        # Use custom patterns or select by ID
        activity_patterns$HBI = define_activity(activity$HBI)
        activity_patterns$HBO = define_activity(activity$HBO)
        activity_patterns$humans_indoors =
            define_activity(activity$humans_indoors)
        activity_patterns$humans_in_bed =
            define_activity(activity$humans_in_bed)
    }
    # Check if all time series have the same length
    assertSetEqual(length(unique(lengths(activity_patterns))), 1)

    return(activity_patterns)
}

#########build_model_obj######
#' @title Incorporate vector and host parameters in the entomological model
#'
#' @description \code{build_model_obj} This function incorporates defined
#' vector and host parameters in the entomological model of mosquito
#' oviposition cycle and builds a comprehensive object containing all model
#' parameters. Additional parameters are calculated and included in the object.
#' These include mosquito death and availability rate for three different types
#' of hosts: humans protected by intervention, unprotected humans and animals.
#' The function should be applied only on list objects created by the
#' package functions \code{def_vector_params} and \code{def_host_params}.
#' Functionality using other customized objects is not guaranteed.
#'
#' Activity patterns are not stored in the model object; pass them directly to
#' \code{calculate_exposure()} where they are used.
#'
#' @param vec_p object containing vector-specific entomological parameters
#' created with the function \code{def_vector_params}
#' @param hosts_p object containing host-specific entomological parameters
#' created with the function \code{def_host_params}
#'
#' @return list object containing the following components:
#' \itemize{
#' \item \code{vec_params}: object with mosquito-specific entomological
#' parameters (use function \code{def_vector_params} to generate object)
#' \item \code{host_params}: object with host-specific entomological parameters
#' (use function \code{def_hosts_params} to generate object).
#' In addition to previously-defined parameters, it contains:
#' \itemize{
#' \item \code{muvA}: mosquito death rate
#' \item \code{alphai}: per-individual-host availability to mosquitoes
#' }
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @import checkmate
#'
#' @examples
#' # Retrieve the species-specific entomological parameters for An. gambiae
#' gambiae_vec_p = def_vector_params("Anopheles gambiae")
#' # Retrieve the host-specific entomological parameters
#' gambiae_hosts_p = def_host_params()
#' # Build model object
#' model_p = build_model_obj(gambiae_vec_p, gambiae_hosts_p)
#' print(model_p)
#' @export
#'
build_model_obj = function(vec_p, hosts_p) {
    hosts_p = calc_hosts_ent_params(vec_p, hosts_p)
    return(model_param = list(vec_params = vec_p, host_params = hosts_p))
}

