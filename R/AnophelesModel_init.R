# This file contains all the package functions necessary for initializing the
# model parameters (vector, host and activity rhythms parameters) as well as
# defining the intervention effects

#########def_vector_params############
#' @title define the vector-specific entomological parameters
#'
#' @description \code{def_vector_params} This function can be used to load
#' entomological parameters for mosquito species available in the package
#' database or to set the entomological parameters to new custom values
#' provided by the user.
#'
#' @param mosquito_species string corresponding to the name of the
#' mosquito species. Default value is "Anopheles gambiae".
#' To be able to use the parameters from the package database, this name must
#' match one of the Anopheles species provided in the package.
#' To see all available Anopheles species, use the function
#' \code{list_all_species()}.
#'
#' @param vector_table data frame with custom vector entomological values.
#' Must be provided if custom parameter values should be used instead of
#' the ones in the package database.
#' The data frame should have the same structure (column names) as the database
#' object \code{vec_ent_param}. Default value is NULL.
#'
#' @param verbose boolean specifying whether any messages should be displayed
#' during the execution of the function. Default value is TRUE.
#'
#' @return list object with the following attributes
#' (same as \code{vec_ent_param}):
#' \itemize{
#' \item \code{species_name}: name of the mosquito species
#' \item \code{M}: parous rate (proportion of host seeking mosquitoes that have
#' laid eggs at least once)
#' #' \item \code{M.sd}: standard deviation of the parous rate
#' \item \code{Chi}: human blood index (proportion of mosquito blood meals
#' derived from humans)
#' \item \code{A0}: sac rate (proportion of mosquitoes who laid eggs in a day)
#' \item \code{A0.sd}: standard deviation of the sac rate
#' \item \code{zeta.3}: relative availability of different non-human hosts
#' \item \code{td}: proportion of a day that a mosquito actively seeks a host
#' \item \code{tau}: time required for a mosquito that has encountered a host to
#' return to host seeking
#' \item \code{ts}: duration of the extrinsic incubation period (time required
#' for sporozoites to develop in mosquitoes)
#' \item \code{endophily:} proportion of indoor resting mosquitoes
#' \item \code{endophily.sd:} standard deviation of the endophily
#' \item \code{endophagy:} proportion of indoor feeding mosquitoes
#' \item \code{endophagy.sd:} standard deviation of the endophagy
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @import checkmate
#'
#' @examples
#' # Retrieve the vector entomological parameters for An. gambiae
#' gambiae_vec_p = def_vector_params()
#'
#' # Print all the parameters
#' print(gambiae_vec_p)
#'
#' @export
#'
def_vector_params = function(mosquito_species = "Anopheles gambiae",
                             vector_table = NULL, verbose = TRUE) {
    if(!is.null(vector_table)) {
        # Check if input is correct
        assertCol = checkmate::makeAssertCollection()
        # Input needs to be a data frame
        checkmate::assertDataFrame(vector_table, any.missing = FALSE,
                                   col.names = "named", add = assertCol)
        # with the same column names as vec_ent_params
        checkmate::assertSubset(colnames(vector_table), colnames(vec_ent_param),
                                empty.ok = FALSE, fmatch = FALSE)
        # and numeric, positive entries except for the name of the species
        checkmate::assertDataFrame(subset(vector_table,
                                          select = -c(species_name)),
                                   types = "numeric", add = assertCol)
        checkmate::assertNumeric(unlist(subset(vector_table, select =
                                                   -c(species_name))),
                                 lower = 0,
                                 add = assertCol)
        checkmate::reportAssertions(collection = assertCol)

        vec_p = as.list(vector_table)
        if(verbose) {
            print(paste("Object with vector entomological parameters defined",
                        "using values provided by the user."))
        }
    } else {
        # First check if the given mosquito species is in the package database
        if(!(mosquito_species %in% vec_ent_param$species_name)) {
            err_msg = paste0("The provided mosquito species is not in ",
                             "the package database. To retrieve the list of",
                             "available species use list_all_species().")
            stop(err_msg)
        } else {
            vec_p = as.list(vec_ent_param[vec_ent_param$species_name ==
                                              mosquito_species,])
        }
    }
    return(vec_p)
}


#########def_host_params#####
#' @title define host-specific entomological parameters
#'
#' @description \code{def_host_params} This function can be used to load
#' host-specific entomological parameters available in the package or to set
#' the entomological parameters to new custom values provided by the user.
#' @param mosquito_species string corresponding to the name of the
#' mosquito species. Default value is "Anopheles gambiae".
#' To be able to use the parameters from the package database, this name must
#' match one of the Anopheles species provided in the package.
#' To see all available Anopheles species, use the function
#' \code{list_all_species()}.
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
#' \item \code{host}: vector of hosts (should be = {"human", "animal"})
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
#' # Retrieve the host-specific entomological parameters for An. gambiae
#' gambiae_hosts_p = def_host_params()
#'
#' # Print all the parameters
#' print(gambiae_hosts_p)
#' @export
#'
def_host_params = function(mosquito_species = "Anopheles gambiae",
                           host_table = NULL, verbose = TRUE) {
    # Parameters specified by the user
    if(!is.null(host_table)) {
        # Check if input is correct
        assertCol = checkmate::makeAssertCollection()
        # Input needs to be a data frame with two rows
        checkmate::assertDataFrame(host_table, any.missing = FALSE,
                                   col.names = "named", nrows = 2,
                                   add = assertCol)
        # with the same column names as vec_ent_params
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
        checkmate::checkSetEqual(host_table$species, c("human", "animal"))
        checkmate::reportAssertions(collection = assertCol)

        if (verbose) {
            print(paste("Using host-specific entomological parameters",
                        "provided by the user."))
        }
        host_param = as.list(host_table)
    }
    else {
        # First check if the given mosquito species is in the package database
        if(!(mosquito_species %in% host_ent_param$species_name)) {
            err_msg = paste0("No host parameters in the database for
                             mosquito_species = ", mosquito_species)
            stop(err_msg)
        }
        # Select the host parameters for the specified mosquito species
        host_p = host_ent_param[host_ent_param$species_name ==
                                    mosquito_species,]
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
#' are indoors or in bed.
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
#' @title incorporate vector, host and activity parameters in the entomological
#' model, initialize and build a model object
#'
#' @description \code{build_model_obj} This function incorporates defined
#' vector, host and activity parameters in the entomological model of mosquito
#' oviposition cycle and builds a comprehensive object containing all model
#' parameters. Additional parameters are calculated and included in the object.
#' These include mosquito death and availability rate for three different types
#' of hosts: humans protected by intervention, unprotected humans and animals).
#' The function should be applied only on list objects created by the
#' package functions \code{def_vector_params} and \code{def_host_params}.
#' Functionality using other customized objects is not guaranteed.
#' @param vec_p object containing vector-specific entomological parameteres
#' created with the function \code{def_vector_params}
#' @param hosts_p object containing host-specific entomological parameters
#' created with the function \code{def_host_params}
#' @param total_pop total population of humans and animals
#'
#' @return list object containing the following component:
#' \itemize{
#' \item \code{vec_params}: object with mosquito-specific entomological
#' parameters (use function \code{def_vector_params} to generate object)
#' \item \code{host_params}: object with host-specific entomological parameters
#' (use function \code{def_hosts_params} to generate object).
#' In addition to previously-defined parameters, it contains:
#' \itemize{
#' \item \code{muvA}: mosquito death rate
#' \item \code{alphai}: availability to mosquitoes for each host type
#' }
#' \item \code{activity}: activity patterns for mosquitoes and humans
#' (see function \code{def_activity_patterns} for object details)
#' \item \code{total_pop}: total population of humans and animals
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @import checkmate
#'
#' @examples
#' # Retrieve the species-specific entomological parameters for An. gambiae
#' gambiae_vec_p = def_vector_params("Anopheles gambiae")
#' # Retrieve the host-specific entomological parameters for An. gambiae
#' gambiae_hosts_p = def_host_params("Anopheles gambiae")
#' # Retrieve default activity patterns for humans and mosquitoes
#' default_rhythms = def_activity_patterns("default_Anopheles_gambiae")
#' # Build model object for a population of 2000 hosts
#' model_p = build_model_obj(gambiae_vec_p, gambiae_hosts_p,
#' default_rhythms, 2000)
#' print(model_p)
#' @export
#'
build_model_obj = function(vec_p, hosts_p, activity, total_pop) {
    # Calculate the mosquito death rate for biting each host type and the host
    # availability rate
    assertNumeric(total_pop, lower = 1)
    hosts_p = calc_hosts_ent_params(vec_p, hosts_p, total_pop)
    return(model_param = list(vec_params = vec_p, host_params = hosts_p,
                              activity = activity, total_pop = total_pop))
}

#########def_interventions_effects#####
#' @title define the effects of interventions
#'
#' @description This function defines the survival and
#' effects of interventions on the transition probabilities between the
#' consecutive stages of the mosquito oviposition cycle. These values are given
#' for a set of interpolation points which are
#' uniformly distributed across the duration of the intervention. To
#' define the effects, this function either uses the
#' intervention models which are available in the AnophelesModel database,
#' or directly receives the vectors with the intervention effects
#' for interventions which are not present in the database. In the latter case,
#' it is assumed that the provided intervention effects values are
#' adjusted according to the mosquito bionomics,
#' activity patterns of mosquitoes and humans, as well as
#' human exposure to mosquitoes.
#'
#' @param intervention_list list of intervention objects,
#' where each object is itself a list with the following attributes:
#' \itemize{
#' \item \code{id:} string with the intervention ID; for intervention IDs
#' available in the package, check \code{list_available_interventions()}
#' \item \code{description:} short string with the description of
#' the intervention; can be used to differentiate between
#' interventions which have the same ID
#' \item \code{parameterisation:} string corresponding to the name of the
#' intervention parameterisation to be used to calculate the transition
#' probabilities. To use a parameterisation available in the
#' package database, this attribute needs to match one of the parameterisations
#' (column "Parameterisation") retrieved with \code{list_intervention_models()}.
#' To use a new parameterisation which is
#' not present in the package database, this
#' argument can contain any name chosen by the user and the attribute
#' \code{effects} needs to be provided as well (see description below).
#' \item \code{LLIN_type:} a string containing the LLIN type,
#' only needed to be specified for LLINs interventions intending to use
#' the LLINs models parameters provided with the package
#' \item \code{LLIN_insecticide:} a string containing the LLIN insecticide type,
#' only needed for LLINs interventions intending to
#' use the LLIN models parameters provided with the package,
#' \item \code{LLIN_country:} a string containing the country where the LLIN
#' characteristics were measured, only provided for LLINs interventions
#' intending to use the LLIN models parameters provided with the package
#' \item \code{effects:} list of intervention survival and effects on the
#' mosquito oviposition cycle. This attribute allows defining intervention
#' effects which are not in the package database and is \strong{optional}
#' if \code{parameterisation} matches one of the intervention parameterisations
#' available in the package database. These effects include, in addition to the
#' availability to mosquitoes and the proportion of infected mosquitoes, the
#' transition probabilities between the consecutive stages of the
#' oviposition cycle. The decay of the effects can also be specified if
#' generation of a <GVI> xml snippet for OpenMalaria simulations is envisaged.
#' The effects are defined through the following list attributes:
#' \itemize{
#' \item \code{host_types:} for reference only, vector of strings coding the
#' different types of hosts, must be equal to
#' \{"protected humans", "unprotected humans", "animals"\}. All the remaining
#' attributes of the \code{effects} object, except for \code{survival}
#' are represented by 3-column matrices, where each column,
#' \code{j=\{1, 2, 3\}}, corresponds to values for the three types of hosts
#' defined in the same order as for \code{host_types}
#' \item \code{Kvi}: proportion of susceptible mosquitoes that become
#' infected after biting a host of type j, contains one row and 3 columns
#' \item \code{alphai}: availability of a host of type j to mosquitoes, contains
#' \code{num_ip_points} rows and 3 columns
#' \item \code{PBi}: probability that a mosquito bites a host of type j,
#' contains \code{num_ip_points} rows and 3 columns
#' \item \code{PCi}: probability that a mosquito finds a resting place after
#' biting a host of type j, contains \code{num_ip_points} rows and 3 columns
#' \item \code{PDi}: probability that a mosquito survives the resting phase
#' after biting a host of type j, contains \code{num_ip_points}
#' rows and 3 columns
#' \item \code{PEi}: probability that a mosquito lays eggs and
#' returns to host-seeking after biting a host of type j,
#' contains \code{num_ip_points} rows and 3 columns
#'  \item \code{survival}: survival of the intervention,
#'  vector of length \code{num_ip_points}
#'  \item \code{alphai_decay}: decay of intervention effect on host availability
#'  to mosquitoes, contains \code{num_ip_points} rows and 3 columns
#' \item \code{PBi_decay}: decay of intervention effect on
#' the probability that a mosquito bites a
#' host of type j, contains \code{num_ip_points} rows and 3 columns
#' \item \code{PCi_decay}: decay of intervention effect on
#' the probability that a mosquito finds a
#' resting place after biting a host of type j,
#' contains \code{num_ip_points} rows and 3 columns
#' }
#' For the above attributes with \code{num_ip_points} rows, the values are
#' provided for
#' consecutive time points equally distributed throughout the duration of the
#' intervention. The number of time points needs to match the provided argument
#' \code{n_time_points}.
#' }
#' @param model_p model object
#' (generated with function \code{build_model_obj()})
#' @param num_ip_points number of interpolation points, default is 100. If
#' custom intervention effects are specified, the relevant effect attributes
#' need to have a number of rows equal to \code{num_ip_points} (see above)
#' @param verbose boolean specifying whether any messages should be displayed
#' during the execution of the function. Default value is TRUE.
#'
#' @return list object with the following attributes:
#' \itemize{
#' \item \code{id:} string with the intervention ID
#' \item \code{description:} short string with the description of
#' the intervention
#' \item \code{parameterisation:} string corresponding to the name of the
#' intervention parameterisation
#' \item \code{LLIN_type:} only for LLINs, a string containing the LLIN type
#' \item \code{LLIN_insecticide:} only for LLINs, a string containing the
#' LLIN insecticide type
#' \item \code{LLIN_country:} only for LLINs, a string containing the country
#' where the LLIN characteristics were measured
#' \item \code{effects:} List of intervention survival and effects on the
#' mosquito oviposition cycle (same format as defined above for the input
#' attributes)
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @importFrom reshape melt
#' @import checkmate
#' @importFrom dplyr "between"
#'
#' @examples
#'
#' @export
#'
def_interventions_effects = function(intervention_list, model_p,
                                     num_ip_points = 100, verbose = TRUE,
                                     specified_multiplier = NULL) {
    # Check if inputs were provided
    assertList(intervention_list, min.len = 1)
    assertList(model_p, len = 4)
    assertNumeric(num_ip_points, lower = 3, upper = 10000)

    for (i in 1:length(intervention_list)) {
        int_obj = intervention_list[[i]]

        # Further checks of the intervention object
        assertList(int_obj, min.len = 1)
        assertSubset(c("id", "description", "parameterisation"), names(int_obj))
        if (int_obj$id == "LLINs") {
            assertSubset(c("id", "description", "parameterisation", "LLIN_type",
                            "LLIN_insecticide", "LLIN_country"), names(int_obj))
        }

        # Definition of intervention effects
        if(verbose) {
            print(paste("Defining intervention effects for", int_obj$id))
        }
        if (!is.null(int_obj$effects)) {
            # Using custom intervention effects outside the package database
            if (!check_custom_interv_effects(int_obj, num_ip_points)) {
                err_msg = paste("Error defining intervention effects.",
                                "Check the package documentation on how to",
                                "specify custom intervention effects.")
                stop(err_msg)
            } else {
                if (verbose) {
                    print("Using the provided, custom intervention effects.")
                }
            }
        } else if (!is.null(int_obj$parameterisation)){
            # Update intervention-specific parameters
            if (verbose) {
                print(paste0("Using intervention effects available for model ",
                             int_obj$parameterisation))
            }
            print(paste("def_intervention_effects", specified_multiplier))
            int_obj = calc_interv_effects_db(interv_obj = int_obj,
                                             model_p = model_p,
                                             ip = num_ip_points,
                                             specified_multiplier = specified_multiplier)
            intervention_list[[i]] = int_obj
        } else {
            err_msg = paste("Error defining intervention effects",
                            "for the specified parameterisation.",
                            "Check that the parameterisation exists and the",
                            "intervention object is defined accordingly.")
            stop(err_msg)
        }
    }
    return(intervention_list)
}
