# This file contains all the functions necessary for querying the
# package database.

#########list_all_species############
#' @title Print all the species whose entomological parameters are provided in
#' the package database.
#'
#' @description This function can be used to list
#' all mosquito species whose entomological parameters are available in the
#' package.
#'
#' @return The function prints all the species names and also
#' returns a vector containing them.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#'
#' @examples
#' species_vec = list_all_species()
#' print(species_vec)
#'
#' @export
#'
list_all_species = function() {
    species_vec = unique(vec_ent_param$species_name)
    paste(species_vec, collapse = ", ")
    return(species_vec)
}

#########list_activity############
#' @title Extract activity patterns of humans or mosquitoes
#'
#' @description This function returns the activity patterns
#' of humans or mosquitoes available in the package database based on a list of
#' provided criteria.
#'
#' @param species string corresponding to the name of the species to extract the
#' activity patterns for. For human activity patterns, this parameters should be
#' "Homo Sapiens". For mosquito activity patterns, this name must match one of
#' the Anopheles species provided in the package.
#' The default value is "Anopheles gambiae".
#' To see all available Anopheles species, use the function list_all_species().
#'
#' @param sampling can only be one of the following strings:
#' \itemize{
#' \item \code{IND}: proportion of humans indoors; only relevant for humans
#' (species parameter is "Homo Sapiens").
#' \item \code{BED}: proportion of humans in bed; only relevant for humans
#' (species parameter is "Homo Sapiens").
#' \item \code{HBI}: proportion of mosquito biting of humans occurring indoors;
#' only relevant for mosquito species.
#' \item \code{HBO}: proportion of mosquito biting of humans occurring outdoors;
#' only relevant for mosquito species.
#' #' \item \code{ABO}: proportion of mosquito biting of animals occurring
#' outdoors; only relevant for mosquito species.
#' #' \item \code{HB}: proportion of mosquito biting of humans
#' (indoors and outdoors); only relevant for mosquito species.
#' The default value is "HBI".
#' }
#'
#' @param country name of the country. Default value is NULL. If the country is
#' not provided for the provided species and
#' sampling code, then data for all available countries will be displayed.
#'
#' @param site name of the geographical location within the country.
#' Default value is NULL. If this parameter is
#' not provided for the provided species and
#' sampling code, then data for all available sites and countries will
#' be displayed).
#'
#' @return data frame with the following columns:
#' \itemize{
#' \item \code{ID}: id of the database entry
#' \item \code{species}: name of the considered species
#' \item \code{sampling}: sampling type
#' \item \code{country}: country where measurements were taken
#' \item \code{site}: geographical site where measurements were taken
#' \item \code{variable}: time points; these are chronologically ordered in the
#' output data frame
#' \item \code{value}: value of the sampling at each time point
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @examples
#' # List the proportion of human biting indoors in the Rachuonyo region, Kenya.
#' humans_indoors = list_activity(species = "Homo sapiens", sampling = "IND",
#' site="Rachuonyo")
#' print(humans_indoors)
#'
#' @export
#'
list_activity = function(species = "Anopheles gambiae", sampling = "HBI",
                        country = NULL, site = NULL) {
    rhythms_table = NULL
    if(is.null(species) | is.null(sampling)) {
        stop("ERROR: Species name and sampling type should be provided.")
    }

    # If no country and site information is provided, all entries are considered
    if(is.null(country)) {
        country = activity_patterns$country
    }
    if(is.null(site)) {
        site = activity_patterns$site
    }

    # Selecting the activity patterns based on provided criteria
    selected_patterns = activity_patterns[
        which(activity_patterns$species == species &
                activity_patterns$sampling == sampling &
                activity_patterns$country %in% country &
                activity_patterns$site %in% site), ]

    if (nrow(selected_patterns) == 0 || is.null(nrow(selected_patterns))) {
        print(paste0("ERROR: No activity patterns found in the package",
                            "database for the provided search criteria."))
    }

    return(selected_patterns)
}

#' @title Print all the interventions available in the package
#'
#' @description This function can be
#' used to list all interventions whose impact
#' models parameterisations are available in the package.
#'
#' @return The function prints all the interventions and also
#' returns a vector containing them.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#'
#' @examples
#' interv_vec = list_available_interventions()
#' print(interv_vec)
#'
#' @export
#'
list_available_interventions = function() {
    all_interv_names = interventions_param$interventions_summary$Intervention
    interv_vec = unique(all_interv_names)
    paste(interv_vec, collapse = ", ")
    return(interv_vec)
}

#' @title Print all the interventions parameterisations available in the package
#'
#' @description This function can be used to list
#' all interventions models parameterisations available in the package.
#'
#' @return The function prints all the interventions models and also
#' returns a vector containing them.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @examples
#' interv_models = list_intervention_models()
#' print(interv_models)
#'
#' @export
#'
list_intervention_models = function() {
    int_models = interventions_param$interventions_summary[,
                                        c("Parameterisation", "Intervention")]
    return(int_models)
}

#' @title Get net types
#'
#' @description This function displays a table of the
#' available LLIN types and the countries for which durability profiles
#' are available within the \code{AnophelesModel} database
#' @return The function returns a table of net types and corresponding countries
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#'
#' @examples
#' net_types = get_net_types()
#' print(net_types)
#'
#' @export
#'
get_net_types = function() {
    durability_data = interventions_param$LLINs_params$durability_estim
    net_types = unique(durability_data[, c("Net_Type", "Country"), drop = TRUE])
    rownames(net_types) = NULL
    return(net_types)
}

#' @title Get net insecticide types
#'
#' @description This function displays a table of the
#' available LLIN insecticide types
#' available within the \code{AnophelesModel} database.
#' @return The function returns a name list of net insecticide types.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#'
#' @examples
#' net_insecticide_types = get_net_insecticides()
#' print(net_insecticide_types)
#'
#' @export
#'
get_net_insecticides = function() {
    insecticide_data = interventions_param$LLINs_params$insecticide_c
    insecticide_types = colnames(insecticide_data)
    return(insecticide_types)
}

#' @title Get LLIN survival and holed area
#'
#' @description This function can be
#' used to calculate LLIN survival and holed area using parameters from
#' the \code{AnophelesModel} database for a given net type and country.
#' To list all available net types and corresponding countries,
#' use the function get_net_types().
#'
#' @param net_type type of the LLIN
#' @param country selected country of interest for which the net_type exists
#' @param insecticide_type type of insecticide
#' @param n_ips number of time points to consider throughout the duration
#' @param duration considered time of the intervention, default is 3 years
#'
#' @return The function returns an object with the following attributes:
#' \itemize{
#' \item \code{time}: time point
#' \item \code{insecticideContent}: insecticide content of the net
#' \item \code{logHoles}: log holed area of the net
#' \item \code{survival}: survival of the net
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @examples
#' net_surv = get_net_decay(net_type = "DuraNet",
#' country = "Kenya", insecticide_type = "Default", n_ips = 100, duration = 3)
#' print(net_surv)
#'
#' @export
#'
get_net_decay = function(net_type, country, insecticide_type, n_ips, duration){

    return(calc_net_decay(net_type, country, insecticide_type, n_ips, duration))
}

#' @title Calculate human exposure to biting
#'
#' @description \code{get_in_out_exp} This function can be
#' used to calculate indoor and outdoor exposure to biting.
#'
#' @param activity_cycles data frame with the following columns:
#' \itemize{
#' \item \code{HBI}: proportion of human biting indoors
#' \item \code{HBO}: propotion human biting outdoors
#' \item \code{humans_indoors}: proportion of humans indoors
#' \item \code{humans_in_bed}:proportion of humans in bed
#' }
#' @param vec_p list of vector-specific entomological paramteres (see
#' description of output from function \code{def_vector_params})
#'
#' @return The function returns an object with the following attributes:
#' \itemize{
#' \item \code{Exposure_Indoor_total}: indoor exposure proportion
#' \item \code{Exposure_Outdoor_total}: outdoor exposure proportion
#' \item \code{Exposure_Indoor_whileinbed}: indoor in bed exposure proportion
#' \item \code{Exposure_Outdoor_whileinbed}: outdoor in bed exposure proportion
#' \item \code{indoor_resting}: proportion of mosquitoes that rest indoors.
#' }
#' All numbers in the output list are proportions of the overall total
#' exposure in the absence of interventions.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @references TO DO
#'
#' @examples
#'
#' @export
#'
#'
# Adjusted to allow for a specified exposure multiplier value.
get_in_out_exp <- function(activity_cycles, vec_p, specified_multiplier = NULL) {

    # If specified_multiplier is not NULL, return a list with each element equal to the specified multiplier.
    print(specified_multiplier)
    if (!is.null(specified_multiplier)) {
        indoor_outdoor <- list(specified_multiplier, specified_multiplier,
                               specified_multiplier, specified_multiplier,
                               specified_multiplier)
        names(indoor_outdoor) <- c('Exposure_Indoor_total', 'Exposure_Outdoor_total',
                                   'Exposure_Indoor_whileinbed', 'Exposure_Outdoor_whileinbed',
                                   'indoor_resting')
        return(indoor_outdoor)
    }
    else {
        # Calculate exposure to biting by category in the absence of interventions
        # Endophagy is ideally the proportion of bites that are indoors from matched
        # indoor:outdoor landing collections
        activity_cycles$HBI <- activity_cycles$HBI / sum(activity_cycles$HBI, na.rm = TRUE) * vec_p$endophagy
        activity_cycles$HBO <- activity_cycles$HBO / sum(activity_cycles$HBO, na.rm = TRUE) * (1 - vec_p$endophagy)

        # Assume humans are indoors while sleeping unless a separate humans_indoors cycle is specified.
        activity_cycles$humans_indoors <- with(activity_cycles, ifelse(is.na(humans_indoors),
                                                                       humans_in_bed, humans_indoors))
        activity_cycles$Exposure_Indoor_total <- with(activity_cycles, HBI * humans_indoors)
        activity_cycles$Exposure_Outdoor_total <- with(activity_cycles, HBO * (1 - humans_indoors))

        # If humans_indoors < humans_in_bed we assign the difference to outdoor sleeping.
        activity_cycles$Exposure_Indoor_whileinbed <- with(activity_cycles, HBI * pmin(humans_in_bed,
                                                                                       humans_indoors, na.rm = TRUE))
        activity_cycles$Exposure_Outdoor_whileinbed <- with(activity_cycles, HBO * pmax(humans_in_bed - humans_indoors,
                                                                                        rep(0, length(humans_indoors)),
                                                                                        na.rm = TRUE))
        Overall_total = sum(activity_cycles$Exposure_Indoor_total, na.rm = TRUE) +
            sum(activity_cycles$Exposure_Outdoor_total, na.rm = TRUE)

        # Calculate exposures.
        # Total indoor exposure as a proportion of overall exposure
        Exposure_Indoor_total <- sum(activity_cycles$Exposure_Indoor_total, na.rm = TRUE) / Overall_total
        # Total outdoor exposure as a proportion of overall exposure
        Exposure_Outdoor_total <- sum(activity_cycles$Exposure_Outdoor_total, na.rm = TRUE) / Overall_total
        # Total indoor in bed exposure as a proportion of overall exposure
        Exposure_Indoor_whileinbed <- sum(activity_cycles$Exposure_Indoor_whileinbed, na.rm = TRUE) / Overall_total
        # Total outdoor in bed exposure as a proportion of overall exposure
        Exposure_Outdoor_whileinbed <- sum(activity_cycles$Exposure_Outdoor_whileinbed, na.rm = TRUE) / Overall_total

        # Indoor-outdoor patterns relate to both mosquito and human behaviour.
        indoor_outdoor <- list(Exposure_Indoor_total, Exposure_Outdoor_total,
                               Exposure_Indoor_whileinbed,
                               Exposure_Outdoor_whileinbed, vec_p$endophily)
        names(indoor_outdoor) <- c('Exposure_Indoor_total', 'Exposure_Outdoor_total',
                                   'Exposure_Indoor_whileinbed',
                                   'Exposure_Outdoor_whileinbed', 'indoor_resting')
    }
    return(indoor_outdoor)
}

#########get_exposure_multiplier#########
#' @title calculate adjustment for averted exposure
#'
#' @description \code{calculate_impact_ip} This function calculates
#' the proportion of averted exposure to adjust for
#' depending on the human location importance for intervention impact
#' @param param_name parameter name to adjust exposure for. Can only be one of:
#' "\code{alphai}", "\code{PBi}", "\code{PCi}", "\code{PDi}"
#' @param model_obj list of vector- and host-specific entomological parameters
#' (see description of output from function \code{calc_model_params})
#' @param intervention_type name of the type of intervention, can be only one
#' of: "LLINs", "IRS", "LLINsO", "Screening", "Larviciding".
#'
#' @return exposure multiplier value to be used for adjusting averted exposure
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @references TO DO
#'
#' @examples
#'
#' @export
#'

# Adjusted to allow for a specified exposure multiplier value.
get_exposure_multiplier = function(param_name, model_obj, intervention_type, specified_multiplier = NULL) {
    # If specified_multiplier is not NULL, return it directly.
    if (!is.null(specified_multiplier)) {
        return(specified_multiplier)
    }
    # Check the inputs
    if (!(param_name %in% c("alphai", "PBi", "PCi", "PDi"))) {
        stop("The argument param_name should be one of: alphai, PBi, PCi, PDi.")
    }
    interventions = c("LLINs", "IRS", "LLINsO", "Screening", "Larviciding")
    if (!(intervention_type %in% interventions)) {
        stop(paste0("The argument intervention_type should be one of: ", paste(interventions, collapse = ", ")))
    }
    # Calculate indoor and outdoor exposure to biting
    biting_exp = get_in_out_exp(model_obj$activity, model_obj$vec_params)
    # Calculate the exposure multiplier value
    multiplier = adjustment_for_location(param_name, biting_exp, intervention_type)
    return(multiplier)
}

