# This file contains functions for querying the package database.

#########list_all_species############
#' @title List the taxa with entomological parameters in the package database
#'
#' @description Lists the mosquito taxa (species, complexes and the genus)
#' whose entomological parameters are available in the package.
#'
#' @param level Optional; \code{"species"}, \code{"complex"} or \code{"genus"}
#' to list only taxa of that level. Default \code{NULL} lists all taxa.
#'
#' @return Data frame with columns \code{species_name}, \code{level},
#' \code{complex} (parent complex of a species, \code{NA} otherwise) and
#' \code{imputed_traits} (parameters without data for this taxon, whose values
#' come from its complex or the genus).
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @examples
#' species = list_all_species(level = "species")
#' head(species)
#'
#' @export
#'
list_all_species = function(level = NULL) {
    taxa = vec_ent_param[, c("species_name", "level", "complex", "imputed_traits")]
    if (!is.null(level)) {
        level = match.arg(level, .TAXON_LEVELS)
        taxa  = taxa[taxa$level == level, ]
    }
    rownames(taxa) = NULL
    taxa
}

#########list_activity############
#' @title Extract activity patterns of humans or mosquitoes
#'
#' @description Returns activity patterns from the package database matching
#' the provided criteria.
#'
#' @param species Name of the species. Use "Homo sapiens" for human patterns.
#' Default is "Anopheles gambiae".
#' @param sampling Sampling type: "IND", "BED", "HBI", "HBO", "ABO", or "HB".
#' Default is "HBI".
#' @param country Country name. Default NULL returns all countries.
#' @param site Site name. Default NULL returns all sites.
#'
#' @return Data frame with columns: id, species, sampling, country, site,
#' variable (time point), value.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @examples
#' humans_indoors = list_activity(species = "Homo sapiens", sampling = "IND",
#' site = "Rachuonyo")
#' print(humans_indoors)
#'
#' @export
#'
list_activity = function(species = "Anopheles gambiae", sampling = "HBI",
                         country = NULL, site = NULL) {
    if (is.null(species) | is.null(sampling)) {
        stop("Species name and sampling type should be provided.")
    }
    if (is.null(country)) country = activity_patterns$country
    if (is.null(site))    site    = activity_patterns$site

    selected_patterns = activity_patterns[
        which(activity_patterns$species == species &
                  activity_patterns$sampling == sampling &
                  activity_patterns$country %in% country &
                  activity_patterns$site %in% site), ]

    if (nrow(selected_patterns) == 0 || is.null(nrow(selected_patterns))) {
        print("No activity patterns found for the provided search criteria.")
    }
    return(selected_patterns)
}

#########calculate_exposure#########
#' @title Calculate indoor and outdoor human exposure to biting
#'
#' @description Calculates indoor and outdoor proportions of human exposure to
#' mosquito biting, based on activity patterns and mosquito endophagy /
#' endophily parameters.
#'
#' The relevant proportion is used as the \code{exposure} argument of
#' \code{calculate_impact()} / \code{run_interventions()}, which reduces the
#' effective coverage (\code{coverage * exposure}); it does not modify the
#' intervention efficacies (unlike AnophelesModel version 1). Typically
#' \code{Exposure_Indoor_whileinbed} for ITNs; for IRS, see below.
#'
#' For IRS, two choices are defensible: the mosquitoes resting indoors
#' (\code{indoor_resting}, the endophily) or the bites received indoors
#' (\code{Exposure_Indoor_total}). They describe different pathways and agree
#' only when both shares are equal; neither is wrong in itself. The
#' OpenMalaria export offers the same choice
#' (\code{get_OM_ento_snippet(irs_exposure = )}, default
#' \code{"indoor_resting"}): use the same one in both routes.
#'
#' @param activity_cycles List with time-series attributes: \code{HBI},
#' \code{HBO}, \code{humans_indoors}, \code{humans_in_bed}.
#' @param vec_p List of vector-specific entomological parameters (see
#' \code{def_vector_params}).
#'
#' @return List with proportions of overall exposure:
#' \itemize{
#'   \item \code{Exposure_Indoor_total}
#'   \item \code{Exposure_Outdoor_total}
#'   \item \code{Exposure_Indoor_whileinbed}
#'   \item \code{Exposure_Outdoor_whileinbed}
#'   \item \code{indoor_resting}
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briet
#' @author Nakul Chitnis
#' @author Tom Smith
#'
#' @export
#'
calculate_exposure = function(activity_cycles, vec_p) {
    check_activity_pattern(activity_cycles, "activity_cycles")
    activity_cycles$HBI = activity_cycles$HBI /
        sum(activity_cycles$HBI, na.rm = TRUE) * vec_p$endophagy
    activity_cycles$HBO = activity_cycles$HBO /
        sum(activity_cycles$HBO, na.rm = TRUE) * (1 - vec_p$endophagy)

    activity_cycles$humans_indoors = with(activity_cycles,
                                          ifelse(is.na(humans_indoors),
                                                 humans_in_bed, humans_indoors))
    activity_cycles$Exposure_Indoor_total  = with(activity_cycles, HBI * humans_indoors)
    activity_cycles$Exposure_Outdoor_total = with(activity_cycles, HBO * (1 - humans_indoors))

    activity_cycles$Exposure_Indoor_whileinbed = with(activity_cycles,
        HBI * pmin(humans_in_bed, humans_indoors, na.rm = TRUE))
    activity_cycles$Exposure_Outdoor_whileinbed = with(activity_cycles,
        HBO * pmax(humans_in_bed - humans_indoors,
                   rep(0, length(humans_indoors)), na.rm = TRUE))

    Overall_total = sum(activity_cycles$Exposure_Indoor_total,  na.rm = TRUE) +
                    sum(activity_cycles$Exposure_Outdoor_total, na.rm = TRUE)

    indoor_outdoor = list(
        Exposure_Indoor_total       = sum(activity_cycles$Exposure_Indoor_total,       na.rm = TRUE) / Overall_total,
        Exposure_Outdoor_total      = sum(activity_cycles$Exposure_Outdoor_total,      na.rm = TRUE) / Overall_total,
        Exposure_Indoor_whileinbed  = sum(activity_cycles$Exposure_Indoor_whileinbed,  na.rm = TRUE) / Overall_total,
        Exposure_Outdoor_whileinbed = sum(activity_cycles$Exposure_Outdoor_whileinbed, na.rm = TRUE) / Overall_total,
        indoor_resting              = vec_p$endophily
    )
    return(indoor_outdoor)
}
