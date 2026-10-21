# Export to OpenMalaria via r-openMalariaUtilities (OMU) or as XML text.
# Schema 44 by default; dummy-host interventions need schema >= 46 (developed
# for 49), whose only change to what is written here is the removal of
# propInfected / propInfectious from <anopheles> (49).

# Internal: number -> string, as in the OMU examples
om_num = function(x) as.character(signif(x, 6))

# Internal: validated OpenMalaria schema version
om_check_schema = function(schema) {
    if (!is.numeric(schema) || length(schema) != 1L || is.na(schema) ||
        schema != round(schema) || schema < 44)
        stop("schema must be an OpenMalaria schema version (a whole number >= 44)")
    as.integer(schema)
}
# Internal: schema version of a baseList (openMalariaUtilities keeps it in
# OMVersion; 44 if absent)
om_baselist_schema = function(baseList)
    if (is.null(baseList$OMVersion)) 44L else om_check_schema(baseList$OMVersion)
# Smallest schema with increasing decays (dummy host)
om_dummy_min_schema = 46L

# Internal: the OpenMalaria export of dummy-host interventions is work in
# progress (not yet validated in OpenMalaria): a warning, unless acknowledged
# with options(AnophelesModel2.om_dummy_host = TRUE). The GVI export of
# standard interventions is validated.
om_check_dummy_enabled = function() {
    if (!isTRUE(getOption("AnophelesModel2.om_dummy_host")))
        warning("\n",
            "*************************************************************************\n",
            "* WORK IN PROGRESS: the OpenMalaria export of dummy-host interventions  *\n",
            "* (host-seeking killing as added non-human hosts, schema >= 46) has not *\n",
            "* been validated in OpenMalaria yet. Check the results carefully, or    *\n",
            "* use standard interventions (get_ITN_params(mode = \"standard\")), whose *\n",
            "* export is validated.                                                  *\n",
            "* (Silence: options(AnophelesModel2.om_dummy_host = TRUE))              *\n",
            "*************************************************************************",
            call. = FALSE)
    invisible()
}

# Internal: decay shape names of this package -> OpenMalaria decay functions
om_decay_name = function(decay) {
    shape = tolower(as.character(decay))
    switch(shape, "smoothcompact" = "smooth-compact",
           "none" = , "no decay" = , "constant" = "constant", shape)
}


#########get_OM_ento_snippet####
#' @title Mosquito parameters for an OpenMalaria scenario
#'
#' @description Builds the mosquito (entomology) parameters of an OpenMalaria
#' scenario (schema 44) from the bionomics in \code{vec_ent_param}, as the
#' input lists of \code{openMalariaUtilities::defineEntomology()}.
#'
#' With \code{activity}, each taxon is split into three mosquito populations
#' according to human exposure (\code{\link{calculate_exposure}}): with
#' \code{in_bed} the exposure indoors while in bed and \code{resting} the
#' proportion resting indoors (endophily),
#' \itemize{
#'   \item \code{<taxon>_indoor1}: share \code{min(in_bed, resting)}, exposed
#'     both in bed and while resting indoors;
#'   \item \code{<taxon>_indoor2}: share \code{max(in_bed, resting) -
#'     min(in_bed, resting)}, exposed in bed only (if \code{in_bed > resting})
#'     or while resting indoors only;
#'   \item \code{<taxon>_outdoor}: the rest, not exposed indoors.
#' }
#' The populations share all parameters except the human blood index: indoor
#' populations use the indoor HBI, the outdoor population the outdoor HBI.
#' Without \code{activity}, each taxon is a single population whose human blood
#' index is chosen with \code{hbi}.
#'
#' Mapping to OpenMalaria: \code{mosqRestDuration} = resting duration
#' \code{tau} in whole days (at least 1), \code{mosqSeekingDuration} =
#' \code{td}, \code{extrinsicIncubationPeriod} = \code{ts},
#' \code{mosqLaidEggsSameDayProportion} = \code{A0},
#' \code{mosqSurvivalFeedingCycleProbability} = \code{M} (parous rate), human
#' host probabilities from \code{hosts_p}. Each population also carries a
#' \code{nonHumanHosts} entry with the animal host probabilities of
#' \code{hosts_p}, written by \code{\link{define_OM_entomology}} and
#' \code{\link{get_OM_xml}} (\code{defineEntomology()} alone writes fixed
#' animal values and ignores this entry).
#'
#' @param taxa Character vector of taxon names (see
#'   \code{\link{list_all_species}}); the usual synonyms of
#'   \emph{An. gambiae ss/coluzzii} are accepted with a warning.
#' @param level Taxonomic level(s) of \code{taxa}: \code{"species"} (default),
#'   \code{"complex"} or \code{"genus"}; recycled.
#' @param activity Optional activity patterns (from
#'   \code{\link{def_activity_patterns}}), one per taxon (a single pattern is
#'   used for all taxa). Triggers the indoor/outdoor split.
#' @param hbi Human blood index of unsplit populations: \code{"weighted"}
#'   (default, indoor and outdoor HBI weighted by endophagy), \code{"indoor"}
#'   or \code{"outdoor"}.
#' @param hosts_p Host parameters from \code{\link{def_host_params}}.
#' @param propInfected,propInfectious Initial proportions of infected and
#'   infectious mosquitoes, written to \code{seasonalityParameters}.
#' @param bionomics_table Optional bionomics table used instead of
#'   \code{vec_ent_param} (see \code{\link{read_bionomics_table}}).
#' @param species_shares Optional named vector: share of each taxon in the
#'   EIR, one per taxon (names as in \code{taxa}, canonical names or
#'   synonyms), summing to 1; see \code{\link{eir_shares}} to compute it from
#'   relative abundances and sporozoite rates.
#'
#'   The \code{annualEIR} of each population is its relative contribution to
#'   the EIR: share of its species \eqn{\times} share within the species,
#'   summing to 1 over all populations. With \code{species_shares}, these are
#'   numbers; without (default), placeholders
#'   \code{"@contrib_<population>@"}, whose values for any species
#'   composition are given by \code{\link{om_ento_values}}. In both cases,
#'   the total EIR is set with the \code{scaledAnnualEIR} attribute of
#'   \code{<entomology>} (a number or a placeholder, e.g.
#'   \code{defineEntomology(..., scaledAnnualEIR = "@EIR@")}).
#'
#' @return List with
#' \itemize{
#'   \item \code{mosquitoParameters}: named list (one element per mosquito
#'     population) for \code{defineEntomology(mosquitoParameters = )};
#'   \item \code{seasonalityParameters}: named list skeleton for
#'     \code{defineEntomology(seasonalityParameters = )}, with placeholders
#'     \code{"@contrib_<population>@"} for \code{annualEIR} (numbers with
#'     \code{species_shares}) and \code{"@m1@"},
#'     ..., \code{"@m12@"} for the seasonality, to be replaced (with R >= 4.3,
#'     \code{defineEntomology()} currently rejects monthly placeholders;
#'     replace them with numeric values, or a fixed openMalariaUtilities);
#'   \item \code{shares}: data frame with \code{mosquito}, \code{taxon},
#'     \code{type} (\code{indoor1}, \code{indoor2}, \code{outdoor} or
#'     \code{all}), \code{share} within the taxon (with
#'     \code{species_shares}, also \code{species_share} and the overall
#'     \code{eir_share}), and the logical columns
#'     \code{in_bed} and \code{indoor_resting} (exposure to interventions
#'     acting in bed, e.g. ITNs, or on indoor resting mosquitoes, e.g. IRS),
#'     and \code{muvA}, the host-seeking death rate without interventions
#'     (per day, with the population's HBI);
#'   \item \code{model_params}: named list (one element per mosquito
#'     population) of \code{\link{build_model_obj}} outputs with the
#'     population's HBI (\code{NULL} if the bionomics give no valid death
#'     rate); their human availability scales the dummy hosts of
#'     host-seeking killing (\code{\link{define_OM_vector_control}}).
#' }
#'
#' @seealso \code{\link{get_OM_GVI_snippet}}, \code{\link{define_OM_vector_control}}
#'
#' @examples
#' om = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
#'                          activity = def_activity_patterns())
#' names(om$mosquitoParameters)
#' om$shares
#' \dontrun{
#' baseList = openMalariaUtilities::defineEntomology(
#'     baseList, om$seasonalityParameters, om$mosquitoParameters)
#' }
#'
#' @export
#'
get_OM_ento_snippet = function(taxa, level = "species", activity = NULL,
                               hbi = "weighted", hosts_p = def_host_params(),
                               propInfected = 0.078, propInfectious = 0.021,
                               bionomics_table = NULL, species_shares = NULL) {
    hbi   = match.arg(hbi, c("weighted", "indoor", "outdoor"))
    level = rep_len(level, length(taxa))
    split = !is.null(activity)
    if (split) {
        if (!is.null(activity$HBI)) {
            # A single pattern for all taxa (c() of several patterns ends here
            # too, with duplicated entries: an error)
            check_activity_pattern(activity)
            activity = rep(list(activity), length(taxa))
        }
        if (!is.list(activity) || length(activity) != length(taxa))
            stop("activity must be one activity pattern, or a list with one ",
                 "pattern per taxon (", length(taxa), "), made with list()")
        for (i in seq_along(activity))
            check_activity_pattern(activity[[i]], paste0("activity[[", i, "]]"))
    }

    mosquitoParameters = list()
    model_params = list()
    shares = list()
    for (i in seq_along(taxa)) {
        vec_p = def_vector_params(taxa[i], level = level[i],
                                  hbi = if (split) "indoor" else hbi,
                                  bionomics_table = bionomics_table,
                                  verbose = FALSE)
        taxon = vec_p$species_name

        if (split) {
            if (is.null(vec_p$HBI_indoor) || is.null(vec_p$HBI_outdoor))
                stop("The indoor/outdoor split requires HBI_indoor and ",
                     "HBI_outdoor for ", taxon)
            exposure = calculate_exposure(activity[[i]], vec_p)
            in_bed   = exposure$Exposure_Indoor_whileinbed
            resting  = exposure$indoor_resting
            indoor1  = min(in_bed, resting)
            indoor2  = max(in_bed, resting) - indoor1
            sh = data.frame(
                mosquito = paste0(taxon, c("_indoor1", "_indoor2", "_outdoor")),
                taxon    = taxon,
                type     = c("indoor1", "indoor2", "outdoor"),
                share    = c(indoor1, indoor2, 1 - indoor1 - indoor2),
                in_bed         = c(TRUE, in_bed > resting, FALSE),
                indoor_resting = c(TRUE, in_bed <= resting, FALSE),
                stringsAsFactors = FALSE)
            chi = c(vec_p$HBI_indoor, vec_p$HBI_indoor, vec_p$HBI_outdoor)
        } else {
            sh = data.frame(mosquito = taxon, taxon = taxon, type = "all",
                            share = 1, in_bed = TRUE, indoor_resting = TRUE,
                            stringsAsFactors = FALSE)
            chi = vec_p$Chi
        }
        sh$muvA = NA_real_
        for (j in seq_len(nrow(sh))) {
            mosquitoParameters[[sh$mosquito[j]]] = om_mosq_params(vec_p, chi[j], hosts_p)
            # Model of this population (its HBI), for the host-seeking killing
            # export (human availability); NULL if the bionomics give no valid
            # death rate (only an error when needed)
            vp = vec_p
            vp$Chi = chi[j]
            mp = tryCatch(build_model_obj(vp, hosts_p), error = function(e) NULL)
            model_params[sh$mosquito[j]] = list(mp)
            if (!is.null(mp)) sh$muvA[j] = mp$host_params$muvA
        }
        shares[[i]] = sh
    }
    shares = do.call(rbind, shares)
    rownames(shares) = NULL
    if (anyDuplicated(shares$mosquito)) stop("Duplicated taxa in 'taxa'")

    # annualEIR = relative contribution of each population to the EIR
    # (species share x share within the species): numbers with
    # species_shares, otherwise one placeholder per population. The total EIR
    # is always set with scaledAnnualEIR.
    if (!is.null(species_shares)) {
        sp = om_species_shares(species_shares, unique(shares$taxon), taxa)
        shares$species_share = unname(sp[shares$taxon])
        shares$eir_share     = shares$species_share * shares$share
        eir = om_num(shares$eir_share)
        what = "the relative contribution of each population"
    } else {
        eir = paste0("@", om_contrib_placeholder(shares$mosquito), "@")
        what = paste("a placeholder @contrib_<population>@ for the relative",
                     "contribution of each population (values: om_ento_values())")
    }
    message("get_OM_ento_snippet: annualEIR = ", what, "; set the total EIR ",
            "with scaledAnnualEIR (e.g. defineEntomology(..., scaledAnnualEIR = ",
            "\"@EIR@\")).")
    seasonalityParameters = lapply(seq_along(shares$mosquito), function(j) list(
        annualEIR      = eir[j],
        seasonality    = paste0("@m", 1:12, "@"),
        propInfected   = propInfected,
        propInfectious = propInfectious))
    names(seasonalityParameters) = shares$mosquito

    list(mosquitoParameters    = mosquitoParameters,
         seasonalityParameters = seasonalityParameters,
         shares                = shares,
         model_params          = model_params)
}


# Internal: name (without @) of the annualEIR placeholder of a population (its
# relative contribution to the EIR)
om_contrib_placeholder = function(mosquito)
    paste0("contrib_", gsub("[^A-Za-z0-9]+", "_", mosquito))

# Internal: validated species shares of the EIR, named by canonical taxon
# (in the order of `canonical`). Names may be canonical, as given in `given`
# (the taxa argument) or an alias; one share per taxon, summing to 1.
om_species_shares = function(x, canonical, given = NULL, label = "species_shares") {
    if (!is.numeric(x) || is.null(names(x)) || any(!is.finite(x)) || any(x < 0))
        stop(label, " must be a named vector of non-negative shares, one per taxon")
    key = vapply(names(x), function(nm) {
        if (nm %in% canonical) return(nm)
        if (!is.null(given) && nm %in% given) return(canonical[match(nm, given)])
        a = .TAXON_ALIASES$canonical[.TAXON_ALIASES$alias == nm]
        if (length(a) && a[1] %in% canonical) return(a[1])
        stop(label, ": '", nm, "' is not one of the taxa (",
             paste(canonical, collapse = ", "), ")")
    }, character(1))
    if (anyDuplicated(key)) stop(label, ": several shares for the same taxon")
    missing = setdiff(canonical, key)
    if (length(missing)) stop(label, ": no share for ", paste(missing, collapse = ", "))
    if (abs(sum(x) - 1) > 1e-8)
        stop(label, " must sum to 1 (sum: ", signif(sum(x), 6), ")")
    stats::setNames(unname(x), key)[canonical]
}


#########eir_shares####
#' @title Share of each mosquito species in the EIR
#'
#' @description Share of each species in the entomological inoculation rate
#' (EIR), from the relative abundance of the species (e.g. proportions in
#' collections) and their sporozoite rates: \code{abundance *
#' sporozoite_rate}, normalised to sum to 1. The result can be passed to
#' \code{get_OM_ento_snippet(species_shares = )} or
#' \code{\link{om_ento_values}}.
#'
#' @param abundance Named vector of relative abundances, summing to 1.
#' @param sporozoite_rate Named vector of sporozoite rates, with the same
#'   names (in any order). There is no default: sporozoite rates are
#'   setting-specific (e.g. Kweyamba et al. 2025 for south-eastern Tanzania).
#'
#' @return Named vector of EIR shares (names and order of \code{abundance}).
#'
#' @examples
#' eir_shares(c("Anopheles funestus" = 0.95, "Anopheles gambiae ss/coluzzii" = 0.05),
#'            c("Anopheles funestus" = 0.235, "Anopheles gambiae ss/coluzzii" = 0.114))
#'
#' @export
#'
eir_shares = function(abundance, sporozoite_rate) {
    for (arg in list(list(abundance, "abundance"), list(sporozoite_rate, "sporozoite_rate")))
        if (!is.numeric(arg[[1]]) || is.null(names(arg[[1]])) || anyDuplicated(names(arg[[1]])) ||
            any(!is.finite(arg[[1]])) || any(arg[[1]] < 0))
            stop(arg[[2]], " must be a named vector of non-negative numbers with unique names")
    if (!setequal(names(abundance), names(sporozoite_rate)))
        stop("abundance and sporozoite_rate must have the same names")
    if (abs(sum(abundance) - 1) > 1e-8)
        stop("abundance must sum to 1 (sum: ", signif(sum(abundance), 6), ")")
    if (any(sporozoite_rate > 1)) stop("sporozoite_rate must be proportions in [0, 1]")
    contribution = abundance * sporozoite_rate[names(abundance)]
    if (sum(contribution) == 0) stop("No species contributes to the EIR")
    contribution / sum(contribution)
}


#########om_ento_values####
#' @title Values of the species contribution placeholders of an OpenMalaria
#'   entomology
#'
#' @description Fills the \code{annualEIR} placeholders
#' \code{@contrib_<population>@} of \code{\link{get_OM_ento_snippet}} (used
#' when \code{species_shares} is not given there): the relative contribution of
#' each mosquito population to the EIR, i.e. the share of its species
#' \eqn{\times} its share within the species (\code{om_ento$shares$share}),
#' summing to 1 over all populations. One row per species composition, to
#' merge into an OpenMalaria scenario table; the total EIR is set separately,
#' with the \code{scaledAnnualEIR} attribute of \code{<entomology>}.
#'
#' @param om_ento Output of \code{get_OM_ento_snippet()} without
#'   \code{species_shares}.
#' @param species_shares Species composition(s): a named vector (one
#'   composition) or a data frame / matrix with one row per composition and
#'   one column per taxon (canonical names or synonyms). Each composition must
#'   sum to 1 (see \code{\link{eir_shares}}). Row names, if any, are kept in a
#'   \code{composition} column.
#'
#' @return Data frame with \code{composition} (with row names), the species
#' shares (one column per taxon) and one column per placeholder, named without
#' the \code{@} (\code{contrib_<population>}).
#'
#' @examples
#' om = get_OM_ento_snippet(c("Anopheles funestus", "Anopheles gambiae ss/coluzzii"),
#'                          activity = def_activity_patterns())
#' om_ento_values(om, rbind(
#'     funestus_dominant = c("Anopheles funestus" = 0.9,
#'                           "Anopheles gambiae ss/coluzzii" = 0.1),
#'     equal             = c("Anopheles funestus" = 0.5,
#'                           "Anopheles gambiae ss/coluzzii" = 0.5)))
#'
#' @export
#'
om_ento_values = function(om_ento, species_shares) {
    sh = om_ento$shares
    if (!is.null(sh$eir_share))
        stop("om_ento was built with species_shares: its contributions are ",
             "numbers, not placeholders")
    taxa = unique(sh$taxon)
    mixes = if (is.null(dim(species_shares))) {
        if (is.null(names(species_shares)))
            stop("species_shares must be named by taxon")
        as.data.frame(as.list(species_shares), check.names = FALSE)
    } else as.data.frame(species_shares, check.names = FALSE)
    if (nrow(mixes) == 0L) stop("species_shares has no composition")
    comp = lapply(seq_len(nrow(mixes)), function(i) om_species_shares(
        unlist(mixes[i, , drop = FALSE]), taxa,
        label = paste0("species_shares (composition ", i, ")")))
    # Composition labels: row names of a matrix, or non-automatic row names of
    # a data frame
    labels = rownames(mixes)
    has_labels = if (is.data.frame(species_shares)) .row_names_info(species_shares) > 0
                 else !is.null(dim(species_shares)) && !is.null(rownames(species_shares))

    out = data.frame(row.names = seq_along(comp))
    if (has_labels) out$composition = labels
    for (tx in taxa) out[[tx]] = vapply(comp, `[[`, numeric(1), tx)
    for (j in seq_len(nrow(sh)))
        out[[om_contrib_placeholder(sh$mosquito[j])]] = out[[sh$taxon[j]]] * sh$share[j]
    rownames(out) = NULL
    out
}


# Internal: parameters of one mosquito population in the OMU format
# (value / mean conventions of openMalariaUtilities::defineEntomology()).
om_mosq_params = function(vec_p, chi, hosts_p) {
    list(
        mosqRestDuration = list(value = om_num(max(1, floor(vec_p$tau + 0.5)))),
        extrinsicIncubationPeriod = list(value = om_num(round(vec_p$ts))),
        mosqLaidEggsSameDayProportion = list(value = om_num(vec_p$A0)),
        mosqSeekingDuration = list(value = om_num(vec_p$td)),
        mosqSurvivalFeedingCycleProbability = list(value = om_num(vec_p$M)),
        mosqProbBiting       = list(mean = om_num(hosts_p$PBi[1]), variance = "0"),
        mosqProbFindRestSite = list(mean = om_num(hosts_p$PCi[1]), variance = "0"),
        mosqProbResting      = list(mean = om_num(hosts_p$PDi[1]), variance = "0"),
        mosqProbOvipositing  = list(mean = om_num(hosts_p$PEi[1])),
        mosqHumanBloodIndex  = list(mean = om_num(chi)),
        nonHumanHosts = list(
            name = "unprotectedAnimals",
            mosqRelativeEntoAvailability = list(value = "1"),
            mosqProbBiting       = list(value = om_num(hosts_p$PBi[2])),
            mosqProbFindRestSite = list(value = om_num(hosts_p$PCi[2])),
            mosqProbResting      = list(value = om_num(hosts_p$PDi[2]))))
}


#########get_OM_GVI_snippet####
#' @title Vector control intervention for an OpenMalaria scenario
#'
#' @description Builds the generic vector intervention (GVI) parameters of an
#' OpenMalaria scenario (schema 44) from an intervention object, as the input
#' list of \code{openMalariaUtilities::defineVectorControl()}. Each effect
#' (deterrency, pre-prandial and post-prandial killing) becomes an
#' OpenMalaria component with its own decay.
#'
#' Values are the initial efficacies. The decay of each effect is the product
#' of attrition and its insecticide decay, exported exactly when it is a single
#' OpenMalaria decay shape and approximated otherwise:
#' \itemize{
#'   \item no insecticide decay for the effect (or \code{insecticide_decay =
#'     FALSE}): the attrition decay;
#'   \item no attrition (\code{decay_attrition = "none"}, e.g. IRS): the
#'     effect's insecticide decay (\code{component_decay}, or linear from
#'     \code{halflife_insecticide});
#'   \item both (e.g. nets with insecticide decay): the Weibull fitted to their
#'     product (\code{fitted_decay} of \code{\link{build_intervention}}; its
#'     amplitude is ignored, as in ITNcascades).
#' }
#' A negative \code{halflife_insecticide} (efficacy increasing with washing)
#' has no OpenMalaria shape: it is only exported through the fitted Weibull
#' (with attrition), and is an error without attrition.
#' Decay parameters \code{L} are converted from days to years. As OpenMalaria
#' has no separate attrition for GVIs, net attrition is folded into the decay
#' of the effects.
#'
#' The effects act on the mosquito populations exposed to the intervention
#' (see \code{\link{get_OM_ento_snippet}}): in bed for ITNs, resting indoors for
#' IRS; other populations get \code{propActive = 0}.
#'
#' Dummy-host interventions (\code{HostSeekingKilling}) have no pre-prandial
#' killing: their GVI has deterrency and post-prandial killing only, and the
#' host-seeking killing, which has no GVI equivalent, is exported by
#' \code{\link{define_OM_vector_control}}.
#'
#' @param interv Intervention from \code{\link{om_vector_control}} (or an
#'   object from \code{\link{build_intervention}}).
#' @param om_ento Output of \code{\link{get_OM_ento_snippet}}.
#' @param intervention_type \code{"ITN"} (acts in bed) or \code{"IRS"} (acts on
#'   indoor resting mosquitoes).
#' @param name Name of the intervention (default: the intervention
#'   description).
#'
#' @return Named list (one element, \code{name}) with \code{deterrency},
#' \code{preprandialKillingEffect} (standard model only) and
#' \code{postprandialKillingEffect}, each
#' with \code{decay} and per-mosquito \code{anophelesParams}
#' (\code{propActive}, \code{value}), for
#' \code{defineVectorControl(vectorInterventionParameters = )}.
#'
#' @seealso \code{\link{get_OM_ento_snippet}}, \code{\link{build_intervention}}
#'
#' @examples
#' itn = om_vector_control(
#'     c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
#'     halflife_attrition = 2 * 365, shape_attrition = 2, name = "ITN")
#' om  = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
#'                           activity = def_activity_patterns())
#' gvi = get_OM_GVI_snippet(itn, om, intervention_type = "ITN")
#' str(gvi$ITN$deterrency$decay)
#' \dontrun{
#' baseList = openMalariaUtilities::defineVectorControl(baseList, gvi)
#' }
#'
#' @export
#'
get_OM_GVI_snippet = function(interv, om_ento, intervention_type = c("ITN", "IRS"),
                              name = NULL) {
    intervention_type = match.arg(intervention_type)
    interv = om_unwrap(interv)
    if (is.null(interv))
        stop("interv must be an intervention from om_vector_control() or ",
             "build_intervention()")
    if (is.null(name)) name = om_default_name(interv, "GVI")
    interv = om_gvi_objects(list(interv), om_ento, intervention_type)[[1]]
    if (interv$settings$mode == "dummy_host") om_check_dummy_enabled()
    if (interv$settings$mode == "dummy_host")
        message("get_OM_GVI_snippet: dummy-host intervention, the GVI has ",
                "deterrency and post-prandial killing only; host-seeking ",
                "killing is exported by deploy_OM_vector_control().")
    om_gvi_list(interv, om_ento, intervention_type, name)
}


# Internal: effects of the GVI for a model (no pre-prandial killing with the
# dummy host, whose host-seeking killing is exported separately)
om_gvi_effects = c(Deterrency   = "deterrency",
                   PrePrandial  = "preprandialKillingEffect",
                   PostPrandial = "postprandialKillingEffect")
om_model_effects = function(mode) {
    if (mode == "dummy_host") c("Deterrency", "PostPrandial")
    else c("Deterrency", "PrePrandial", "PostPrandial")
}
# Placeholder labels of the effects
om_ph_label = c(Deterrency = "deterrency", PrePrandial = "preprandial",
                PostPrandial = "postprandial")
# Placeholder-safe version of a name
om_ph_name = function(name) gsub("[^A-Za-z0-9]+", "_", name)

# Internal: mosquito populations reached by the intervention
om_active = function(om_ento, intervention_type) {
    if (intervention_type == "ITN") om_ento$shares$in_bed else om_ento$shares$indoor_resting
}

# Internal: an intervention to export: a specification from
# om_vector_control(), or an intervention object (element of
# build_intervention()'s list); NULL otherwise
om_unwrap = function(x) {
    if (om_is_spec(x)) return(x)
    if (!is.list(x)) return(NULL)
    if (!is.null(x$settings)) return(x)
    if (length(x) == 1L && is.null(names(x)) && is.list(x[[1]]) &&
        !is.null(x[[1]]$settings)) return(x[[1]])
    NULL
}

om_default_name = function(interv, default) {
    if (om_is_spec(interv)) return(if (!is.null(interv$name)) interv$name else default)
    if (!is.null(interv$description) && interv$description != "custom")
        interv$description else default
}

om_is_spec = function(x) inherits(x, "om_vector_control")

# Internal: model structure of an intervention or specification
om_item_mode = function(x) if (om_is_spec(x)) x$mode else x$settings$mode

# Internal: intervention object of a specification, over `years` (one point
# per day). The model only sets baseline host parameters, which the
# OpenMalaria export does not use (relative efficacies, absolute host-seeking
# killing), so any model of the entomology gives the same export.
om_build_spec = function(spec, model_p, years) {
    suppressMessages(build_intervention(
        efficacy = spec$efficacy, halflife_attrition = spec$halflife_attrition,
        shape_attrition = spec$shape_attrition, decay_attrition = spec$decay_attrition,
        duration_years = years, model_p = model_p, name = spec$name, mode = spec$mode,
        halflife_insecticide = spec$halflife_insecticide,
        component_decay = spec$component_decay,
        insecticide_decay = spec$insecticide_decay))[[1]]
}

# Internal: window (years) over which the GVI decays are fitted when
# attrition and insecticide decay are not a single OpenMalaria decay
om_fit_years = function(spec, intervention_type) {
    if (!is.null(spec$fit_years)) spec$fit_years
    else if (intervention_type == "IRS") 1 else 3
}

# Internal: a model of the entomology (the first valid population), to build
# the interventions
om_model_for = function(om_ento) {
    mp = Filter(Negate(is.null), om_ento$model_params)
    if (!length(mp)) stop("om_ento has no valid mosquito population model")
    mp[[1]]
}

# Internal: intervention objects for the GVI (specifications built over their
# fitting window); intervention objects are used as they are
om_gvi_objects = function(items, om_ento, intervention_type) {
    lapply(items, function(it) if (om_is_spec(it))
        om_build_spec(it, om_model_for(om_ento), om_fit_years(it, intervention_type))
        else it)
}

# Internal: GVI list of get_OM_GVI_snippet() for one intervention
om_gvi_list = function(interv, om_ento, intervention_type, name) {
    s = interv$settings
    attr_none = om_decay_name(s$decay_attrition) == "constant"
    if (intervention_type == "IRS" && !attr_none)
        warning("IRS with attrition decay '", s$decay_attrition,
                "': IRS usually has no attrition (decay_attrition = \"none\").")
    if (intervention_type == "ITN" && attr_none)
        warning("ITN without attrition decay (decay_attrition = \"none\").")

    active = om_active(om_ento, intervention_type)
    comps  = om_model_effects(s$mode)
    out = lapply(comps, function(comp) {
        value = if (comp %in% names(s$efficacy)) unname(s$efficacy[[comp]]) else 0
        params = lapply(seq_along(active), function(j)
            list(propActive = if (active[j]) 1 else 0,
                 value      = om_num(if (active[j]) value else 0)))
        names(params) = om_ento$shares$mosquito
        list(decay = om_gvi_decay(interv, comp), anophelesParams = params)
    })
    names(out) = unname(om_gvi_effects[comps])
    stats::setNames(list(out), name)
}


#########add_OM_GVI####
#' @title Add vector control interventions to an OpenMalaria scenario
#'
#' @description Writes the generic vector interventions from
#' \code{\link{get_OM_GVI_snippet}} into an openMalariaUtilities
#' \code{baseList}. When the effects of an intervention share the same
#' decay (e.g. no insecticide decay, or the same insecticide decay for all
#' effects), they are written as \strong{one} component named after the
#' intervention, as in ITNcascades; otherwise as one component per effect
#' (\code{<name>-deterrency}, \code{<name>-preprandialKillingEffect},
#' \code{<name>-postprandialKillingEffect}), as
#' \code{openMalariaUtilities::defineVectorControl()} does. The component ids
#' are printed: when there are several per intervention, they must be deployed
#' together, to the same people (\code{\link{deploy_OM_vector_control}}).
#' Dummy-host interventions have no pre-prandial component (its value is
#' written as 0).
#'
#' Requires the openMalariaUtilities package (uses its internal functions
#' \code{.defineInterventionsHeader()} and \code{.xmlAddList()}).
#'
#' @param baseList openMalariaUtilities \code{baseList} (the human
#'   interventions section is created if needed).
#' @param gvi Output of \code{\link{get_OM_GVI_snippet}} (several can be
#'   combined with \code{c()}).
#' @param merge Logical (default \code{TRUE}): write one component when the
#'   three decays are identical.
#' @param append Logical (default \code{TRUE}): append to the existing
#'   interventions.
#'
#' @return The updated \code{baseList}.
#'
#' @seealso \code{\link{get_OM_GVI_snippet}}
#'
#' @examples
#' \dontrun{
#' gvi = c(get_OM_GVI_snippet(itn, om_ento, "ITN"),
#'         get_OM_GVI_snippet(irs, om_ento, "IRS"))
#' baseList = add_OM_GVI(baseList, gvi)
#' }
#'
#' @export
#'
add_OM_GVI = function(baseList, gvi, merge = TRUE, append = TRUE) {
    om_add_components(baseList, om_gvi_components(gvi, merge), append)
}


# Internal: writes <component> lists into baseList$interventions$human with
# the openMalariaUtilities internals (as its own define functions do)
om_add_components = function(baseList, components, append = TRUE) {
    if (!requireNamespace("openMalariaUtilities", quietly = TRUE))
        stop("Writing into a baseList requires the openMalariaUtilities package ",
             "(get_OM_xml() writes XML text without it)")
    xml_add = utils::getFromNamespace(".xmlAddList", "openMalariaUtilities")
    header  = utils::getFromNamespace(".defineInterventionsHeader",
                                      "openMalariaUtilities")
    baseList = header(baseList = baseList)
    # A new baseList has no human interventions yet: start the section
    if (is.null(baseList$interventions$human)) baseList$interventions$human = list()
    for (cm in components) {
        baseList = xml_add(data = baseList, sublist = c("interventions", "human"),
                           append = append, entry = "component", input = cm)
        append = TRUE
    }
    ids = vapply(components, `[[`, character(1), "id")
    message("Components written (deploy those of the same intervention ",
            "together): ", paste(ids, collapse = ", "))
    baseList
}


# Internal: <component> lists (XML structure of openMalariaUtilities) of GVIs
# from get_OM_GVI_snippet(): one component per intervention when the decays of
# its effects are identical (and merge = TRUE), otherwise one per effect
# (<name>-<effect>). Shared by add_OM_GVI(), define_OM_vector_control() and
# get_OM_xml().
om_gvi_components = function(gvi, merge = TRUE) {
    effects = unname(om_gvi_effects)
    out = list()
    for (k in names(gvi)) {
        comp  = gvi[[k]]
        # Effects of the intervention (no pre-prandial killing with the dummy
        # host); absent effects are written with value 0
        present = intersect(effects, names(comp))
        mosqs = names(comp[[present[1]]]$anophelesParams)
        same  = merge && all(vapply(present[-1], function(e)
            identical(comp[[e]]$decay, comp[[present[1]]]$decay), logical(1)))
        groups = if (same) list(present) else as.list(present)

        for (g in groups) {
            id = if (same) k else paste0(k, "-", g)
            params = lapply(mosqs, function(m) {
                vals = lapply(effects, function(e) list(
                    value = if (e %in% g) comp[[e]]$anophelesParams[[m]]$value else "0"))
                names(vals) = effects
                c(list(mosquito   = m,
                       propActive = comp[[g[1]]]$anophelesParams[[m]]$propActive),
                  vals)
            })
            names(params) = rep("anophelesParams", length(params))
            out = c(out, list(component = list(
                id = id, name = id,
                GVI = c(list(decay = comp[[g[1]]]$decay), params))))
        }
    }
    out
}


# Internal: OpenMalaria decay (OMU format, L in years) of one effect of an
# intervention: exact when attrition x insecticide decay is a single shape,
# otherwise the fitted Weibull.
om_gvi_decay = function(interv, comp) {
    s = interv$settings
    shape_list = function(decay, L_days, k) {
        fn = om_decay_name(decay)
        if (fn == "constant") return(list("function" = "constant"))
        out = list(L = om_num(L_days / 365), "function" = fn)
        if (fn %in% c("weibull", "hill", "smooth-compact")) out$k = om_num(k)
        out
    }

    attrition = shape_list(s$decay_attrition, s$halflife_attrition, s$shape_attrition)

    # Insecticide decay of this effect, with the priorities of the model
    # (make_mult_matrices()): an explicit shape, then a linear half-life (NA:
    # unspecified, Inf: no decay)
    ins = NULL
    if (isTRUE(s$insecticide_decay)) {
        cd = s$component_decay[[comp]]
        hl = s$halflife_insecticide
        hl = if (!is.null(hl) && comp %in% names(hl)) unname(hl[[comp]]) else NA_real_
        if (!is.null(cd$decay) && !is.na(cd$decay)) {
            ins = shape_list(cd$decay, cd$L, cd$kappa)
        } else if (!is.na(hl)) {
            if (hl < 0 && attrition[["function"]] == "constant")
                stop("Negative halflife_insecticide for ", comp, " (efficacy ",
                     "increasing over time) has no OpenMalaria decay shape; use ",
                     "insecticide_decay = FALSE.")
            if (hl < 0)
                ins = list("function" = "increasing")   # only via the fitted Weibull
            else if (is.finite(hl))
                ins = shape_list("linear", 365 * hl, NULL)
        }
        if (!is.null(ins) && ins[["function"]] == "constant") ins = NULL
    }

    if (is.null(ins)) return(attrition)
    if (attrition[["function"]] == "constant") return(ins)

    fd = interv$fitted_decay[interv$fitted_decay$component == comp, ]
    message("OpenMalaria export: ", comp, " decay = Weibull fitted to attrition x ",
            "insecticide decay (rmse ", signif(fd$rmse, 3), ")")
    list(L = om_num(fd$L / 365), "function" = "weibull", k = om_num(fd$kappa))
}


# Internal: Weibull decay (L in years, k) of one effect, for templates where
# all interventions share the decay function: the exact decay if it is a
# Weibull, a very slow Weibull if constant, otherwise a Weibull fitted to the
# exact decay over the intervention duration.
om_weibull_decay = function(interv, comp) {
    d  = suppressMessages(om_gvi_decay(interv, comp))
    fn = d[["function"]]
    if (fn == "weibull") return(c(L = as.numeric(d$L), k = as.numeric(d$k)))
    if (fn == "constant") return(c(L = 1e6, k = 1))
    days  = seq_len(interv$duration) - 1
    curve = decay_function(L = 365 * as.numeric(d$L),
                           kappa = if (!is.null(d$k)) as.numeric(d$k),
                           datej = days, decay = fn)
    fit = fit_weibull_decay(curve, days)
    message("om_vector_control: ", comp, " decay '", fn, "' approximated by a ",
            "Weibull (rmse ", signif(fit[["rmse"]], 3), ")")
    c(L = fit[["L"]] / 365, k = fit[["kappa"]])
}


# Internal: decay function of an effect in a template (list of
# interventions): the exact function when all the interventions have the same
# one (e.g. a step for historical nets), otherwise a Weibull (exact or fitted
# per intervention)
om_template_fn = function(nets, comp) {
    fns = vapply(nets, function(n)
        suppressMessages(om_gvi_decay(n, comp))[["function"]], character(1),
        USE.NAMES = FALSE)
    if (length(unique(fns)) == 1L) fns[1] else "weibull"
}

# Internal: decay parameters (L in years, k or NA) of an effect of one
# intervention, for the template decay function fn
om_template_decay_values = function(net, comp, fn) {
    if (fn == "weibull") return(om_weibull_decay(net, comp))
    d = suppressMessages(om_gvi_decay(net, comp))
    c(L = if (!is.null(d$L)) as.numeric(d$L) else NA_real_,
      k = if (!is.null(d$k)) as.numeric(d$k) else NA_real_)
}

# Internal: OpenMalaria decay of a template effect, with placeholders for its
# parameters (none for a constant decay)
om_template_decay = function(fn, lab) {
    if (fn == "constant") return(list("function" = "constant"))
    out = list(L = paste0("@", lab, "_L@"), "function" = fn)
    if (fn %in% c("weibull", "hill", "smooth-compact")) out$k = paste0("@", lab, "_k@")
    out
}


# Internal: the interventions to export, as list(nets, template): a single
# intervention (template = FALSE), or a named list of them, all with the same
# model (template = TRUE, values as placeholders). Each is a specification
# (om_vector_control()) or an intervention object (build_intervention()).
om_nets = function(interv) {
    one = om_unwrap(interv)
    if (!is.null(one)) return(list(nets = list(one), template = FALSE))
    nets = if (is.list(interv)) lapply(interv, om_unwrap) else list(NULL)
    if (!length(nets) || any(vapply(nets, is.null, logical(1))))
        stop("interv must be an intervention from om_vector_control() (or ",
             "build_intervention()), or a named list of them")
    if (is.null(names(interv)) || any(names(interv) == "") ||
        anyDuplicated(names(interv)))
        stop("A list of interventions needs unique, non-empty names")
    modes = unique(vapply(nets, om_item_mode, character(1)))
    if (length(modes) > 1L)
        stop("All interventions of a list must use the same model (standard or ",
             "dummy_host): the XML structure is shared")
    list(nets = nets, template = TRUE)
}

om_name = function(x, name, intervention_type) {
    if (!is.null(name)) {
        if (!is.character(name) || length(name) != 1L || is.na(name) || name == "")
            stop("name must be a single non-empty string")
        return(name)
    }
    if (x$template) intervention_type
    else om_default_name(x$nets[[1]], intervention_type)
}

# Internal: validated deployment schedule. Coverages are numbers in [0, 1] or
# placeholders "@...@".
om_schedule = function(schedule, end_date = NULL) {
    if (!is.data.frame(schedule) || nrow(schedule) == 0L ||
        !all(c("date", "coverage") %in% names(schedule)))
        stop("schedule must be a data frame with columns date and coverage, ",
             "one row per campaign")
    dates = tryCatch(as.Date(as.character(schedule$date)), error = function(e) NA)
    if (anyNA(dates)) stop("schedule$date must be dates (\"YYYY-MM-DD\")")
    if (any(diff(dates) <= 0)) stop("schedule$date must be strictly increasing")
    cov  = as.character(schedule$coverage)
    ph   = !is.na(cov) & grepl("^@[^@]+@$", cov)
    num  = suppressWarnings(as.numeric(ifelse(ph, NA, cov)))
    if (any(!ph & (is.na(num) | num < 0 | num > 1)))
        stop("schedule$coverage must be numbers in [0, 1] or placeholders ",
             "such as \"@cov2021@\"")
    out = list(dates = dates, coverage = ifelse(ph, cov, om_num(num)),
               placeholder = ph, num = num, names = gsub("@", "", cov))
    # End of the period: kept in the schedule by om_campaigns(end_date = ), or
    # given as an argument (both: they must agree)
    in_schedule = attr(schedule, "end_date")
    if (!is.null(in_schedule) && !is.null(end_date) &&
        !identical(as.Date(as.character(end_date)), as.Date(in_schedule)))
        stop("end_date (", end_date, ") differs from the end_date of the schedule (",
             in_schedule, ")")
    if (is.null(end_date)) end_date = in_schedule
    if (!is.null(end_date)) {
        end = tryCatch(as.Date(as.character(end_date)), error = function(e) NA)
        if (length(end) != 1L || is.na(end))
            stop("end_date must be a single date (\"YYYY-MM-DD\")")
        if (end <= max(dates)) stop("end_date must be after the last campaign")
        out$end = end
    }
    out
}

# ---------------------------------------------------------------------------
# Host-seeking killing (dummy host) as added non-human hosts
#
# In the package, a covered person has a dummy host of availability
# HostSeekingKilling x insecticide decay (alphai_4, in units of the human
# availability alpha_h), on which every mosquito that finds it dies; attrition
# removes covered people (survival). In OpenMalaria (schema >= 46), each
# campaign k is one added non-human host <name>_dummy_<k> (addNonHumanHosts),
# whose availability relative to an adult human is c_k x HostSeekingKilling /
# alpha_h (alpha_h of the population's model), with biting probability 0. Two
# nonHumanHostsModifications multiply its availability by the attrition and
# by the insecticide decay: availabilityReduction 1 with an increasing decay
# 1 - f gives 1 - (1 - f) = f. Campaigns replace nets, as the GVI does (a
# component redeployed to a person is refreshed, not stacked; campaigns
# reach random, independent shares of the population): host j is thinned by
# (1 - c_k) at each later campaign k (constant availabilityReduction c_k), so
# the total is sum_j c_j g(t - t_j) prod_{k > j, t_k <= t} (1 - c_k), the
# expected GVI effect, exactly.
# ---------------------------------------------------------------------------

# Internal: settings of a specification or an intervention object
om_settings = function(item) if (om_is_spec(item)) item else item$settings

# Internal: host-seeking killing (absolute) of a specification or object
om_hsk_value = function(item) {
    e = om_settings(item)$efficacy
    if ("HostSeekingKilling" %in% names(e)) unname(e[["HostSeekingKilling"]]) else 0
}

# Decay functions with a shape parameter k
om_k_fns = c("weibull", "hill", "smooth-compact")

# A rising linear insecticide decay (negative halflife_insecticide, effect
# 1 + t / |hl| in the model) is written as availabilityReduction
# initial = R / hl (< 0) with an increasing linear decay over R years:
# 1 - (R / hl) t / R = 1 + t / |hl| for t < R, exact over the life of the
# hosts (at most R years)
om_rise_years = 100

# Internal: the two decaying factors of the dummy host's availability of an
# intervention, attrition (att) and insecticide decay of host-seeking killing
# (ins), as list(fn, L (years), k, initial), NULL when constant. Same
# priorities as the model (make_mult_matrices()).
om_dummy_factors = function(item) {
    s = om_settings(item)
    one = function(decay, L_days, k) {
        fn = om_decay_name(decay)
        if (fn == "constant") return(NULL)
        list(fn = fn, L = L_days / 365,
             k = if (fn %in% om_k_fns && !is.null(k)) k else NA_real_, initial = 1)
    }
    att = one(s$decay_attrition, s$halflife_attrition, s$shape_attrition)
    ins = NULL
    if (isTRUE(s$insecticide_decay)) {
        cd = s$component_decay$HostSeekingKilling
        hl = s$halflife_insecticide
        hl = if (!is.null(hl) && "HostSeekingKilling" %in% names(hl))
                 unname(hl[["HostSeekingKilling"]]) else NA_real_
        if (!is.null(cd$decay) && !is.na(cd$decay)) {
            ins = one(cd$decay, cd$L, cd$kappa)
        } else if (!is.na(hl) && is.finite(hl)) {
            ins = if (hl > 0) list(fn = "linear", L = hl, k = NA_real_, initial = 1)
                  else list(fn = "linear", L = om_rise_years, k = NA_real_,
                            initial = om_rise_years / hl)
        }
    }
    list(att = att, ins = ins)
}

# Internal: decay function of a factor for a list of interventions: the one
# they share (constant when none has the factor). Different functions are an
# error: host-seeking killing is written exactly.
om_dummy_fn = function(factors, which) {
    fns = unique(unlist(lapply(factors, function(f) f[[which]]$fn)))
    if (!length(fns)) return("constant")
    if (length(fns) > 1L)
        stop("The interventions have different ", om_dummy_what[[which]],
             " decay functions (", paste(fns, collapse = ", "), "): the dummy ",
             "host of host-seeking killing has one decay function per factor; ",
             "define these interventions separately")
    fns
}
om_dummy_what  = c(att = "attrition", ins = "host-seeking killing insecticide")
# Placeholder labels (after the name) and modification names of the factors
om_dummy_label = c(att = "attrition", ins = "hsk")
om_dummy_mod   = c(att = "attrition", ins = "insecticide")

# Internal: values (L in years, k, initial) of a factor; a factor the
# intervention does not have is neutral (very slow decay)
om_dummy_values = function(f) {
    if (is.null(f)) return(c(L = 1e6, k = 1, initial = 1))
    c(L = f$L, k = f$k, initial = f$initial)
}

# Internal: OpenMalaria <decay> of a factor, increasing (1 - f): numbers v, or
# placeholders @<lab>_L@ / @<lab>_k@
om_dummy_decay = function(fn, v = NULL, lab = NULL) {
    out = list("function" = fn,
               L = if (is.null(lab)) om_num(v[["L"]]) else paste0("@", lab, "_L@"))
    if (fn %in% om_k_fns)
        out$k = if (is.null(lab)) om_num(v[["k"]]) else paste0("@", lab, "_k@")
    out$increasing = "true"
    out
}

# Internal: record of the dummy host of a dummy-host intervention (in the
# record of om_vc_components()): factors per net, shared functions,
# host-seeking killing per net, human availability per population
om_dummy_record = function(items, om_ento, active, several, ph_decay, name) {
    factors = lapply(items, om_dummy_factors)
    if (several && !ph_decay &&
        !all(vapply(factors[-1], identical, logical(1), factors[[1]])))
        stop("The interventions of '", name, "' have different host-seeking ",
             "killing decays (attrition or insecticide decay): use decay = \"placeholder\"")
    mosq = om_ento$shares$mosquito
    alpha_h = vapply(seq_along(mosq), function(j) {
        if (!active[j]) return(NA_real_)
        mp = om_ento$model_params[[mosq[j]]]
        a  = if (!is.null(mp)) mp$host_params$alphai[1] else NA_real_
        if (!is.finite(a) || a <= 0)
            stop("No valid human availability for ", mosq[j], " (its bionomics ",
                 "give no valid model)")
        a
    }, numeric(1))
    list(factors = factors,
         fns     = c(att = om_dummy_fn(factors, "att"), ins = om_dummy_fn(factors, "ins")),
         hsk     = vapply(items, om_hsk_value, numeric(1), USE.NAMES = FALSE),
         alpha_h = alpha_h)
}

# Internal: the dummy hosts of a dummy-host intervention (record rec of
# om_vc_components()) for a schedule: addNonHumanHosts <nonHumanHosts> and
# nonHumanHostsModifications <intervention> lists (with their deployment
# dates). Availabilities are written when one net and numeric coverages fix
# them, decays unless defined with decay = "placeholder"; otherwise
# placeholders (om_scenario_values()). Shared by deploy_OM_vector_control()
# and get_OM_xml().
om_dummy_hosts = function(rec, sch) {
    d = rec$dummy; mosq = rec$mosq; active = rec$active; pn = rec$pn
    n_camp = length(sch$dates)
    if (!is.null(sch$end) && as.numeric(sch$end - sch$dates[1]) > 365 * om_rise_years)
        stop("end_date is more than ", om_rise_years, " years after the first campaign")
    lifespan = function(k) if (is.null(sch$end)) paste0(om_rise_years, "y")
                           else paste0(as.numeric(sch$end - sch$dates[k]), "d")
    fixed_avail = !rec$several && !any(sch$placeholder)

    # Decay modifications (no modification for a constant factor)
    mods = list()
    for (w in c("att", "ins")) {
        fn = d$fns[[w]]
        if (fn == "constant") next
        lab = paste0(pn, "_", om_dummy_label[[w]])
        mods[[w]] = if (rec$decay) {
            list(decay   = om_dummy_decay(fn, lab = lab),
                 initial = if (w == "ins" && fn == "linear") paste0("@", lab, "_initial@")
                           else "1")
        } else {
            v = om_dummy_values(d$factors[[1]][[w]])
            list(decay = om_dummy_decay(fn, v), initial = om_num(v[["initial"]]))
        }
    }

    add = list(); modify = list()
    for (k in seq_len(n_camp)) {
        host = paste0(rec$name, "_dummy_", k)
        time = format(sch$dates[k])
        an = lapply(seq_along(mosq), function(j) list(
            mosquito = mosq[j],
            mosqRelativeAvailabilityHuman = list(value =
                if (!active[j]) "0"
                else if (fixed_avail) om_num(sch$num[k] * d$hsk[1] / d$alpha_h[j])
                else paste0("@", pn, "_dummy_", k, "_pop", j, "@")),
            mosqProbBiting       = list(value = "0"),
            mosqProbFindRestSite = list(value = "0"),
            mosqProbResting      = list(value = "0"),
            hostFecundityFactor  = list(value = "1")))
        names(an) = rep("anopheles", length(an))
        add = c(add, list(nonHumanHosts = list(
            name = host, description = an,
            timed = list(deploy = list(time = time, lifespan = lifespan(k))))))
        reduction = function(initial) {
            red = lapply(mosq, function(m) list(
                mosquito = m, availabilityReduction = list(initial = initial)))
            names(red) = rep("anopheles", length(red))
            red
        }
        for (w in names(mods))
            modify = c(modify, list(intervention = list(
                name = paste0(host, "_", om_dummy_mod[[w]]), nonHumanHostsName = host,
                decay = mods[[w]]$decay, description = reduction(mods[[w]]$initial),
                timed = list(deploy = list(time = time)))))
        # Replacement: campaign k gives new nets to a random share c_k of the
        # population, whose earlier nets are replaced (as a GVI component
        # redeployed to a person is refreshed, not stacked). Each earlier
        # host is thinned by (1 - c_k) from t_k on: availability reduction
        # c_k (the campaign's coverage, number or placeholder), constant.
        for (j in seq_len(k - 1L))
            modify = c(modify, list(intervention = list(
                name = paste0(rec$name, "_dummy_", j, "_replaced_", k),
                nonHumanHostsName = paste0(rec$name, "_dummy_", j),
                decay = list("function" = "constant"),
                description = reduction(sch$coverage[k]),
                timed = list(deploy = list(time = time)))))
    }
    list(name = rec$name, add = add, modify = modify, n = n_camp,
         factors = unname(om_dummy_mod[names(mods)]),
         placeholders = if (!fixed_avail) paste0("pop", which(active), " = ", mosq[active]))
}

# Internal: messages of deploy_OM_vector_control() / get_OM_xml() about the
# dummy hosts
om_dummy_messages = function(h, caller) {
    message(caller, ": host-seeking killing as added non-human hosts ", h$name,
            "_dummy_1..", h$n, " (one per campaign, deployed at its date)",
            if (length(h$factors))
                paste0(", their availability decaying with modifications ",
                       paste0("<host>_", h$factors, collapse = ", ")),
            if (h$n > 1L)
                paste0("; each campaign replaces the nets of its recipients ",
                       "(earlier hosts reduced by its coverage: <host>_replaced_<k>)"),
            "; needs OpenMalaria schema >= ", om_dummy_min_schema, ".")
    if (!is.null(h$placeholders))
        message("  Placeholders @", om_ph_name(h$name), "_dummy_<k>_pop<j>@ (fill with ",
                "om_scenario_values()): ", paste(h$placeholders, collapse = ", "))
    invisible()
}


#########om_vector_control####
#' @title Specify a vector control intervention for OpenMalaria
#'
#' @description Specifies an ITN or IRS intervention for the OpenMalaria
#' export (\code{\link{define_OM_vector_control}},
#' \code{\link{deploy_OM_vector_control}}, \code{\link{get_OM_xml}},
#' \code{\link{om_scenario_values}}) from its efficacies and decays,
#' without a model or a duration: the export builds the intervention with the
#' entomology of the scenario, over the right period. Efficacies and decays
#' are as in \code{\link{build_intervention}}.
#'
#' @param efficacy Named efficacies (\code{Deterrency}, \code{PrePrandial},
#'   \code{PostPrandial}, \code{HostSeekingKilling}), or the output of
#'   \code{\link{get_ITN_params}}, whose efficacy, insecticide decay, model
#'   structure and name are used (unless given here).
#' @param halflife_attrition,shape_attrition,decay_attrition Attrition decay,
#'   as in \code{build_intervention()} (\code{halflife_attrition} in days).
#' @param halflife_insecticide,component_decay,insecticide_decay
#'   Per-component insecticide decay, as in \code{build_intervention()}.
#' @param mode \code{"standard"} or \code{"dummy_host"} (default: from the
#'   efficacies).
#' @param name Optional name (used for the OpenMalaria ids if no other name is
#'   given).
#' @param fit_years Window (years) over which a Weibull is fitted to each
#'   effect's decay when attrition and insecticide decay cannot be exported as
#'   a single OpenMalaria decay (default: 3 years for ITNs, 1 year for IRS,
#'   chosen by \code{intervention_type} at the export).
#'
#' @return An object of class \code{om_vector_control}.
#'
#' @seealso \code{\link{define_OM_vector_control}}, \code{\link{get_ITN_params}}
#'
#' @examples
#' # Built-in parameterisation
#' ig2 = om_vector_control(get_ITN_params("Interceptor G2", "Kibondo",
#'                                        mode = "dummy_host", verbose = FALSE),
#'                         halflife_attrition = 2 * 365, shape_attrition = 2)
#' # Custom net
#' custom = om_vector_control(
#'     c(Deterrency = 0.3, HostSeekingKilling = 0.5, PostPrandial = 0.4),
#'     halflife_attrition = 2 * 365, shape_attrition = 2, name = "Custom ITN")
#'
#' @export
#'
om_vector_control = function(efficacy, halflife_attrition = NULL,
                             shape_attrition = NULL, decay_attrition = "weibull",
                             halflife_insecticide = NULL, component_decay = NULL,
                             insecticide_decay = TRUE, mode = NULL, name = NULL,
                             fit_years = NULL) {
    if (is.list(efficacy) && !is.null(efficacy$efficacy)) {   # get_ITN_params()
        p = efficacy
        efficacy = p$efficacy
        if (is.null(halflife_insecticide)) halflife_insecticide = p$halflife_insecticide
        if (is.null(mode)) mode = p$mode
        if (is.null(name)) name = p$name
    }
    if (!is.null(fit_years)) intervention_npoints(fit_years)   # validates
    spec = structure(list(
        efficacy = efficacy, halflife_attrition = halflife_attrition,
        shape_attrition = shape_attrition, decay_attrition = decay_attrition,
        halflife_insecticide = halflife_insecticide, component_decay = component_decay,
        insecticide_decay = insecticide_decay, mode = mode, name = name,
        fit_years = fit_years), class = "om_vector_control")
    # Validates now (the model does not matter, see om_build_spec())
    spec$mode = om_build_spec(spec, om_default_model(), 1 / 365)$settings$mode
    spec
}

#########om_campaigns####
#' @title Schedule of vector control campaigns
#'
#' @description Builds the \code{schedule} of
#' \code{\link{deploy_OM_vector_control}} and \code{\link{get_OM_xml}} (which
#' keep it for \code{\link{om_scenario_values}}): one row per campaign, with its date
#' and coverage, from regular dates (\code{from}, \code{to}, \code{every}) or
#' given \code{dates}.
#'
#' @param from,to First and last possible dates (\code{"YYYY-MM-DD"}); the
#'   campaigns are \code{seq(from, to, by = every)}.
#' @param every Interval between campaigns, as \code{by} in
#'   \code{\link{seq.Date}}: \code{"1 year"} (default), \code{"6 months"},
#'   \code{"3 weeks"}, ...
#' @param coverage Coverage: a number in [0, 1] or a placeholder (e.g.
#'   \code{"@contITNcov@"}), used for every campaign, or one value per
#'   campaign.
#' @param by_year Logical (default \code{FALSE}): with a placeholder
#'   coverage, one placeholder per year (\code{"@histITNcov@"} becomes
#'   \code{"@histITNcov2015@"}, \code{"@histITNcov2016@"}, ...), as
#'   \code{byyear} of openMalariaUtilities' \code{deploy_it_compat()}.
#' @param dates Dates of the campaigns, instead of \code{from}, \code{to} and
#'   \code{every}.
#' @param end_date Optional end of the simulated period (\code{"YYYY-MM-DD"},
#'   after the last campaign): the dummy hosts of dummy-host interventions
#'   last until then (otherwise 100 years). Kept with the schedule, so that
#'   \code{deploy_OM_vector_control()} and \code{get_OM_xml()} need no
#'   \code{end_date} argument.
#'
#' @return Data frame with \code{date} and \code{coverage}, and the
#' \code{end_date} as an attribute when given (subsetting the data frame
#' drops it).
#'
#' @examples
#' # Annual campaigns, one coverage placeholder per year
#' om_campaigns("2015-01-25", "2018-01-25", coverage = "@histITNcov@", by_year = TRUE)
#' # Same coverage every year, period ending in 2031
#' om_campaigns("2019-01-25", "2030-01-25", coverage = "@contITNcov@",
#'              end_date = "2031-01-01")
#' # Given dates and coverages
#' om_campaigns(dates = c("2021-01-01", "2024-01-01"), coverage = c(0.8, 0.6))
#'
#' @export
#'
om_campaigns = function(from = NULL, to = NULL, every = "1 year", coverage,
                        by_year = FALSE, dates = NULL, end_date = NULL) {
    if (is.null(dates)) {
        if (is.null(from) || is.null(to))
            stop("Give from and to (with every), or dates")
        dates = tryCatch(seq(as.Date(from), as.Date(to), by = every),
                         error = function(e) stop("Invalid from / to / every: ",
                                                  conditionMessage(e), call. = FALSE))
    } else {
        dates = tryCatch(as.Date(as.character(dates)), error = function(e) NA)
        if (anyNA(dates)) stop("dates must be dates (\"YYYY-MM-DD\")")
    }
    if (!length(dates)) stop("No campaign between from and to")
    if (!length(coverage) %in% c(1L, length(dates)))
        stop("coverage must be one value, or one per campaign (", length(dates), ")")
    if (isTRUE(by_year)) {
        if (length(coverage) != 1L || !is.character(coverage) ||
            !grepl("^@[^@]+@$", coverage))
            stop("by_year = TRUE needs one placeholder coverage, e.g. \"@histITNcov@\"")
        years = format(dates, "%Y")
        if (anyDuplicated(years))
            stop("by_year = TRUE needs at most one campaign per year")
        coverage = paste0(sub("@$", "", coverage), years, "@")
    }
    out = data.frame(date = format(dates), coverage = rep(coverage, length.out = length(dates)),
                     stringsAsFactors = FALSE)
    om_schedule(out, end_date)   # validates dates, coverages and end_date
    if (!is.null(end_date)) attr(out, "end_date") = format(as.Date(as.character(end_date)))
    out
}

#########om_continuous####
#' @title Continuous deployment of a vector control intervention
#'
#' @description Builds a continuous \code{schedule} for
#' \code{\link{deploy_OM_vector_control}} and \code{\link{get_OM_xml}}: the
#' intervention is given to people when they reach a target age (e.g. nets
#' through EPI or antenatal care, or at school entry), with a coverage, while
#' the deployment is active (OpenMalaria \code{<continuous>} deployment).
#' Unlike campaigns (\code{\link{om_campaigns}}), it cannot target an age
#' range at given dates: use campaigns with \code{minAge} / \code{maxAge} for
#' that. A net deployed both continuously and by campaigns is deployed twice
#' under the same name (two deployments of the same components).
#'
#' Standard (non dummy-host) interventions only; \code{cumulative},
#' \code{minAge} and \code{maxAge} of the deploy do not apply, \code{subpop}
#' does.
#'
#' @param age Target ages in years (0 to 100), numbers or placeholders (e.g.
#'   \code{"@ageITN@"}); one deploy per age.
#' @param coverage Coverage at each target age: a number in [0, 1] or a
#'   placeholder (e.g. \code{"@contITNcov@"}), one value or one per age.
#' @param begin,end Optional dates (\code{"YYYY-MM-DD"}) between which the
#'   deployment is active (OpenMalaria's defaults: from the start of the
#'   intervention period, without end), one value or one per age.
#'
#' @return A data frame of class \code{om_continuous}, one row per target age
#' (\code{targetAgeYrs}, \code{coverage}, \code{begin}, \code{end}).
#'
#' @seealso \code{\link{om_campaigns}}, \code{\link{deploy_OM_vector_control}}
#'
#' @examples
#' # Nets to infants at 3 months (EPI), from 2021, coverage as a placeholder
#' om_continuous(age = 0.25, coverage = "@contITNcov@", begin = "2021-01-01")
#' # Two channels with their own coverage
#' om_continuous(age = c(0.25, 6), coverage = c(0.8, 0.6),
#'               begin = "2021-01-01", end = "2030-01-01")
#' \dontrun{
#' baseList = deploy_OM_vector_control(baseList, "IG2",
#'     om_continuous(age = 0.25, coverage = 0.8, begin = "2021-01-01"))
#' }
#'
#' @export
#'
om_continuous = function(age, coverage, begin = NULL, end = NULL) {
    n = length(age)
    if (!n) stop("age: give at least one target age")
    per_age = function(x, what) {
        if (is.null(x)) return(rep(NA_character_, n))
        if (!length(x) %in% c(1L, n))
            stop(what, " must be one value, or one per target age (", n, ")")
        rep(as.character(x), length.out = n)
    }
    out = data.frame(targetAgeYrs = as.character(age),
                     coverage = per_age(coverage, "coverage"),
                     begin = per_age(begin, "begin"), end = per_age(end, "end"),
                     stringsAsFactors = FALSE)
    om_check_continuous(out)   # validates
    class(out) = c("om_continuous", class(out))
    out
}


# Internal: a default model, only to validate specifications
om_default_model = function() {
    build_model_obj(def_vector_params(verbose = FALSE), def_host_params(verbose = FALSE))
}


#########define_OM_entomology####
#' @title Define the entomology of an OpenMalaria scenario
#'
#' @description Writes the entomology of \code{\link{get_OM_ento_snippet}}
#' into an openMalariaUtilities \code{baseList}, with
#' \code{openMalariaUtilities::defineEntomology()}, and keeps it in the
#' \code{baseList}, so that \code{\link{define_OM_vector_control}} uses the
#' same mosquito populations and parameters.
#'
#' The \code{annualEIR} of each population is its relative contribution to
#' the EIR (see \code{get_OM_ento_snippet(species_shares = )}); the total EIR
#' is \code{scaledAnnualEIR}. The seasonality gives the shape of the EIR over
#' the year; it must be numeric, since \code{defineEntomology()} currently
#' rejects monthly placeholders with R >= 4.3 (an openMalariaUtilities issue;
#' \code{get_OM_xml()} writes the entomology with placeholders).
#'
#' The animal host parameters of each population are those of \code{om_ento}
#' (its \code{hosts_p}), as in \code{\link{get_OM_xml}}:
#' \code{defineEntomology()} writes fixed values (0.95, 0.95, 0.99), which
#' are replaced. The schema version is that of the \code{baseList}
#' (\code{OMVersion}, 44 if absent); from schema 49, \code{propInfected} and
#' \code{propInfectious} (removed from the schema) are not written.
#'
#' Requires the openMalariaUtilities package.
#'
#' @param baseList openMalariaUtilities \code{baseList}.
#' @param om_ento Output of \code{\link{get_OM_ento_snippet}}.
#' @param seasonality Numeric vector of 12 monthly (or 365 daily) values for
#'   all populations, or a named list with one such vector per population
#'   (names as in \code{om_ento$shares$mosquito}).
#' @param scaledAnnualEIR Total annual EIR: a number or a placeholder
#'   (default \code{"@EIR@"}).
#' @param ... Passed to \code{defineEntomology()} (e.g. \code{name},
#'   \code{mode}).
#'
#' @return The updated \code{baseList}.
#'
#' @seealso \code{\link{get_OM_ento_snippet}}, \code{\link{define_OM_vector_control}}
#'
#' @export
#'
define_OM_entomology = function(baseList, om_ento, seasonality,
                                scaledAnnualEIR = "@EIR@", ...) {
    if (!requireNamespace("openMalariaUtilities", quietly = TRUE))
        stop("define_OM_entomology() requires the openMalariaUtilities package ",
             "(get_OM_xml() writes XML text without it)")
    mosq = om_ento$shares$mosquito
    if (is.null(mosq)) stop("om_ento must be the output of get_OM_ento_snippet()")
    seas = if (is.list(seasonality)) seasonality else rep(list(seasonality), length(mosq))
    if (is.list(seasonality)) {
        if (is.null(names(seas)) || !setequal(names(seas), mosq))
            stop("A list seasonality needs one element per population, named as ",
                 "om_ento$shares$mosquito")
        seas = seas[mosq]
    }
    for (s in seas)
        if (!is.numeric(s) || !length(s) %in% c(12L, 365L) || any(!is.finite(s)) ||
            any(s < 0))
            stop("seasonality must be 12 monthly or 365 daily non-negative numbers ",
                 "(openMalariaUtilities currently rejects placeholders; use ",
                 "get_OM_xml() to keep them)")
    params = om_ento$seasonalityParameters
    for (j in seq_along(mosq)) params[[mosq[j]]]$seasonality = seas[[j]]
    baseList = openMalariaUtilities::defineEntomology(
        baseList, params, om_ento$mosquitoParameters,
        scaledAnnualEIR = scaledAnnualEIR, ...)
    baseList$entomology$vector = om_fix_omu_vector(baseList$entomology$vector, om_ento,
                                                   om_baselist_schema(baseList))
    baseList$anophelesmodel2$ento = om_ento
    baseList
}

# Internal: <vector> written by openMalariaUtilities::defineEntomology(),
# made identical to ours (om_vector_list()): the animal host parameters of
# each population are those of om_ento (defineEntomology() writes fixed
# values, 0.95 / 0.95 / 0.99), matched by population and host name; from
# schema 49, without propInfected / propInfectious (always written by
# defineEntomology(), removed in 49).
om_fix_omu_vector = function(v, om_ento, schema) {
    for (i in which(names(v) == "anopheles")) {
        a = v[[i]]
        m = om_ento$mosquitoParameters[[as.character(a$mosquito)]]
        if (is.null(m))
            stop("define_OM_entomology: population '", a$mosquito, "' written by ",
                 "defineEntomology() is not in om_ento")
        idx = which(names(a) == "nonHumanHosts")
        ours = m$nonHumanHosts
        if (is.null(ours)) {
            if (length(idx)) a = a[-idx]
        } else {
            omu_names = vapply(a[idx], function(h) as.character(h$name), character(1))
            if (length(idx) != 1L || !identical(unname(omu_names), ours$name))
                stop("define_OM_entomology: the animal hosts written by defineEntomology() ",
                     "for '", a$mosquito, "' (", paste(omu_names, collapse = ", "),
                     ") differ from those of om_ento (", ours$name, ")")
            a[[idx]] = ours
        }
        if (schema >= 49L) {
            a$propInfected   = NULL
            a$propInfectious = NULL
        }
        v[[i]] = a
    }
    v
}

# Internal: the entomology kept by define_OM_entomology()
om_baselist_ento = function(baseList) {
    om_ento = baseList$anophelesmodel2$ento
    if (is.null(om_ento))
        stop("No entomology in baseList: call define_OM_entomology() first ",
             "(the interventions use its mosquito populations and parameters)")
    om_ento
}


#########define_OM_vector_control####
#' @title Define a vector control intervention in an OpenMalaria scenario
#'
#' @description Writes the definition of a vector control intervention (ITN or
#' IRS) into an openMalariaUtilities \code{baseList}, for both model
#' structures: the components of the generic vector intervention (GVI) of
#' \code{\link{add_OM_GVI}} (deterrency, pre-prandial killing in the standard
#' model, post-prandial killing). Deploy it with
#' \code{\link{deploy_OM_vector_control}} and the same \code{name}.
#'
#' For dummy-host interventions, the host-seeking killing is written by
#' \code{deploy_OM_vector_control()}, one dummy host per campaign;
#' \code{define_OM_vector_control()} keeps what it needs in
#' \code{baseList$anophelesmodel2} (not written to the XML) and says so in a
#' message.
#'
#' \strong{Host-seeking killing (OpenMalaria schema >= 46), work in
#' progress:} this part has not been validated in OpenMalaria yet, and gives
#' a warning (silenced with \code{options(AnophelesModel2.om_dummy_host =
#' TRUE)}); the export of standard interventions is validated. OpenMalaria's
#' GVI has no host-seeking killing. In this package it is a dummy host per
#' covered person, of availability \code{HostSeekingKilling} \eqn{\times}
#' insecticide decay (relative to the human availability \eqn{\alpha_h}),
#' on which every mosquito that finds it dies. It is written exactly as
#' OpenMalaria non-human hosts: for each campaign \eqn{k} (date \eqn{t_k},
#' coverage \eqn{c_k}), an added non-human host \code{<name>_dummy_<k>}
#' (\code{addNonHumanHosts}, deployed at \eqn{t_k}), of availability
#' relative to an adult human \eqn{c_k \times} \code{HostSeekingKilling}
#' \eqn{/ \alpha_h} (\eqn{\alpha_h} of the population's model) and biting
#' probability 0, whose availability decays with the attrition and the
#' insecticide decay of host-seeking killing (two
#' \code{nonHumanHostsModifications} \code{<name>_dummy_<k>_attrition} and
#' \code{_insecticide}: an availability reduction of 1 with an
#' \code{increasing} decay multiplies the availability by the decay). As for
#' the GVI, whose components are refreshed (not stacked) in people who
#' receive them again, a campaign \strong{replaces} the nets of its
#' recipients: each earlier dummy host is reduced by the coverage \eqn{c_k}
#' of every later campaign \eqn{k}
#' (\code{<name>_dummy_<j>_replaced_<k>}, constant availability reduction
#' \eqn{c_k}, the same number or placeholder as the schedule), which gives
#' the expected effect of campaigns reaching random, independent shares of
#' the population. Increasing decays need OpenMalaria schema 46 or later (developed for
#' 49): the \code{OMVersion} of the \code{baseList}. A rising insecticide
#' decay (negative \code{halflife_insecticide}) is written with a negative
#' availability reduction, exactly. Host-seeking killing acts on the same
#' populations as the GVI (in bed for ITNs).
#'
#' \strong{Placeholders, only where something varies.}
#' \itemize{
#'   \item \strong{Efficacies:} written for one intervention; placeholders
#'     (\code{"@<name>_deterrency@"}, \code{"@<name>_preprandial@"},
#'     \code{"@<name>_postprandial@"}) for a \strong{named list} of several
#'     interventions (e.g. nets or parameterisations to choose between in
#'     scenarios). A list defines \strong{one} intervention, deployed under
#'     \code{name}; each scenario uses the values of one of its interventions.
#'   \item \strong{Decays:} with \code{decay = "fixed"} (default), written
#'     from the interventions (which must then have the same decays); with
#'     \code{decay = "placeholder"}, \code{"@<name>_deterrency_L@"} (and
#'     \code{_k} when the decay function has a shape parameter), ... Each
#'     effect keeps its decay function when it is the same for all the
#'     interventions (e.g. a \code{step} for historical nets), and is
#'     otherwise a Weibull (exact, or fitted).
#'   \item \strong{Dummy hosts:} availabilities written when one intervention
#'     and numeric coverages fix them; otherwise
#'     \code{"@<name>_dummy_<k>_pop<j>@"} (campaign \eqn{k}, mosquito
#'     population \eqn{j}: row of \code{om_ento$shares}, printed by the
#'     message). Their decays follow \code{decay}: with
#'     \code{"placeholder"}, \code{"@<name>_attrition_L@"} (and \code{_k}) and
#'     \code{"@<name>_hsk_L@"} (and \code{_k}, or \code{_initial} for a
#'     linear decay, negative when rising); the decay functions must be the
#'     same for all the interventions.
#' }
#' All interventions of a list must use the same model structure. Fill the
#' placeholders of a scenario table with \code{\link{om_scenario_values}}.
#'
#' Requires the openMalariaUtilities package (see \code{\link{add_OM_GVI}}).
#'
#' @param baseList openMalariaUtilities \code{baseList}, with the entomology
#'   written by \code{\link{define_OM_entomology}} (whose mosquito populations
#'   and parameters are used).
#' @param interv Intervention from \code{\link{om_vector_control}} (or an
#'   object from \code{\link{build_intervention}}), or a named list of them
#'   (template).
#' @param intervention_type \code{"ITN"} (acts in bed) or \code{"IRS"} (acts on
#'   indoor resting mosquitoes).
#' @param name Name of the intervention, used for component ids and
#'   placeholders (default: the intervention description, or
#'   \code{intervention_type}).
#' @param decay \code{"fixed"} (default: decays written from the
#'   interventions) or \code{"placeholder"} (decay parameters as placeholders,
#'   e.g. when the attrition varies between scenarios).
#'
#' @return The updated \code{baseList}.
#'
#' @seealso \code{\link{deploy_OM_vector_control}},
#'   \code{\link{om_scenario_values}}, \code{\link{get_ITN_params}}
#'
#' @examples
#' \dontrun{
#' # Built-in and custom dummy-host nets (other nets: list_ITN_params())
#' ig2 = om_vector_control(get_ITN_params("Interceptor G2", "Kibondo",
#'                                        mode = "dummy_host"),
#'                         halflife_attrition = 2 * 365, shape_attrition = 2)
#' custom = om_vector_control(
#'     c(Deterrency = 0.3, HostSeekingKilling = 0.5, PostPrandial = 0.4),
#'     halflife_attrition = 2 * 365, shape_attrition = 2)
#' nets = list(IG2 = ig2, custom = custom)
#' om_ento  = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
#'                                activity = def_activity_patterns())
#' baseList$OMVersion = 49L     # dummy hosts need schema >= 46
#' baseList = define_OM_entomology(baseList, om_ento, seasonality = rep(1, 12))
#' baseList = define_OM_vector_control(baseList, nets, name = "ITN",
#'                                     decay = "placeholder")
#' schedule = om_campaigns("2021-01-01", "2027-01-01", every = "3 years",
#'                         coverage = "@cov@", by_year = TRUE,
#'                         end_date = "2030-01-01")
#' baseList = deploy_OM_vector_control(baseList, "ITN", schedule)
#' # The user's scenario table, filled with the values of the placeholders
#' scens = expand.grid(net = c("IG2", "custom"), attrition = c(2, 3),
#'                     cov2021 = c(0.5, 0.8), cov2024 = 0.6, cov2027 = 0.6)
#' scens = om_scenario_values(scens, baseList, "ITN", net = "net",
#'                            halflife = "attrition")
#' }
#'
#' @export
#'
define_OM_vector_control = function(baseList, interv,
                                    intervention_type = c("ITN", "IRS"),
                                    name = NULL, decay = c("fixed", "placeholder")) {
    intervention_type = match.arg(intervention_type)
    decay = match.arg(decay)
    om_ento = om_baselist_ento(baseList)
    b = om_vc_components(interv, om_ento, intervention_type, name, decay,
                         schema = om_baselist_schema(baseList))
    ids = vapply(b$components, `[[`, character(1), "id")
    human = baseList$interventions$human
    if (any(ids %in% vapply(human[names(human) == "component"],
                            function(cm) as.character(cm$id), character(1))))
        stop("baseList already has components of '", b$name, "'")

    baseList = om_add_components(baseList, b$components)
    # Record of the intervention (nets, placeholders, populations), for
    # deploy_OM_vector_control() and om_scenario_values(). Kept in a top-level
    # entry of baseList that openMalariaUtilities does not write to the XML
    # (its writer only writes the scenario sections it knows).
    baseList$anophelesmodel2$vc[[b$name]] = b$rec
    if (b$mode == "dummy_host")
        message("define_OM_vector_control: the host-seeking killing of '", b$name,
                "' (dummy hosts, one per campaign) will be added by ",
                "deploy_OM_vector_control(), with the whole campaign schedule.")
    baseList
}


# Internal: the <component> lists of an intervention (XML structure of
# openMalariaUtilities, no baseList needed) and its record (rec): nets, which
# parts are placeholders, populations, what the dummy hosts need. Shared by
# define_OM_vector_control() and get_OM_xml().
#
# Placeholders only where something varies: efficacies when there are
# several nets, decays with decay = "placeholder" (otherwise the nets'
# decays, which must then be identical), dummy-host availabilities unless
# one net and numeric coverages fix them (om_dummy_hosts()).
om_vc_components = function(interv, om_ento, intervention_type, name,
                            decay = "fixed", schema = 44L) {
    x     = om_nets(interv)
    name  = om_name(x, name, intervention_type)
    items = x$nets
    mode  = om_item_mode(items[[1]])
    if (mode == "dummy_host") om_check_dummy_enabled()
    if (mode == "dummy_host" && schema < om_dummy_min_schema)
        stop("The host-seeking killing of dummy-host interventions is written as ",
             "non-human hosts with increasing decays, which need OpenMalaria schema >= ",
             om_dummy_min_schema, " (developed for 49): use schema = 49 in get_OM_xml(), ",
             "or OMVersion = 49L in the baseList (schema ", schema, " here)")
    nets  = om_gvi_objects(items, om_ento, intervention_type)
    pn    = om_ph_name(name)
    mosq   = om_ento$shares$mosquito
    active = om_active(om_ento, intervention_type)
    comps  = om_model_effects(mode)
    several  = length(nets) > 1L
    ph_decay = decay == "placeholder"

    if (several && !ph_decay) for (comp in comps) {
        d = lapply(nets, function(n) suppressMessages(om_gvi_decay(n, comp)))
        if (!all(vapply(d[-1], identical, logical(1), d[[1]])))
            stop("The interventions of '", name, "' have different ", comp,
                 " decays (attrition or insecticide decay): with decay = \"fixed\" ",
                 "(default) the decays are written as numbers, shared by all the ",
                 "interventions; use decay = \"placeholder\" and fill them per ",
                 "scenario with om_scenario_values()")
    }
    # Decay function of each effect with placeholders: the one shared by the
    # nets, otherwise a Weibull
    fns = stats::setNames(lapply(comps, function(comp) om_template_fn(nets, comp)), comps)

    if (!several && !ph_decay) {
        # Everything fixed: values written, merged component when possible
        components = om_gvi_components(om_gvi_list(nets[[1]], om_ento,
                                                   intervention_type, name))
    } else {
        gvi = lapply(comps, function(comp) {
            lab = paste0(pn, "_", om_ph_label[[comp]])
            eff = nets[[1]]$settings$efficacy
            value = if (several) paste0("@", lab, "@")
                    else om_num(if (comp %in% names(eff)) eff[[comp]] else 0)
            params = lapply(seq_along(active), function(j)
                list(propActive = if (active[j]) 1 else 0,
                     value      = if (active[j]) value else "0"))
            names(params) = mosq
            dec = if (ph_decay) {
                for (net in nets) om_template_decay_values(net, comp, fns[[comp]])  # checks
                om_template_decay(fns[[comp]], lab)
            } else suppressMessages(om_gvi_decay(nets[[1]], comp))
            list(decay = dec, anophelesParams = params)
        })
        names(gvi) = unname(om_gvi_effects[comps])
        components = om_gvi_components(stats::setNames(list(gvi), name), merge = FALSE)
    }

    rec = list(
        name = name, pn = pn, items = items, members = if (x$template) names(interv),
        several = several, decay = ph_decay, fns = fns, comps = comps, mode = mode,
        intervention_type = intervention_type, mosq = mosq, active = active,
        model_p = om_model_for(om_ento), schema = schema,
        dummy = if (mode == "dummy_host")
                    om_dummy_record(items, om_ento, active, several, ph_decay, name))
    list(name = name, mode = mode, components = components, rec = rec)
}


#########deploy_OM_vector_control####
#' @title Deploy a vector control intervention in an OpenMalaria scenario
#'
#' @description Writes the deployment of an intervention defined with
#' \code{\link{define_OM_vector_control}}: \strong{one} OpenMalaria
#' \code{<deployment>} containing all its GVI components (one per effect, or
#' one merged component), so that all effects reach the same people, with one
#' \code{<deploy>} per campaign of \code{schedule}.
#'
#' For dummy-host interventions, it also writes the host-seeking killing: one
#' dummy host per campaign (\code{addNonHumanHosts}, with its decays in
#' \code{nonHumanHostsModifications}; see
#' \code{\link{define_OM_vector_control}}), which needs OpenMalaria schema
#' >= 46. Such an intervention is deployed \strong{once}, with the whole
#' schedule; \code{subpop}, \code{cumulative}, \code{minAge} and
#' \code{maxAge} are rejected (the dummy hosts assume campaigns covering the
#' whole population at the schedule coverages).
#'
#' @param baseList openMalariaUtilities \code{baseList}, after
#'   \code{define_OM_vector_control()}.
#' @param name Name of the intervention given to (or printed by)
#'   \code{define_OM_vector_control()}.
#' @param schedule Data frame with one row per campaign: \code{date}
#'   (\code{"YYYY-MM-DD"}, increasing) and \code{coverage} (numbers in [0, 1]
#'   or placeholders such as \code{"@cov2021@"}), e.g. from
#'   \code{\link{om_campaigns}}; or a continuous schedule from
#'   \code{\link{om_continuous}} (deployment at target ages; standard model,
#'   without \code{cumulative}, \code{minAge}, \code{maxAge}).
#' @param end_date Optional end of the simulated period (\code{"YYYY-MM-DD"}):
#'   the dummy hosts of dummy-host interventions last until then (otherwise
#'   100 years). It can also be kept in the schedule
#'   (\code{\link{om_campaigns}(end_date = )}; if both are given, they must
#'   agree).
#' @param subpop Optional sub-population id suffix: the deployment is
#'   restricted to the sub-population \code{"<name>-<subpop>"}
#'   (\code{restrictToSubPop}). This and the next arguments: standard model
#'   only.
#' @param cumulative Logical (default \code{FALSE}): cumulative coverage
#'   (\code{cumulativeCoverage}, people already covered are not redeployed),
#'   measured on \code{"<name>-<subpop>"} or the first component.
#' @param minAge,maxAge Optional age limits of the deployment, in years
#'   (numbers or placeholders): \strong{both or neither} (all ages), e.g.
#'   \code{minAge = 0, maxAge = 5} for under-fives. Several age groups:
#'   one deploy per group.
#'
#' @return The updated \code{baseList}.
#'
#' @seealso \code{\link{define_OM_vector_control}}
#'
#' @export
#'
deploy_OM_vector_control = function(baseList, name, schedule, end_date = NULL,
                                    subpop = NULL, cumulative = FALSE,
                                    minAge = NULL, maxAge = NULL) {
    if (!is.character(name) || length(name) != 1L || is.na(name))
        stop("name must be a single string")

    human = baseList$interventions$human
    ids = vapply(human[names(human) == "component"],
                 function(cm) as.character(cm$id), character(1))
    ids = ids[ids == name | ids %in% paste0(name, "-", om_gvi_effects)]
    if (!length(ids)) {
        recs = baseList$anophelesmodel2$vc
        in_list = names(recs)[vapply(recs, function(r) name %in% r$members, logical(1))]
        if (length(in_list))
            stop("'", name, "' is one of the interventions of the list defined as '",
                 in_list[1], "': a list defines one intervention, whose values are ",
                 "chosen per scenario (om_scenario_values()); deploy '",
                 in_list[1], "'")
        stop("No component '", name, "' or '", name, "-<effect>' in baseList: ",
             "call define_OM_vector_control() first")
    }

    # Dummy host: the dummy hosts of the campaigns are written here, from the
    # record of define_OM_vector_control() and the whole schedule
    rec = baseList$anophelesmodel2$vc[[name]]
    dummy = !is.null(rec) && rec$mode == "dummy_host"
    if (dummy) om_check_hsk_deploy(subpop, cumulative, minAge, maxAge)
    d = om_build_deployment(name, ids, schedule, end_date, subpop, cumulative,
                            minAge, maxAge, dummy)
    sch = d$sch
    if (dummy) {
        schema = om_baselist_schema(baseList)
        if (schema < om_dummy_min_schema)
            stop("The dummy hosts of '", name, "' need OpenMalaria schema >= ",
                 om_dummy_min_schema, " (OMVersion of the baseList: ", schema, ")")
        nhh = baseList$interventions$addNonHumanHosts
        existing = vapply(nhh[names(nhh) == "nonHumanHosts"],
                          function(v) as.character(v$name), character(1))
        if (any(startsWith(existing, paste0(name, "_dummy_"))))
            stop("'", name, "' is already deployed: deploy a dummy-host ",
                 "intervention once, with the whole campaign schedule")
        h = om_dummy_hosts(rec, sch)
    }

    baseList$interventions$human = c(human, list(deployment = d$deployment))
    if (dummy) {
        baseList$interventions$addNonHumanHosts =
            c(baseList$interventions$addNonHumanHosts, h$add)
        if (length(h$modify))
            baseList$interventions$nonHumanHostsModifications =
                c(baseList$interventions$nonHumanHostsModifications, h$modify)
    }
    # The schedule, for om_scenario_values() (coverages of the dummy hosts)
    if (!is.null(rec) && !is.null(sch)) baseList$anophelesmodel2$vc[[name]]$sch = sch
    message("deploy_OM_vector_control: deployment '", name, "' with components ",
            paste(ids, collapse = ", "), ", ", d$when)
    if (dummy) om_dummy_messages(h, "deploy_OM_vector_control")
    baseList
}


# Internal: dummy-host interventions cannot be restricted (the dummy hosts
# assume campaigns covering the whole population)
om_check_hsk_deploy = function(subpop, cumulative, minAge, maxAge) {
    if (!is.null(subpop) || isTRUE(cumulative) || !is.null(minAge) || !is.null(maxAge))
        stop("subpop, cumulative, minAge and maxAge are not supported for ",
             "dummy-host interventions: the dummy hosts assume campaigns ",
             "covering the whole population at the schedule coverages")
}

# Internal: age limits of a deployment: both or neither (a single limit has
# led to deployments without the intended age restriction), each a number
# >= 0 or a placeholder, minAge < maxAge when both are numbers
om_check_ages = function(minAge, maxAge) {
    if (is.null(minAge) && is.null(maxAge)) return(invisible())
    if (is.null(minAge) || is.null(maxAge))
        stop("Give both minAge and maxAge (years), or neither (all ages): ",
             "e.g. minAge = 0, maxAge = 5 for under-fives, minAge = 15, maxAge = 100 ",
             "for adults")
    check = function(x, what) {
        ok = length(x) == 1L && !is.na(x) &&
            ((is.numeric(x) && x >= 0) ||
             (is.character(x) && grepl("^@[^@]+@$", x)))
        if (!ok) stop(what, " must be a number >= 0 (years) or a placeholder such as \"@",
                      what, "@\"")
    }
    check(minAge, "minAge")
    check(maxAge, "maxAge")
    if (is.numeric(minAge) && is.numeric(maxAge) && minAge >= maxAge)
        stop("minAge (", minAge, ") must be smaller than maxAge (", maxAge, ")")
    invisible()
}

# Internal: the <deployment> of an intervention, from a campaign schedule
# (om_campaigns() or a data frame) or a continuous one (om_continuous()):
# list(deployment, sch (campaigns; NULL if continuous), when (for messages)).
# Shared by deploy_OM_vector_control() and get_OM_xml().
om_build_deployment = function(name, ids, schedule, end_date = NULL, subpop = NULL,
                               cumulative = FALSE, minAge = NULL, maxAge = NULL,
                               dummy = FALSE) {
    if (inherits(schedule, "om_continuous")) {
        if (dummy)
            stop("Continuous deployment is not supported for dummy-host ",
                 "interventions (their dummy hosts are written per campaign)")
        if (isTRUE(cumulative) || !is.null(minAge) || !is.null(maxAge))
            stop("cumulative, minAge and maxAge are for campaigns: a continuous ",
                 "deployment gives the intervention at its target ages ",
                 "(om_continuous(age = ))")
        if (!is.null(end_date))
            stop("end_date is for campaigns: a continuous deployment has its own ",
                 "begin and end (om_continuous())")
        cont = om_check_continuous(schedule)
        return(list(deployment = om_deployment_continuous(name, ids, cont, subpop),
                    sch = NULL,
                    when = paste0("continuous, at target ages ",
                                  paste(cont$targetAgeYrs, collapse = ", "))))
    }
    sch = om_schedule(schedule, end_date)
    list(deployment = om_deployment(name, ids, sch, subpop, cumulative, minAge, maxAge),
         sch = sch, when = paste0("at ", paste(format(sch$dates), collapse = ", ")))
}

# Internal: validated continuous schedule (om_continuous()), as strings:
# target ages (years, 0 to 100) and coverages ([0, 1]) as numbers or
# placeholders, begin / end dates or NA
om_check_continuous = function(x) {
    num_or_ph = function(v, what, hi) {
        v  = as.character(v)
        ph = !is.na(v) & grepl("^@[^@]+@$", v)
        num = suppressWarnings(as.numeric(ifelse(ph, NA, v)))
        if (any(!ph & (is.na(num) | num < 0 | num > hi)))
            stop(what, " must be numbers in [0, ", hi, "] or placeholders such as \"@",
                 what, "@\"")
        ifelse(ph, v, om_num(num))
    }
    date_of = function(v, what) {
        v = as.character(v)
        d = as.Date(v, format = "%Y-%m-%d")
        if (any(!is.na(v) & is.na(d))) stop(what, " must be dates (\"YYYY-MM-DD\") or NA")
        d
    }
    begin = date_of(x$begin, "begin")
    end   = date_of(x$end, "end")
    if (any(!is.na(begin) & !is.na(end) & end <= begin)) stop("end must be after begin")
    list(targetAgeYrs = num_or_ph(x$targetAgeYrs, "age", 100),
         coverage     = num_or_ph(x$coverage, "coverage", 1),
         begin = ifelse(is.na(begin), NA_character_, format(begin)),
         end   = ifelse(is.na(end), NA_character_, format(end)))
}

# Internal: <deployment> list of a continuous deployment: all the components
# (same people), then <continuous> with one deploy per target age (same
# structure as openMalariaUtilities' deploy_cont_compat())
om_deployment_continuous = function(name, ids, cont, subpop = NULL) {
    continuous = list()
    if (!is.null(subpop))
        continuous$restrictToSubPop = list(id = paste0(name, "-", subpop))
    for (k in seq_along(cont$targetAgeYrs)) {
        d = list(coverage = cont$coverage[k], targetAgeYrs = cont$targetAgeYrs[k],
                 begin = cont$begin[k], end = cont$end[k])
        continuous = c(continuous, list(deploy = d[!is.na(unlist(d))]))
    }
    components = lapply(unname(ids), function(i) list(id = i))
    names(components) = rep("component", length(components))
    c(list(name = name), components, list(continuous = continuous))
}

# Internal: <deployment> list of an intervention: all its components (same
# people), then one deploy per campaign.
om_deployment = function(name, ids, sch, subpop = NULL, cumulative = FALSE,
                         minAge = NULL, maxAge = NULL) {
    om_check_ages(minAge, maxAge)
    timed = list()
    if (!is.null(subpop))
        timed$restrictToSubPop = list(id = paste0(name, "-", subpop))
    if (isTRUE(cumulative))
        timed$cumulativeCoverage = list(
            component = if (!is.null(subpop)) paste0(name, "-", subpop) else ids[[1]])
    for (k in seq_along(sch$dates)) {
        d = list(coverage = sch$coverage[k], time = format(sch$dates[k]))
        if (!is.null(minAge)) d$minAge = as.character(minAge)
        if (!is.null(maxAge)) d$maxAge = as.character(maxAge)
        timed = c(timed, list(deploy = d))
    }
    components = lapply(unname(ids), function(i) list(id = i))
    names(components) = rep("component", length(components))
    c(list(name = name), components, list(timed = timed))
}


#########om_scenario_values####
#' @title Fill the vector control placeholders of a scenario table
#'
#' @description Adds to the user's OpenMalaria scenario table (one row per
#' scenario, built as the user wants, e.g. with \code{expand.grid()}) the
#' values of the placeholders of a vector control intervention, row by row:
#' \itemize{
#'   \item \strong{efficacies} (\code{<name>_deterrency}, ...), when the
#'     intervention is a list of several nets: those of the net named in
#'     column \code{net};
#'   \item \strong{decay parameters} (\code{<name>_deterrency_L} in years,
#'     \code{<name>_deterrency_k}, ...), when the intervention was defined
#'     with \code{decay = "placeholder"};
#'   \item \strong{dummy-host availabilities} (\code{<name>_dummy_<k>_pop<j>})
#'     of dummy-host nets, when the net or the coverages vary: coverage of
#'     campaign \eqn{k} \eqn{\times} \code{HostSeekingKilling} / human
#'     availability of population \eqn{j}, the coverages taken from the
#'     columns named like the coverage placeholders of the schedule (e.g.
#'     \code{cov2021}); and the decays of the dummy hosts
#'     (\code{<name>_attrition_L}, \code{<name>_hsk_L}, ...) with
#'     \code{decay = "placeholder"}.
#' }
#' Only the placeholders written by \code{\link{define_OM_vector_control}} /
#' \code{\link{deploy_OM_vector_control}} (or \code{\link{get_OM_xml}}) are
#' filled: the intervention's record (nets, placeholders, populations,
#' schedule) is read from \code{source}. The other columns and the row order
#' are kept; the coverages and the scenario design stay the user's.
#'
#' \strong{Decays.} The decays differ between effects through the
#' insecticide decay of each effect (from the net's parameterisation), while
#' the attrition is common to all effects. With \code{combine = TRUE}
#' (default), \code{halflife} and \code{kappa} name \strong{one attrition
#' column} each (e.g. a usage-adjusted attrition): each effect's decay is this
#' attrition combined with the effect's own insecticide decay (fitted by a
#' Weibull when needed, over \code{fit_years} of \code{\link{om_vector_control}}),
#' and the dummy hosts get this attrition as it is (their attrition and
#' insecticide decays are separate). With
#' \code{combine = FALSE}, the columns are \strong{final} decays, written as
#' they are: one column per effect (named vector, e.g.
#' \code{c(Deterrency = "hl_det", PrePrandial = "hl_pre", PostPrandial =
#' "hl_post", HostSeekingKilling = "hl_hsk")}) or one column for all
#' effects; the final decay of host-seeking killing is written as the dummy
#' hosts' attrition (in its decay function), their insecticide decay then
#' being neutral. Without columns, each net's own decay is used. Decay
#' columns need an intervention defined with \code{decay = "placeholder"}.
#'
#' Half-life and shape are replaced \strong{independently}: a \code{kappa}
#' column alone changes the shape and keeps the half-life (and the other way
#' round), the rest coming from the net (its attrition with \code{combine =
#' TRUE}; its exported decay of the effect with \code{combine = FALSE}).
#' Values must be positive numbers (the shape only where the decay function
#' has one); errors name the rows and the effect.
#'
#' Values are computed once per distinct net and decay, so large tables are
#' fast.
#'
#' @param scenarios Data frame of scenarios (one row per scenario).
#' @param source The \code{baseList} after \code{define_OM_vector_control()}
#'   and \code{deploy_OM_vector_control()}, or the result of
#'   \code{get_OM_xml()}.
#' @param name Name of the intervention (optional with \code{get_OM_xml()}).
#' @param net Column naming the net of each row (the names of the list given
#'   to \code{define_OM_vector_control()}); needed when the list has several
#'   nets.
#' @param halflife,kappa Columns of decay parameters (see Decays): half-life
#'   (or time to zero, for linear / step decays) and shape, each optional.
#'   Interventions specified with \code{om_vector_control()} are needed for
#'   \code{combine = TRUE}.
#' @param decay_unit Unit of the \code{halflife} columns: \code{"years"}
#'   (default, as OpenMalaria) or \code{"days"}. Written values are in years.
#' @param combine Logical (default \code{TRUE}): \code{halflife} / \code{kappa}
#'   are an attrition combined with each effect's insecticide decay;
#'   \code{FALSE}: final decays.
#'
#' @return \code{scenarios} with one column per placeholder, named without
#' the \code{@}.
#'
#' @seealso \code{\link{define_OM_vector_control}}, \code{\link{om_campaigns}}
#'
#' @examples
#' \dontrun{
#' nets = list(martin  = om_vector_control(get_ITN_params("Interceptor G2", "Martin"),
#'                                         halflife_attrition = 2 * 365, shape_attrition = 2),
#'             assenga = om_vector_control(get_ITN_params("Interceptor G2", "Assenga"),
#'                                         halflife_attrition = 2 * 365, shape_attrition = 2))
#' baseList = define_OM_vector_control(baseList, nets, name = "futITN",
#'                                     decay = "placeholder")
#' baseList = deploy_OM_vector_control(baseList, "futITN",
#'     om_campaigns("2019-01-25", "2030-01-25", coverage = "@futITNcov@"))
#' scens = expand.grid(net = c("martin", "assenga"), halflife = c(1, 2, 3),
#'                     futITNcov = c(0.5, 0.8))
#' scens = om_scenario_values(scens, baseList, "futITN", net = "net",
#'                            halflife = "halflife")
#' }
#'
#' @export
#'
om_scenario_values = function(scenarios, source, name = NULL, net = NULL,
                              halflife = NULL, kappa = NULL,
                              decay_unit = c("years", "days"), combine = TRUE) {
    decay_unit = match.arg(decay_unit)
    if (!is.data.frame(scenarios) || nrow(scenarios) == 0L)
        stop("scenarios must be a non-empty data frame")
    rec = om_scenario_record(source, name)
    n   = nrow(scenarios)
    lab = function(comp) paste0(rec$pn, "_", om_ph_label[[comp]])
    col_of = function(x, label) {
        if (is.null(x)) return(NULL)
        missing = setdiff(x, names(scenarios))
        if (length(missing))
            stop(label, ": no column ", paste(missing, collapse = ", "), " in scenarios")
        x
    }
    halflife = col_of(halflife, "halflife")
    kappa    = col_of(kappa, "kappa")
    to_days  = if (decay_unit == "years") 365 else 1

    # Which placeholders were written
    sch   = rec$sch
    dummy = rec$mode == "dummy_host"
    if (dummy && is.null(sch) && (rec$several || rec$decay))
        stop("'", rec$name, "' has not been deployed: its dummy hosts (host-seeking ",
             "killing) are written with the schedule (deploy_OM_vector_control())")
    avail = dummy && (rec$several || (!is.null(sch) && any(sch$placeholder)))
    if (!rec$several && !rec$decay && !avail) {
        message("om_scenario_values: '", rec$name, "' has no placeholders to fill")
        return(scenarios)
    }
    if (!rec$decay && (!is.null(halflife) || !is.null(kappa)))
        stop("'", rec$name, "' was defined with fixed decays: decay columns need ",
             "decay = \"placeholder\" in define_OM_vector_control() / get_OM_xml()")

    # Net of each row
    idx = rep(1L, n)
    if (rec$several) {
        if (is.null(net) || is.null(scenarios[[net]]))
            stop("net: give the column naming the net of each row (one of ",
                 paste(rec$members, collapse = ", "), ")")
        idx = match(as.character(scenarios[[net]]), rec$members)
        if (anyNA(idx))
            stop("Unknown net(s) in column ", net, ": ", paste(unique(
                as.character(scenarios[[net]])[is.na(idx)]), collapse = ", "),
                " (nets: ", paste(rec$members, collapse = ", "), ")")
    }

    # Decay columns per effect (combine = FALSE: per effect or one for all;
    # combine = TRUE: one attrition column)
    effects = c(rec$comps, if (rec$mode == "dummy_host") "HostSeekingKilling")
    per_effect = function(x) {
        if (is.null(x)) return(stats::setNames(rep(list(NULL), length(effects)), effects))
        if (is.null(names(x))) {
            if (length(x) != 1L) stop("Give one column, or a named vector (one per effect)")
            return(stats::setNames(rep(list(x), length(effects)), effects))
        }
        bad = setdiff(names(x), effects)
        if (length(bad)) stop("Unknown effect(s) ", paste(bad, collapse = ", "),
                              "; effects: ", paste(effects, collapse = ", "))
        lapply(stats::setNames(effects, effects), function(e) if (e %in% names(x)) x[[e]])
    }
    if (combine && (length(halflife) > 1L || length(kappa) > 1L))
        stop("With combine = TRUE, halflife and kappa are one attrition column each ",
             "(common to all effects); per-effect decays need combine = FALSE")
    if (combine && (!is.null(halflife) || !is.null(kappa)) &&
        !all(vapply(rec$items, om_is_spec, logical(1))))
        stop("combine = TRUE rebuilds the nets with the attrition of each row: ",
             "specify them with om_vector_control()")
    hl_cols = if (combine) NULL else per_effect(halflife)
    k_cols  = if (combine) NULL else per_effect(kappa)
    num = function(col) if (is.null(col)) rep(NA_real_, n) else as.numeric(scenarios[[col]])
    # Decay values, read once: attrition (combine) or final decays per effect
    hl_att = if (combine) num(halflife) else NULL
    k_att  = if (combine) num(kappa) else NULL
    # Attrition overrides (half-life and shape independently): positive numbers
    positive = function(v, col) {
        bad = which(!is.finite(v) | v <= 0)
        if (length(bad))
            stop("Column ", col, ": rows ", paste(utils::head(bad, 10), collapse = ", "),
                 " must be positive numbers")
    }
    if (combine && !is.null(halflife)) positive(hl_att, halflife)
    if (combine && !is.null(kappa))    positive(k_att, kappa)
    hl_eff = lapply(hl_cols, num); k_eff = lapply(k_cols, num)

    # Distinct (net, decay) combinations
    key_parts = list(idx)
    if (combine) key_parts = c(key_parts, list(hl_att, k_att))
    else for (e in effects) key_parts = c(key_parts, list(hl_eff[[e]], k_eff[[e]]))
    key  = do.call(paste, c(key_parts, sep = "|"))
    keys = unique(key)
    groups = split(seq_len(n), factor(key, levels = keys))

    # Coverages of the campaigns, per row
    if (avail) {
        cov = matrix(sch$num, n, length(sch$num), byrow = TRUE)
        for (k in which(sch$placeholder)) {
            cn = sch$names[k]
            if (is.null(scenarios[[cn]]))
                stop("No coverage column '", cn, "' (placeholder @", cn, "@ of the schedule)")
            check_coverage(as.numeric(scenarios[[cn]]), paste0("scenarios$", cn))
            cov[, k] = as.numeric(scenarios[[cn]])
        }
    }

    # Final decay of an effect (combine = FALSE): the intervention's own
    # parameters `base` (L in years, k), with only the supplied ones replaced,
    # validated for the exported decay function fn
    final_decay = function(base, comp, fn, r1, rows) {
        if (!is.null(hl_cols[[comp]])) base[["L"]] = hl_eff[[comp]][r1] * to_days / 365
        if (!is.null(k_cols[[comp]]))  base[["k"]] = k_eff[[comp]][r1]
        where = paste0("om_scenario_values, rows ", paste(utils::head(rows, 10), collapse = ", "),
                       ", ", comp, ": ")
        if (fn != "constant" && !(is.finite(base[["L"]]) && base[["L"]] > 0))
            stop(where, "the half-life (time to zero for linear / step / ",
                 "smooth-compact) of the ", fn, " decay must be a positive number")
        if (fn %in% om_k_fns && !(is.finite(base[["k"]]) && base[["k"]] > 0))
            stop(where, "the shape (kappa) of the ", fn, " decay must be a positive number")
        base
    }

    out = list()
    put = function(col, rows, v) {
        if (is.null(out[[col]])) out[[col]] <<- rep(NA_real_, n)
        out[[col]][rows] <<- v
    }
    for (g in seq_along(keys)) {
        rows = groups[[g]]
        r1   = rows[1]
        item = rec$items[[idx[r1]]]
        # The net with the row's attrition (combine = TRUE): half-life and
        # shape replaced independently, the other kept
        if (combine) {
            if (!is.null(halflife)) item$halflife_attrition = hl_att[r1] * to_days
            if (!is.null(kappa))    item$shape_attrition    = k_att[r1]
        }
        gvi_net = if (om_is_spec(item))
            om_build_spec(item, rec$model_p, om_fit_years(item, rec$intervention_type))
            else item

        # Efficacies
        if (rec$several) {
            eff = gvi_net$settings$efficacy
            for (comp in rec$comps)
                put(lab(comp), rows, if (comp %in% names(eff)) unname(eff[[comp]]) else 0)
        }
        # Decay parameters
        if (rec$decay) for (comp in rec$comps) {
            ph = om_template_decay(rec$fns[[comp]], lab(comp))
            w  = om_template_decay_values(gvi_net, comp, rec$fns[[comp]])
            if (!combine) w = final_decay(w, comp, rec$fns[[comp]], r1, rows)
            if (!is.null(ph$L)) put(paste0(lab(comp), "_L"), rows, unname(w["L"]))
            if (!is.null(ph$k)) put(paste0(lab(comp), "_k"), rows, unname(w["k"]))
        }
        # Dummy hosts: decays (attrition and insecticide decay of host-seeking
        # killing, separate and exact) and availabilities
        if (dummy && rec$decay) {
            f = om_dummy_factors(item)              # with the row's attrition
            vals = list(att = om_dummy_values(f$att), ins = om_dummy_values(f$ins))
            if (!combine && (!is.null(hl_cols$HostSeekingKilling) ||
                             !is.null(k_cols$HostSeekingKilling))) {
                # Final decay: written as the attrition (or as the insecticide
                # decay without attrition), starting from the net's parameters
                # of that factor, the other factor neutral
                w = if (rec$dummy$fns[["att"]] != "constant") "att"
                    else if (rec$dummy$fns[["ins"]] != "constant") "ins"
                    else stop("'", rec$name, "' has no host-seeking killing decay ",
                              "to write the final decay into")
                fin = final_decay(vals[[w]][c("L", "k")], "HostSeekingKilling",
                                  rec$dummy$fns[[w]], r1, rows)
                neutral = om_dummy_values(NULL)
                vals = list(att = neutral, ins = neutral)
                vals[[w]] = c(fin, initial = 1)
            }
            for (w in c("att", "ins")) {
                fn = rec$dummy$fns[[w]]
                if (fn == "constant") next
                l = paste0(rec$pn, "_", om_dummy_label[[w]])
                put(paste0(l, "_L"), rows, unname(vals[[w]]["L"]))
                if (fn %in% om_k_fns) put(paste0(l, "_k"), rows, unname(vals[[w]]["k"]))
                if (w == "ins" && fn == "linear")
                    put(paste0(l, "_initial"), rows, unname(vals[[w]]["initial"]))
            }
        }
        if (avail) {
            hsk = om_hsk_value(item)
            for (k in seq_along(sch$dates)) for (j in which(rec$active))
                put(paste0(rec$pn, "_dummy_", k, "_pop", j), rows,
                    cov[rows, k] * hsk / rec$dummy$alpha_h[j])
        }
    }
    for (col in names(out)) scenarios[[col]] = out[[col]]
    scenarios
}

# Internal: the record of an intervention, from a baseList or get_OM_xml()
om_scenario_record = function(source, name) {
    if (inherits(source, "om_xml")) {
        rec = attr(source, "om_vc")
        if (is.null(rec)) stop("This om_xml has no intervention (only entomology)")
        if (!is.null(name) && name != rec$name)
            stop("This om_xml is for '", rec$name, "', not '", name, "'")
        return(rec)
    }
    if (is.null(name)) stop("name is needed with a baseList")
    rec = source$anophelesmodel2$vc[[name]]
    if (is.null(rec))
        stop("No intervention '", name, "' defined with define_OM_vector_control() ",
             "in this baseList")
    rec
}

#########get_OM_xml####
#' @title OpenMalaria XML snippets, as text
#'
#' @description Writes the entomology and the vector control of an
#' OpenMalaria scenario (schema 44, or \code{schema}) as XML text, to paste into an existing
#' scenario, without openMalariaUtilities. The vector control snippets are
#' those that \code{\link{define_OM_vector_control}} and
#' \code{\link{deploy_OM_vector_control}} write into a \code{baseList}
#' (same lists, same checks and messages):
#' \itemize{
#'   \item \code{entomology}: the \code{<vector>} element, with one
#'     \code{<anopheles>} per mosquito population of \code{om_ento} and the
#'     declaration of the non-human hosts, to paste inside
#'     \code{<entomology>} (replacing its \code{<vector>}). Same structure as
#'     \code{openMalariaUtilities::defineEntomology()}, but with the animal
#'     host parameters of \code{om_ento} (\code{defineEntomology()} writes
#'     fixed values); the placeholders of \code{om_ento$seasonalityParameters}
#'     are kept (contributions \code{"@contrib_<population>@"} and monthly
#'     \code{"@m1@"}, ..., \code{"@m12@"}). The \code{annualEIR} values are
#'     relative contributions: add \code{scaledAnnualEIR} (the total EIR) to
#'     \code{<entomology>};
#'   \item \code{define}: the \code{<component>} elements of the intervention,
#'     to paste inside \code{<interventions><human>};
#'   \item \code{deploy}: its \code{<deployment>} (all components, so that all
#'     effects reach the same people, and one \code{<deploy>} per campaign),
#'     to paste inside \code{<interventions><human>} after all
#'     \code{<component>} elements (OpenMalaria requires the components
#'     first);
#'   \item \code{add_hosts} and \code{modify_hosts}: dummy-host interventions
#'     only (schema >= 46), the host-seeking killing as one dummy host per
#'     campaign (\code{<nonHumanHosts>} elements, with their deployment
#'     dates, to paste inside \code{<interventions><addNonHumanHosts>}) and
#'     the decays of their availability (\code{<intervention>} elements, to
#'     paste inside \code{<interventions><nonHumanHostsModifications>}); see
#'     \code{\link{define_OM_vector_control}}.
#' }
#' Placeholders (coverages, and the values that vary, as in
#' \code{define_OM_vector_control()}) are kept in the text;
#' \code{om_scenario_values(scenarios, xml)} fills them in a scenario table
#' (the result keeps the record of the intervention and its schedule).
#' Printing the result shows each snippet with a comment saying where to
#' paste it. For several interventions, call the function once per
#' intervention (with \code{entomology = FALSE} after the first).
#'
#' @inheritParams define_OM_vector_control
#' @inheritParams deploy_OM_vector_control
#' @param om_ento Output of \code{\link{get_OM_ento_snippet}}: written as the
#'   entomology snippet, and used for the mosquito populations of the
#'   interventions.
#' @param interv Intervention from \code{\link{om_vector_control}} (or an
#'   object from \code{\link{build_intervention}}), or a named list of them
#'   (template, see \code{\link{define_OM_vector_control}}). \code{NULL}
#'   (default): entomology only.
#' @param schedule,end_date As in \code{\link{deploy_OM_vector_control}}:
#'   \code{schedule} is needed with \code{interv}.
#' @param entomology Logical (default \code{TRUE}): include the entomology
#'   snippet.
#' @param schema OpenMalaria schema version of the scenario (default 44).
#'   Dummy-host interventions need 46 or later (developed for 49); from 49,
#'   the entomology has no \code{propInfected} / \code{propInfectious}.
#'
#' @return Object of class \code{om_xml}: named list with \code{entomology},
#' \code{define}, \code{deploy}, \code{add_hosts} and \code{modify_hosts},
#' each a single string (XML text) or \code{NULL}.
#'
#' @seealso \code{\link{define_OM_vector_control}},
#'   \code{\link{om_scenario_values}}
#'
#' @examples
#' itn = om_vector_control(
#'     c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
#'     halflife_attrition = 2 * 365, shape_attrition = 2, name = "ITN")
#' om_ento = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
#'                               activity = def_activity_patterns())
#' schedule = data.frame(date = c("2021-01-01", "2024-01-01"),
#'                       coverage = c("@cov2021@", "@cov2024@"))
#' xml = get_OM_xml(om_ento, itn, schedule)
#' xml
#' \dontrun{
#' writeLines(unlist(xml), "ITN_snippets.xml")
#' }
#'
#' @export
#'
get_OM_xml = function(om_ento, interv = NULL, schedule = NULL, end_date = NULL,
                      intervention_type = c("ITN", "IRS"), name = NULL,
                      subpop = NULL, cumulative = FALSE, minAge = NULL,
                      maxAge = NULL, entomology = TRUE,
                      decay = c("fixed", "placeholder"), schema = 44) {
    intervention_type = match.arg(intervention_type)
    decay  = match.arg(decay)
    schema = om_check_schema(schema)
    to_text = function(tag, items)
        paste(unlist(lapply(items, function(it) om_list_to_xml(tag, it))),
              collapse = "\n")
    out = list(entomology = if (isTRUE(entomology))
                                to_text("vector", list(om_vector_list(om_ento, schema))),
               define = NULL, deploy = NULL, add_hosts = NULL, modify_hosts = NULL)
    # annualEIR are relative contributions: the total goes to scaledAnnualEIR
    relative_eir = isTRUE(entomology)
    if (is.null(interv)) {
        if (!isTRUE(entomology)) stop("Nothing to write: give interv or entomology = TRUE")
        return(structure(out, class = "om_xml", relative_eir = relative_eir))
    }
    if (is.null(schedule)) stop("schedule is needed with interv")

    b = om_vc_components(interv, om_ento, intervention_type, name, decay, schema)
    dummy = b$mode == "dummy_host"
    if (dummy) om_check_hsk_deploy(subpop, cumulative, minAge, maxAge)
    ids = vapply(b$components, `[[`, character(1), "id")
    d   = om_build_deployment(b$name, ids, schedule, end_date, subpop, cumulative,
                              minAge, maxAge, dummy)
    sch = d$sch
    out$define = to_text("component", b$components)
    out$deploy = to_text("deployment", list(d$deployment))
    if (dummy) {
        h = om_dummy_hosts(b$rec, sch)
        out$add_hosts = to_text("nonHumanHosts", h$add)
        if (length(h$modify)) out$modify_hosts = to_text("intervention", h$modify)
        om_dummy_messages(h, "get_OM_xml")
    }
    # Record of the intervention and its schedule, for om_scenario_values()
    rec = b$rec
    rec$sch = sch
    structure(out, class = "om_xml", relative_eir = relative_eir, om_vc = rec)
}


# Internal: <vector> element of the entomology (OpenMalaria schema 44, or
# schema >= 49 without propInfected / propInfectious, removed in 49), as a
# list in the XML structure of openMalariaUtilities: one <anopheles> per
# mosquito population of get_OM_ento_snippet(), then the declaration of the
# non-human hosts. Same structure and conventions as
# openMalariaUtilities::defineEntomology() (mean -> value for
# mosqProbOvipositing and mosqHumanBloodIndex, availability "const",
# minInfectedThreshold 0.001), except that the animal host parameters are those
# of hosts_p (defineEntomology() writes fixed values).
om_vector_list = function(om_ento, schema = 44L) {
    mp  = om_ento$mosquitoParameters
    sp  = om_ento$seasonalityParameters
    nhh = NULL
    out = list()
    for (k in names(mp)) {
        m = mp[[k]]
        s = sp[[k]]
        if (is.null(s)) stop("No seasonalityParameters for ", k)
        n_seas = length(s$seasonality)
        if (!n_seas %in% c(12L, 365L))
            stop("seasonality of ", k, " must have 12 (monthly) or 365 (daily) values")
        values = lapply(s$seasonality, function(v) list(v))
        names(values) = rep("value", n_seas)
        seas = list(annualEIR = s$annualEIR, input = "EIR")
        seas[[if (n_seas == 12L) "monthlyValues" else "dailyValues"]] =
            c(list(smoothing = "fourier"), values)

        value_of = function(p) list(value = m[[p]]$value)
        beta_of  = function(p) list(mean = m[[p]]$mean, variance = m[[p]]$variance)
        mosq = list(
            minInfectedThreshold                = "0.001",
            mosqRestDuration                    = value_of("mosqRestDuration"),
            extrinsicIncubationPeriod           = value_of("extrinsicIncubationPeriod"),
            mosqLaidEggsSameDayProportion       = value_of("mosqLaidEggsSameDayProportion"),
            mosqSeekingDuration                 = value_of("mosqSeekingDuration"),
            mosqSurvivalFeedingCycleProbability = value_of("mosqSurvivalFeedingCycleProbability"),
            availability                        = list(distr = "const"),
            mosqProbBiting                      = beta_of("mosqProbBiting"),
            mosqProbFindRestSite                = beta_of("mosqProbFindRestSite"),
            mosqProbResting                     = beta_of("mosqProbResting"),
            mosqProbOvipositing                 = list(value = m$mosqProbOvipositing$mean),
            mosqHumanBloodIndex                 = list(value = m$mosqHumanBloodIndex$mean))

        anoph = list(mosquito = k)
        if (schema < 49L) {
            anoph$propInfected   = s$propInfected
            anoph$propInfectious = s$propInfectious
        }
        anoph$seasonality = seas
        anoph$mosq = mosq
        if (!is.null(m$nonHumanHosts)) {
            anoph$nonHumanHosts = m$nonHumanHosts
            nhh = union(nhh, m$nonHumanHosts$name)
        }
        out = c(out, list(anopheles = anoph))
    }
    # Declaration of the non-human host species (population size not used by
    # OpenMalaria)
    for (h in nhh) out = c(out, list(nonHumanHosts = list(name = h, number = "1")))
    out
}

#' @rdname get_OM_xml
#' @param x Object of class \code{om_xml}.
#' @param ... Not used.
#' @export
print.om_xml = function(x, ...) {
    where = c(
        entomology = "inside <entomology>, replacing its <vector> element",
        define     = paste("inside <interventions><human>, with the other",
                           "<component> elements"),
        deploy     = paste("inside <interventions><human>, after all <component>",
                           "elements (deploys the components above together)"),
        add_hosts  = "inside <interventions><addNonHumanHosts> (schema >= 46)",
        modify_hosts = "inside <interventions><nonHumanHostsModifications>")
    if (isTRUE(attr(x, "relative_eir")))
        where[["entomology"]] = paste0(where[["entomology"]], "; annualEIR are ",
            "relative contributions: set the total EIR with <entomology ... ",
            "scaledAnnualEIR=\"@EIR@\">")
    for (s in names(where)) if (!is.null(x[[s]]))
        cat("<!-- ", s, ": paste ", where[[s]], " -->\n", x[[s]], "\n\n", sep = "")
    invisible(x)
}


# Internal: list in the XML structure of openMalariaUtilities -> XML lines,
# with the conventions of its writer: a named single value is an attribute
# (NA: omitted), an unnamed single value is the element's text, a named list is
# a child element (repeated names: repeated elements).
om_list_to_xml = function(tag, x, indent = 0L) {
    pad = strrep("  ", indent)
    nm  = names(x)
    if (is.null(nm)) nm = rep("", length(x))
    is_child = vapply(x, is.list, logical(1))
    attrs = ""
    text  = NULL
    for (i in which(!is_child)) {
        v = x[[i]]
        if (is.null(v) || length(v) != 1L)
            stop("XML element <", tag, ">: '", nm[i], "' must be a single value")
        if (is.na(v)) next
        v = om_xml_escape(as.character(v))
        if (nm[i] == "") text = v
        else attrs = paste0(attrs, " ", nm[i], "=\"", v, "\"")
    }
    if (any(is_child & nm == ""))
        stop("XML element <", tag, ">: unnamed child element")
    children = unlist(lapply(which(is_child), function(i)
        om_list_to_xml(nm[i], x[[i]], indent + 1L)), use.names = FALSE)
    open = paste0(pad, "<", tag, attrs)
    if (!length(children) && is.null(text)) return(paste0(open, "/>"))
    if (!length(children)) return(paste0(open, ">", text, "</", tag, ">"))
    c(paste0(open, ">"), children, paste0(pad, "</", tag, ">"))
}

om_xml_escape = function(x) {
    x = gsub("&", "&amp;", x, fixed = TRUE)
    x = gsub("<", "&lt;", x, fixed = TRUE)
    x = gsub(">", "&gt;", x, fixed = TRUE)
    gsub("\"", "&quot;", x, fixed = TRUE)
}
