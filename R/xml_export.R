# Export to OpenMalaria (schema 44) via r-openMalariaUtilities (OMU): the
# functions below return the input lists of OMU::defineEntomology() and
# OMU::defineVectorControl(); OMU writes the XML.

# Internal: number -> string, as in the OMU examples
om_num = function(x) as.character(signif(x, 6))

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
#' \code{hosts_p}; note that \code{defineEntomology()} currently writes fixed
#' animal values and ignores this entry.
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
#'     (per day, with the population's HBI), used to export host-seeking
#'     killing (\code{\link{define_OM_vector_control}});
#'   \item \code{model_params}: named list (one element per mosquito
#'     population) of \code{\link{build_model_obj}} outputs with the
#'     population's HBI (\code{NULL} if the bionomics give no valid death rate).
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
        if (!is.null(activity$HBI)) activity = rep(list(activity), length(taxa))
        if (length(activity) != length(taxa))
            stop("activity must be one activity pattern, or a list with one ",
                 "pattern per taxon")
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
            # export; NULL if the bionomics give no valid death rate (only an
            # error when needed, in define_OM_vector_control())
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
# fitting window) or over the n_days of a schedule (host-seeking killing);
# intervention objects are used as they are
om_gvi_objects = function(items, om_ento, intervention_type) {
    lapply(items, function(it) if (om_is_spec(it))
        om_build_spec(it, om_model_for(om_ento), om_fit_years(it, intervention_type))
        else it)
}
om_period_objects = function(items, model_p, n_days) {
    lapply(items, function(it) if (om_is_spec(it))
        om_build_spec(it, model_p, n_days / 365) else it)
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

    # Insecticide decay of this effect
    ins = NULL
    if (isTRUE(s$insecticide_decay)) {
        if (!is.null(s$component_decay[[comp]])) {
            cd  = s$component_decay[[comp]]
            ins = shape_list(cd$decay, cd$L, cd$kappa)
        } else if (!is.null(s$halflife_insecticide) &&
                   comp %in% names(s$halflife_insecticide)) {
            hl = unname(s$halflife_insecticide[[comp]])
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
    if (!is.null(end_date)) {
        end = tryCatch(as.Date(as.character(end_date)), error = function(e) NA)
        if (length(end) != 1L || is.na(end))
            stop("end_date must be a single date (\"YYYY-MM-DD\")")
        if (end <= max(dates)) stop("end_date must be after the last campaign")
        out$end = end
    }
    out
}

# Internal: host-seeking killing of the campaigns of a schedule, as steps.
# g(a) = HostSeekingKilling x insecticide decay x attrition at net age a (days)
# is the dummy-host availability per person covered at deployment. For each
# campaign j (day s_j) and day t, M[t, j] = g(t - s_j) for t >= s_j (nets of
# all campaigns still in use add up). Step k covers [s_k, s_k+1) (the last
# one up to end_date); B[k, j] is the mean of M[, j] over step k, so that the
# step value is B %*% coverage, linear in the coverages.
om_hsk_factors = function(interv, sch) {
    s      = as.numeric(sch$dates - sch$dates[1])
    n_days = as.numeric(sch$end - sch$dates[1])
    if (interv$duration < n_days)
        stop("The intervention lasts ", interv$duration, " days but must cover ",
             "the period from the first campaign to end_date (", n_days,
             " days), for the nets of the first campaign: increase duration_years ",
             "(or use om_vector_control(), built over the period)")
    rows = seq_len(interv$duration)
    g    = interv$effects$alphai[rows, 4] * interv$effects$survival[rows]
    t    = seq_len(n_days) - 1
    M = matrix(vapply(s, function(sj) {
        a = t - sj
        out = numeric(n_days)
        out[a >= 0] = g[a[a >= 0] + 1]
        out
    }, numeric(n_days)), n_days)
    step = findInterval(t, s)
    B = rowsum(M, step, reorder = TRUE) / as.vector(table(step))
    list(B = unname(B), M = M, step = step, L_years = diff(c(s, n_days)) / 365)
}

# Internal: average vectorial capacity reduction from host-seeking killing
# alone, exact curve vs steps (for the define message)
om_hsk_check = function(fac, cov, model_p) {
    h  = model_p$host_params
    n  = nrow(fac$M)
    one = function(x) matrix(x, 2L, n)
    exact = as.vector(fac$M %*% cov)
    steps = as.vector(fac$B %*% cov)[fac$step]
    eff = list(alphai_1 = one(h$alphai[1]), PBi_1 = one(h$PBi[1]),
               PCi_1 = one(h$PCi[1]), PDi_1 = one(h$PDi[1]), PEi_1 = one(h$PEi[1]),
               alphai_4 = rbind(exact, steps, deparse.level = 0),
               surv_mat = one(1), use_dummy = TRUE)
    as.vector(simulate_vc(eff, host_base(h), model_p$vec_params, h, 1)$avg_impact)
}

# Internal: muvA of the populations reached, with checks
om_muvA = function(om_ento, active) {
    muvA = om_ento$shares$muvA
    if (is.null(muvA))
        stop("om_ento has no muvA: rebuild it with get_OM_ento_snippet()")
    bad = active & !(is.finite(muvA) & muvA > 0)
    if (any(bad))
        stop("No valid host-seeking death rate (muvA) for ",
             paste(om_ento$shares$mosquito[bad], collapse = ", "))
    muvA
}


#########om_vector_control####
#' @title Specify a vector control intervention for OpenMalaria
#'
#' @description Specifies an ITN or IRS intervention for the OpenMalaria
#' export (\code{\link{define_OM_vector_control}},
#' \code{\link{deploy_OM_vector_control}}, \code{\link{get_OM_xml}},
#' \code{\link{om_vector_control_values}}) from its efficacies and decays,
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
    baseList$anophelesmodel2$ento = om_ento
    baseList
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
#' IRS) into an openMalariaUtilities \code{baseList} (OpenMalaria schema 44),
#' for both model structures: the components of the generic vector
#' intervention (GVI) of \code{\link{add_OM_GVI}} (deterrency, pre-prandial
#' killing in the standard model, post-prandial killing). Deploy it with
#' \code{\link{deploy_OM_vector_control}} and the same \code{name}.
#'
#' For dummy-host interventions, the host-seeking killing depends on the
#' campaigns (dates and coverages), so it is written by
#' \code{deploy_OM_vector_control()}; \code{define_OM_vector_control()} keeps
#' what it needs in \code{baseList$anophelesmodel2} (not written to the XML)
#' and says so in a message.
#'
#' \strong{Host-seeking killing.} OpenMalaria's GVI has no host-seeking
#' killing. In this package it is a dummy host whose availability \eqn{d(t)},
#' per human, kills the mosquitoes that find it; this is exactly an increase
#' of the host-seeking death rate \code{muvA} by the factor \eqn{1 + d(t) /
#' muvA}, OpenMalaria's vector population intervention
#' \code{seekingDeathRateIncrease} (same death rate formula). Its value is a
#' population average: \eqn{d(t) = \sum_j c_j\, g(t - t_j)}, with \eqn{c_j}
#' the coverage of the campaign at date \eqn{t_j} and \eqn{g} the
#' \code{HostSeekingKilling} \eqn{\times} insecticide decay \eqn{\times}
#' attrition of a net of that age (nets of earlier campaigns still in use are
#' included). As several instances of \code{seekingDeathRateIncrease} multiply
#' and a redeployment of an instance replaces it, \eqn{d(t)} is written as
#' \strong{steps}: one vector population intervention
#' \code{<name>_hsk_<k>} per campaign, deployed at its date with a
#' \code{step} decay lasting until the next campaign (or \code{end_date}), of
#' value the average of \eqn{d(t) / muvA} over that period. The message of
#' \code{deploy_OM_vector_control()} gives the average vectorial capacity
#' reduction from host-seeking killing alone with the exact curve and with the
#' steps (at the numeric coverages, or 0.8 for placeholders). Host-seeking
#' killing acts on the same populations as the GVI (in bed for ITNs).
#' Interventions from \code{\link{om_vector_control}} are built over exactly
#' the period from the first campaign to \code{end_date}; objects from
#' \code{build_intervention()} must last (\code{duration_years}) that long.
#'
#' \strong{Placeholders.} Coverages in the schedule of
#' \code{deploy_OM_vector_control()} can be placeholders
#' (\code{"@cov2021@"}); the step values then depend on them and are written
#' as placeholders \code{"@<name>_hsk_<k>_pop<j>@"} (campaign \eqn{k},
#' mosquito population \eqn{j}: row of \code{om_ento$shares}, printed by the
#' message). With a \strong{named list} of interventions (e.g. several nets
#' or parameterisations, to loop over in scenarios), the structure is shared
#' and every value is a placeholder: efficacies \code{"@<name>_deterrency@"},
#' \code{"@<name>_preprandial@"}, \code{"@<name>_postprandial@"}, Weibull
#' decays \code{"@<name>_deterrency_L@"}, \code{"@<name>_deterrency_k@"}, ...
#' (one GVI component per effect; the exact decay if it is a Weibull,
#' otherwise a fitted Weibull) and the host-seeking killing steps. All
#' interventions of the list must use the same model. Fill the placeholders
#' with \code{\link{om_vector_control_values}}.
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
#'
#' @return The updated \code{baseList}.
#'
#' @seealso \code{\link{deploy_OM_vector_control}},
#'   \code{\link{om_vector_control_values}}, \code{\link{get_ITN_params}}
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
#' baseList = define_OM_entomology(baseList, om_ento, seasonality = rep(1, 12))
#' baseList = define_OM_vector_control(baseList, nets, name = "ITN")
#' schedule = data.frame(date = c("2021-01-01", "2024-01-01", "2027-01-01"),
#'                       coverage = c("@cov2021@", "@cov2024@", "@cov2027@"))
#' baseList = deploy_OM_vector_control(baseList, "ITN", schedule,
#'                                     end_date = "2030-01-01")
#' values = om_vector_control_values(nets, om_ento, schedule, "2030-01-01",
#'     coverages = expand.grid(cov2021 = c(0.5, 0.8), cov2024 = 0.6,
#'                             cov2027 = 0.6), name = "ITN")
#' }
#'
#' @export
#'
define_OM_vector_control = function(baseList, interv,
                                    intervention_type = c("ITN", "IRS"),
                                    name = NULL) {
    intervention_type = match.arg(intervention_type)
    om_ento = om_baselist_ento(baseList)
    b = om_vc_components(interv, om_ento, intervention_type, name)
    ids = vapply(b$components, `[[`, character(1), "id")
    human = baseList$interventions$human
    if (any(ids %in% vapply(human[names(human) == "component"],
                            function(cm) as.character(cm$id), character(1))))
        stop("baseList already has components of '", b$name, "'")

    baseList = om_add_components(baseList, b$components)
    # Dummy host: what the host-seeking killing steps need, for
    # deploy_OM_vector_control(), which writes the steps. Kept in a top-level
    # entry of baseList that openMalariaUtilities does not write to the XML
    # (its writer only writes the scenario sections it knows).
    if (!is.null(b$hsk)) {
        baseList$anophelesmodel2$hsk[[b$name]] = b$hsk
        message("define_OM_vector_control: the host-seeking killing of '", b$name,
                "' will be added by deploy_OM_vector_control(), which needs the ",
                "whole campaign schedule and end_date.")
    }
    baseList
}


# Internal: the <component> lists of an intervention (XML structure of
# openMalariaUtilities, no baseList needed) and, for dummy-host
# interventions, what the host-seeking killing steps need (hsk, for
# om_hsk_steps()). Shared by define_OM_vector_control() and get_OM_xml().
om_vc_components = function(interv, om_ento, intervention_type, name) {
    x     = om_nets(interv)
    name  = om_name(x, name, intervention_type)
    items = x$nets
    x$nets = om_gvi_objects(items, om_ento, intervention_type)
    pn   = om_ph_name(name)
    mode = x$nets[[1]]$settings$mode
    mosq   = om_ento$shares$mosquito
    active = om_active(om_ento, intervention_type)
    out = list(name = name, mode = mode)

    # GVI: values, or placeholders for a list of interventions
    if (x$template) {
        comps = om_model_effects(mode)
        for (net in x$nets) for (comp in comps) om_weibull_decay(net, comp)   # checks
        gvi = lapply(comps, function(comp) {
            lab = paste0(pn, "_", om_ph_label[[comp]])
            params = lapply(seq_along(active), function(j)
                list(propActive = if (active[j]) 1 else 0,
                     value      = if (active[j]) paste0("@", lab, "@") else "0"))
            names(params) = mosq
            list(decay = list(L = paste0("@", lab, "_L@"), "function" = "weibull",
                              k = paste0("@", lab, "_k@")),
                 anophelesParams = params)
        })
        names(gvi) = unname(om_gvi_effects[comps])
        out$components = om_gvi_components(stats::setNames(list(gvi), name), merge = FALSE)
    } else {
        out$components = om_gvi_components(om_gvi_list(x$nets[[1]], om_ento,
                                                       intervention_type, name))
    }
    if (mode != "dummy_host") return(out)

    # Host-seeking killing: computed from the schedule by om_hsk_steps()
    p1 = which(active)[1]
    out$hsk = list(
        name = name, pn = pn, items = items, model_p = om_model_for(om_ento),
        template = x$template,
        labels = if (x$template) paste0(names(interv), ": ") else "",
        mosq = mosq, active = active, muvA = om_muvA(om_ento, active),
        check_model = if (!is.na(p1)) om_ento$model_params[[mosq[p1]]])
    out
}

# Internal: host-seeking killing of a dummy-host intervention (hsk from
# om_vc_components()) for a schedule: one seekingDeathRateIncrease step per
# campaign (vectorPop <intervention> lists, with their deployment dates), and
# what the messages report. Shared by deploy_OM_vector_control() and
# get_OM_xml().
om_hsk_steps = function(hsk, sch) {
    mosq = hsk$mosq; active = hsk$active
    # Specifications are built over exactly the schedule period
    nets = om_period_objects(hsk$items, hsk$model_p,
                             as.numeric(sch$end - sch$dates[1]))
    facs = lapply(nets, om_hsk_factors, sch = sch)
    numeric_values = !hsk$template && !any(sch$placeholder)
    if (numeric_values) d_step = as.vector(facs[[1]]$B %*% sch$num)
    out = list(name = hsk$name, steps = list())
    for (k in seq_along(sch$dates)) {
        anoph = lapply(seq_along(mosq), function(j) list(
            mosquito = mosq[j],
            seekingDeathRateIncrease = list(
                initial = if (!active[j]) "0"
                          else if (numeric_values) om_num(d_step[k] / hsk$muvA[j])
                          else paste0("@", hsk$pn, "_hsk_", k, "_pop", j, "@"),
                decay = list("function" = "step", L = om_num(facs[[1]]$L_years[k])))))
        names(anoph) = rep("anopheles", length(anoph))
        out$steps = c(out$steps, list(intervention = list(
            name = paste0(hsk$name, "_hsk_", k), description = anoph,
            timed = list(deploy = list(time = format(sch$dates[k]))))))
    }

    # Approximation check: VC reduction from host-seeking killing alone
    if (!is.null(hsk$check_model)) {
        cov = ifelse(sch$placeholder, 0.8, sch$num)
        out$check = list(
            mosquito = mosq[which(active)[1]], coverage = cov,
            impact   = vapply(facs, om_hsk_check, numeric(2), cov = cov,
                              model_p = hsk$check_model),
            labels   = hsk$labels)
    }
    if (!numeric_values)
        out$placeholders = paste0("pop", which(active), " = ", mosq[active])
    out
}

# Internal: messages of deploy_OM_vector_control() / get_OM_xml() about the
# host-seeking killing steps
om_vc_messages = function(b, caller) {
    if (!length(b$steps)) return(invisible())
    K = length(b$steps)
    if (!is.null(b$check))
        message(caller, ": host-seeking killing as ", K,
                " seekingDeathRateIncrease steps (vectorPop ", b$name, "_hsk_1..", K,
                ", deployed on the schedule dates).\n",
                "  Average VC reduction from host-seeking killing alone (",
                b$check$mosquito, ", coverages ",
                paste(om_num(b$check$coverage), collapse = ", "), "), exact vs steps: ",
                paste0(b$check$labels, om_num(100 * b$check$impact[1, ]), "% vs ",
                       om_num(100 * b$check$impact[2, ]), "%", collapse = "; "))
    if (!is.null(b$placeholders))
        message("  Placeholders @", om_ph_name(b$name), "_hsk_<k>_pop<j>@ (fill with ",
                "om_vector_control_values()): ",
                paste(b$placeholders, collapse = ", "))
    invisible()
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
#' For dummy-host interventions, it also writes the host-seeking killing:
#' one \code{seekingDeathRateIncrease} step per campaign, whose values depend
#' on the dates and coverages of all campaigns (nets of earlier campaigns
#' still in use are included; see \code{\link{define_OM_vector_control}}).
#' Such an intervention must therefore be deployed \strong{once}, with the
#' whole schedule, and \code{end_date} is required; \code{subpop},
#' \code{cumulative}, \code{minAge} and \code{maxAge} are rejected (not
#' accounted for in the steps). The message compares the vectorial capacity
#' reduction of the steps with that of the exact curve.
#'
#' @param baseList openMalariaUtilities \code{baseList}, after
#'   \code{define_OM_vector_control()}.
#' @param name Name of the intervention given to (or printed by)
#'   \code{define_OM_vector_control()}.
#' @param schedule Data frame with one row per campaign: \code{date}
#'   (\code{"YYYY-MM-DD"}, increasing) and \code{coverage} (numbers in [0, 1]
#'   or placeholders such as \code{"@cov2021@"}).
#' @param end_date End of the simulated period (\code{"YYYY-MM-DD"}), the end of
#'   the last host-seeking killing step; required for dummy-host
#'   interventions (objects from \code{build_intervention()} must last
#'   from the first campaign to \code{end_date}; interventions from
#'   \code{om_vector_control()} are built over that period).
#' @param subpop Optional sub-population id suffix: the deployment is
#'   restricted to the sub-population \code{"<name>-<subpop>"}
#'   (\code{restrictToSubPop}). This and the next arguments: standard model
#'   only.
#' @param cumulative Logical (default \code{FALSE}): cumulative coverage
#'   (\code{cumulativeCoverage}, people already covered are not redeployed),
#'   measured on \code{"<name>-<subpop>"} or the first component.
#' @param minAge,maxAge Optional age limits of the deployment (numbers or
#'   placeholders).
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
    if (!length(ids))
        stop("No component '", name, "' or '", name, "-<effect>' in baseList: ",
             "call define_OM_vector_control() first")

    # Dummy host: the host-seeking killing steps are computed here, from the
    # information kept by define_OM_vector_control() and the whole schedule
    hsk = baseList$anophelesmodel2$hsk[[name]]
    if (!is.null(hsk)) {
        om_check_hsk_deploy(subpop, cumulative, minAge, maxAge)
        if (is.null(end_date))
            stop("end_date is needed to deploy the dummy-host intervention '", name,
                 "': it ends the last host-seeking killing step")
        sch = om_schedule(schedule, end_date)
        vp = baseList$interventions$vectorPop
        existing = vapply(vp[names(vp) == "intervention"],
                          function(v) as.character(v$name), character(1))
        if (any(startsWith(existing, paste0(name, "_hsk_"))))
            stop("'", name, "' is already deployed: deploy a dummy-host ",
                 "intervention once, with the whole campaign schedule (its ",
                 "host-seeking killing includes the nets of earlier campaigns)")
        h = om_hsk_steps(hsk, sch)
    } else {
        sch = om_schedule(schedule, end_date)
    }

    baseList$interventions$human = c(human, list(deployment = om_deployment(
        name, ids, sch, subpop, cumulative, minAge, maxAge)))
    if (!is.null(hsk))
        baseList$interventions$vectorPop = c(baseList$interventions$vectorPop, h$steps)
    message("deploy_OM_vector_control: deployment '", name, "' with components ",
            paste(ids, collapse = ", "), " at ", paste(format(sch$dates), collapse = ", "))
    if (!is.null(hsk)) om_vc_messages(h, "deploy_OM_vector_control")
    baseList
}


# Internal: dummy-host interventions cannot be restricted (the host-seeking
# killing steps assume campaigns covering the whole population)
om_check_hsk_deploy = function(subpop, cumulative, minAge, maxAge) {
    if (!is.null(subpop) || isTRUE(cumulative) || !is.null(minAge) || !is.null(maxAge))
        stop("subpop, cumulative, minAge and maxAge are not supported for ",
             "dummy-host interventions: the host-seeking killing steps assume ",
             "campaigns covering the whole population at the schedule coverages")
}

# Internal: <deployment> list of an intervention: all its components (same
# people), then one deploy per campaign. Shared by deploy_OM_vector_control()
# and get_OM_xml().
om_deployment = function(name, ids, sch, subpop = NULL, cumulative = FALSE,
                         minAge = NULL, maxAge = NULL) {
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


#########om_vector_control_values####
#' @title Values of the placeholders of a vector control intervention
#'
#' @description Computes the values of the placeholders written by
#' \code{\link{define_OM_vector_control}}, to fill an OpenMalaria scenario
#' table: one row per intervention (for a named list) and coverage scenario.
#'
#' @inheritParams define_OM_vector_control
#' @inheritParams deploy_OM_vector_control
#' @param om_ento Output of \code{\link{get_OM_ento_snippet}} (the one given to
#'   \code{define_OM_entomology()}).
#' @param coverages Data frame with one column per coverage placeholder of
#'   \code{schedule}, named without the \code{@} (e.g. \code{cov2021}), and one
#'   row per coverage scenario. Not needed when all coverages are numeric.
#'
#' @return Data frame with \code{net} (the names of a list of interventions),
#' the coverage columns, and one column per placeholder (named without the
#' \code{@}): efficacies and Weibull decays (\code{L} in years, \code{k}) for a
#' list of interventions, and host-seeking killing steps
#' \code{<name>_hsk_<k>_pop<j>} for dummy-host interventions.
#'
#' @seealso \code{\link{define_OM_vector_control}}
#'
#' @export
#'
om_vector_control_values = function(interv, om_ento, schedule, end_date,
                                    coverages = NULL,
                                    intervention_type = c("ITN", "IRS"),
                                    name = NULL) {
    intervention_type = match.arg(intervention_type)
    x    = om_nets(interv)
    sch  = om_schedule(schedule, end_date)
    name = om_name(x, name, intervention_type)
    pn   = om_ph_name(name)
    # As define / deploy: GVI decays over the fitting window, host-seeking
    # killing over the schedule period
    gvi_nets = om_gvi_objects(x$nets, om_ento, intervention_type)
    hsk_nets = om_period_objects(x$nets, om_model_for(om_ento),
                                 as.numeric(sch$end - sch$dates[1]))
    mode = gvi_nets[[1]]$settings$mode
    active = om_active(om_ento, intervention_type)

    cov_names = sch$names[sch$placeholder]
    if (length(cov_names)) {
        if (is.null(coverages))
            stop("coverages is needed for the coverage placeholders: ",
                 paste(cov_names, collapse = ", "))
        coverages = as.data.frame(coverages)
        missing = setdiff(cov_names, names(coverages))
        if (length(missing))
            stop("coverages has no column for: ", paste(missing, collapse = ", "))
        cov_tab = coverages[, cov_names, drop = FALSE]
        if (nrow(cov_tab) == 0L) stop("coverages must have at least one row")
        for (cn in cov_names) check_coverage(cov_tab[[cn]], paste0("coverages$", cn))
    } else {
        cov_tab = data.frame(row.names = 1L)
    }
    n_sc = nrow(cov_tab)
    cov_mat = matrix(sch$num, n_sc, length(sch$num), byrow = TRUE)
    cov_mat[, sch$placeholder] = as.matrix(cov_tab[, cov_names, drop = FALSE])
    hsk = mode == "dummy_host" && (x$template || length(cov_names))
    if (hsk) muvA = om_muvA(om_ento, active)

    rows = lapply(seq_along(x$nets), function(i) {
        net = gvi_nets[[i]]
        out = cov_tab
        if (x$template) {
            for (comp in om_model_effects(mode)) {
                lab = paste0(pn, "_", om_ph_label[[comp]])
                eff = net$settings$efficacy
                w   = om_weibull_decay(net, comp)
                out[[lab]] = if (comp %in% names(eff)) unname(eff[[comp]]) else 0
                out[[paste0(lab, "_L")]] = unname(w["L"])
                out[[paste0(lab, "_k")]] = unname(w["k"])
            }
        }
        if (hsk) {
            d_step = cov_mat %*% t(om_hsk_factors(hsk_nets[[i]], sch)$B)   # scenarios x steps
            for (k in seq_len(ncol(d_step))) for (j in which(active))
                out[[paste0(pn, "_hsk_", k, "_pop", j)]] = d_step[, k] / muvA[j]
        }
        if (x$template)
            out = data.frame(net = rep(names(interv)[i], nrow(out)), out,
                             check.names = FALSE, stringsAsFactors = FALSE)
        out
    })
    out = do.call(rbind, rows)
    rownames(out) = NULL
    if (ncol(out) == 0L)
        message("om_vector_control_values: no placeholders to fill")
    out
}


#########get_OM_xml####
#' @title OpenMalaria XML snippets, as text
#'
#' @description Writes the entomology and the vector control of an
#' OpenMalaria scenario (schema 44) as XML text, to paste into an existing
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
#'   \item \code{hsk}: dummy-host interventions only, the host-seeking
#'     killing steps (vector population \code{<intervention>} elements, with
#'     their deployment dates), to paste inside
#'     \code{<interventions><vectorPop>}.
#' }
#' Placeholders (coverages, values of a list of interventions) are kept in
#' the text; \code{\link{om_vector_control_values}} gives their values.
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
#'   \code{schedule} is needed with \code{interv}, \code{end_date} for
#'   dummy-host interventions.
#' @param entomology Logical (default \code{TRUE}): include the entomology
#'   snippet.
#'
#' @return Object of class \code{om_xml}: named list with \code{entomology},
#' \code{define}, \code{deploy} and \code{hsk}, each a single string (XML
#' text) or \code{NULL}.
#'
#' @seealso \code{\link{define_OM_vector_control}},
#'   \code{\link{om_vector_control_values}}
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
                      maxAge = NULL, entomology = TRUE) {
    intervention_type = match.arg(intervention_type)
    to_text = function(tag, items)
        paste(unlist(lapply(items, function(it) om_list_to_xml(tag, it))),
              collapse = "\n")
    out = list(entomology = if (isTRUE(entomology))
                                to_text("vector", list(om_vector_list(om_ento))),
               define = NULL, deploy = NULL, hsk = NULL)
    # annualEIR are relative contributions: the total goes to scaledAnnualEIR
    relative_eir = isTRUE(entomology)
    if (is.null(interv)) {
        if (!isTRUE(entomology)) stop("Nothing to write: give interv or entomology = TRUE")
        return(structure(out, class = "om_xml", relative_eir = relative_eir))
    }
    if (is.null(schedule)) stop("schedule is needed with interv")

    b = om_vc_components(interv, om_ento, intervention_type, name)
    if (!is.null(b$hsk)) {
        om_check_hsk_deploy(subpop, cumulative, minAge, maxAge)
        if (is.null(end_date))
            stop("end_date is needed for dummy-host interventions: it ends the ",
                 "last host-seeking killing step")
    }
    sch = om_schedule(schedule, end_date)
    ids = vapply(b$components, `[[`, character(1), "id")
    out$define = to_text("component", b$components)
    out$deploy = to_text("deployment", list(om_deployment(
        b$name, ids, sch, subpop, cumulative, minAge, maxAge)))
    if (!is.null(b$hsk)) {
        h = om_hsk_steps(b$hsk, sch)
        out$hsk = to_text("intervention", h$steps)
        om_vc_messages(h, "get_OM_xml")
    }
    structure(out, class = "om_xml", relative_eir = relative_eir)
}


# Internal: <vector> element of the entomology (OpenMalaria schema 44), as a
# list in the XML structure of openMalariaUtilities: one <anopheles> per
# mosquito population of get_OM_ento_snippet(), then the declaration of the
# non-human hosts. Same structure and conventions as
# openMalariaUtilities::defineEntomology() (mean -> value for
# mosqProbOvipositing and mosqHumanBloodIndex, availability "const",
# minInfectedThreshold 0.001), except that the animal host parameters are those
# of hosts_p (defineEntomology() writes fixed values).
om_vector_list = function(om_ento) {
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

        anoph = list(mosquito = k, propInfected = s$propInfected,
                     propInfectious = s$propInfectious,
                     seasonality = seas, mosq = mosq)
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
        hsk        = "inside <interventions><vectorPop>")
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
