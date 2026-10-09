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
#'
#' @return List with
#' \itemize{
#'   \item \code{mosquitoParameters}: named list (one element per mosquito
#'     population) for \code{defineEntomology(mosquitoParameters = )};
#'   \item \code{seasonalityParameters}: named list skeleton for
#'     \code{defineEntomology(seasonalityParameters = )}, with placeholders
#'     \code{"@EIR_<name>@"} for \code{annualEIR} and \code{"@m1@"},
#'     ..., \code{"@m12@"} for the seasonality, to be replaced (with R >= 4.3,
#'     \code{defineEntomology()} currently rejects monthly placeholders;
#'     replace them with numeric values, or a fixed openMalariaUtilities);
#'   \item \code{shares}: data frame with \code{mosquito}, \code{taxon},
#'     \code{type} (\code{indoor1}, \code{indoor2}, \code{outdoor} or
#'     \code{all}), \code{share} within the taxon, and the logical columns
#'     \code{in_bed} and \code{indoor_resting} (exposure to interventions
#'     acting in bed, e.g. ITNs, or on indoor resting mosquitoes, e.g. IRS).
#' }
#'
#' @seealso \code{\link{get_OM_GVI_snippet}}
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
                               bionomics_table = NULL) {
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
        for (j in seq_len(nrow(sh)))
            mosquitoParameters[[sh$mosquito[j]]] = om_mosq_params(vec_p, chi[j], hosts_p)
        shares[[i]] = sh
    }
    shares = do.call(rbind, shares)
    rownames(shares) = NULL
    if (anyDuplicated(shares$mosquito)) stop("Duplicated taxa in 'taxa'")

    seasonalityParameters = lapply(shares$mosquito, function(m) list(
        annualEIR      = paste0("@EIR_", gsub("[^A-Za-z0-9]+", "_", m), "@"),
        seasonality    = paste0("@m", 1:12, "@"),
        propInfected   = propInfected,
        propInfectious = propInfectious))
    names(seasonalityParameters) = shares$mosquito

    list(mosquitoParameters    = mosquitoParameters,
         seasonalityParameters = seasonalityParameters,
         shares                = shares)
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
#' Dummy-host interventions (\code{HostSeekingKilling}) cannot be exported yet.
#'
#' @param interv Intervention object from \code{\link{build_intervention}}.
#' @param om_ento Output of \code{\link{get_OM_ento_snippet}}.
#' @param intervention_type \code{"ITN"} (acts in bed) or \code{"IRS"} (acts on
#'   indoor resting mosquitoes).
#' @param name Name of the intervention (default: the intervention
#'   description).
#'
#' @return Named list (one element, \code{name}) with \code{deterrency},
#' \code{preprandialKillingEffect} and \code{postprandialKillingEffect}, each
#' with \code{decay} and per-mosquito \code{anophelesParams}
#' (\code{propActive}, \code{value}), for
#' \code{defineVectorControl(vectorInterventionParameters = )}.
#'
#' @seealso \code{\link{get_OM_ento_snippet}}, \code{\link{build_intervention}}
#'
#' @examples
#' model_p = build_model_obj(def_vector_params(), def_host_params())
#' itn = build_intervention(
#'     efficacy = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
#'     halflife_attrition = 2 * 365, shape_attrition = 2,
#'     duration_years = 3, model_p = model_p, name = "ITN")
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
    if (is.null(interv$settings) && length(interv) == 1L) interv = interv[[1]]
    s = interv$settings
    if (is.null(s))
        stop("interv must be an intervention object from build_intervention()")
    if (s$mode == "dummy_host")
        stop("Dummy-host interventions (HostSeekingKilling) cannot be exported ",
             "to OpenMalaria yet; use the standard parameterisation ",
             "(e.g. get_ITN_params(mode = \"standard\")).")

    attr_none = om_decay_name(s$decay_attrition) == "constant"
    if (intervention_type == "IRS" && !attr_none)
        warning("IRS with attrition decay '", s$decay_attrition,
                "': IRS usually has no attrition (decay_attrition = \"none\").")
    if (intervention_type == "ITN" && attr_none)
        warning("ITN without attrition decay (decay_attrition = \"none\").")

    effects = c(Deterrency   = "deterrency",
                PrePrandial  = "preprandialKillingEffect",
                PostPrandial = "postprandialKillingEffect")
    active = if (intervention_type == "ITN") om_ento$shares$in_bed
             else om_ento$shares$indoor_resting

    out = lapply(names(effects), function(comp) {
        value = if (comp %in% names(s$efficacy)) unname(s$efficacy[[comp]]) else 0
        params = lapply(seq_along(active), function(j)
            list(propActive = if (active[j]) 1 else 0,
                 value      = om_num(if (active[j]) value else 0)))
        names(params) = om_ento$shares$mosquito
        list(decay = om_gvi_decay(interv, comp), anophelesParams = params)
    })
    names(out) = unname(effects)

    if (is.null(name))
        name = if (!is.null(interv$description) && interv$description != "custom")
                   interv$description else "GVI"
    stats::setNames(list(out), name)
}


#########add_OM_GVI####
#' @title Add vector control interventions to an OpenMalaria scenario
#'
#' @description Writes the generic vector interventions from
#' \code{\link{get_OM_GVI_snippet}} into an openMalariaUtilities
#' \code{baseList}. When the three effects of an intervention share the same
#' decay (e.g. no insecticide decay, or the same insecticide decay for all
#' effects), they are written as \strong{one} component named after the
#' intervention, as in ITNcascades; otherwise as one component per effect
#' (\code{<name>-deterrency}, \code{<name>-preprandialKillingEffect},
#' \code{<name>-postprandialKillingEffect}), as
#' \code{openMalariaUtilities::defineVectorControl()} does. The component ids
#' are printed: when there are several per intervention, they must be deployed
#' together, to the same people.
#'
#' Requires the openMalariaUtilities package (uses its internal functions
#' \code{.defineInterventionsHeader()} and \code{.xmlAddList()}).
#'
#' @param baseList openMalariaUtilities \code{baseList}; with
#'   \code{append = TRUE}, \code{baseList$interventions$human} must exist.
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
    if (!requireNamespace("openMalariaUtilities", quietly = TRUE))
        stop("add_OM_GVI() requires the openMalariaUtilities package")
    xml_add = utils::getFromNamespace(".xmlAddList", "openMalariaUtilities")
    header  = utils::getFromNamespace(".defineInterventionsHeader",
                                      "openMalariaUtilities")
    if (append && is.null(baseList$interventions$human))
        stop("To append, baseList$interventions$human must exist ",
             "(or use append = FALSE).")

    effects  = c("deterrency", "preprandialKillingEffect", "postprandialKillingEffect")
    baseList = header(baseList = baseList)
    ids = character(0)
    for (k in names(gvi)) {
        comp  = gvi[[k]]
        mosqs = names(comp[[effects[1]]]$anophelesParams)
        same  = merge && all(vapply(effects[-1], function(e)
            identical(comp[[e]]$decay, comp[[effects[1]]]$decay), logical(1)))
        groups = if (same) list(effects) else as.list(effects)

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
            baseList = xml_add(data = baseList, sublist = c("interventions", "human"),
                               append = append, entry = "component",
                               input = list(id = id, name = id,
                                            GVI = c(list(decay = comp[[g[1]]]$decay),
                                                    params)))
            append = TRUE
            ids = c(ids, id)
        }
    }
    message("add_OM_GVI: components written (deploy those of the same ",
            "intervention together): ", paste(ids, collapse = ", "))
    baseList
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
    message("get_OM_GVI_snippet: ", comp, " decay = Weibull fitted to attrition x ",
            "insecticide decay (rmse ", signif(fd$rmse, 3), ")")
    list(L = om_num(fd$L / 365), "function" = "weibull", k = om_num(fd$kappa))
}
