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

    seasonalityParameters = lapply(shares$mosquito, function(m) list(
        annualEIR      = paste0("@EIR_", gsub("[^A-Za-z0-9]+", "_", m), "@"),
        seasonality    = paste0("@m", 1:12, "@"),
        propInfected   = propInfected,
        propInfectious = propInfectious))
    names(seasonalityParameters) = shares$mosquito

    list(mosquitoParameters    = mosquitoParameters,
         seasonalityParameters = seasonalityParameters,
         shares                = shares,
         model_params          = model_params)
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
#' @param interv Intervention object from \code{\link{build_intervention}}.
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
    interv = om_unwrap(interv)
    if (is.null(interv))
        stop("interv must be an intervention object from build_intervention()")
    if (is.null(name)) name = om_default_name(interv, "GVI")
    if (interv$settings$mode == "dummy_host")
        message("get_OM_GVI_snippet: dummy-host intervention, the GVI has ",
                "deterrency and post-prandial killing only; host-seeking ",
                "killing is exported by define_OM_vector_control().")
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

# Internal: the intervention object (element of build_intervention()'s list),
# or NULL
om_unwrap = function(x) {
    if (!is.list(x)) return(NULL)
    if (!is.null(x$settings)) return(x)
    if (length(x) == 1L && is.null(names(x)) && is.list(x[[1]]) &&
        !is.null(x[[1]]$settings)) return(x[[1]])
    NULL
}

om_default_name = function(interv, default) {
    if (!is.null(interv$description) && interv$description != "custom")
        interv$description else default
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

    effects  = unname(om_gvi_effects)
    baseList = header(baseList = baseList)
    ids = character(0)
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
# intervention object (template = FALSE), or a named list of them, all with
# the same model (template = TRUE, values as placeholders).
om_nets = function(interv) {
    one = om_unwrap(interv)
    if (!is.null(one)) return(list(nets = list(one), template = FALSE))
    nets = if (is.list(interv)) lapply(interv, om_unwrap) else list(NULL)
    if (!length(nets) || any(vapply(nets, is.null, logical(1))))
        stop("interv must be an intervention from build_intervention(), or a ",
             "named list of them")
    if (is.null(names(interv)) || any(names(interv) == "") ||
        anyDuplicated(names(interv)))
        stop("A list of interventions needs unique, non-empty names")
    modes = unique(vapply(nets, function(n) n$settings$mode, character(1)))
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
             " days), for the nets of the first campaign: increase duration_years")
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


#########define_OM_vector_control####
#' @title Define a vector control intervention in an OpenMalaria scenario
#'
#' @description Writes the definition of a vector control intervention (ITN or
#' IRS) into an openMalariaUtilities \code{baseList} (OpenMalaria schema 44),
#' for both model structures: the generic vector intervention (GVI) of
#' \code{\link{add_OM_GVI}} (deterrency, pre-prandial killing in the standard
#' model, post-prandial killing) and, for dummy-host interventions, the
#' host-seeking killing. Deploy it with \code{\link{deploy_OM_vector_control}}
#' and the same \code{name} and \code{schedule}.
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
#' value the average of \eqn{d(t) / muvA} over that period. The message gives
#' the average vectorial capacity reduction from host-seeking killing alone
#' with the exact curve and with the steps (at the numeric coverages, or 0.8
#' for placeholders). Host-seeking killing acts on the same populations as the
#' GVI (in bed for ITNs).
#'
#' \strong{Placeholders.} Coverages in \code{schedule} can be placeholders
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
#' @param baseList openMalariaUtilities \code{baseList}, with
#'   \code{baseList$interventions$human}.
#' @param interv Intervention object from \code{\link{build_intervention}}, or
#'   a named list of them (template).
#' @param om_ento Output of \code{\link{get_OM_ento_snippet}}.
#' @param schedule Data frame with one row per campaign: \code{date}
#'   (\code{"YYYY-MM-DD"}, increasing) and \code{coverage} (numbers in [0, 1]
#'   or placeholders such as \code{"@cov2021@"}).
#' @param end_date End of the simulated period (\code{"YYYY-MM-DD"}), the end of
#'   the last host-seeking killing step. The duration of the interventions
#'   (\code{duration_years}) must cover the period from the first campaign to
#'   \code{end_date}.
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
#' model_p = build_model_obj(def_vector_params(), def_host_params())
#' nets = lapply(c(IG2 = "Interceptor G2", OlysetPlus = "Olyset Plus"),
#'     function(n) {
#'         p = get_ITN_params(n, EHT = ..., mode = "dummy_host")
#'         build_intervention(p$efficacy, halflife_attrition = 2 * 365,
#'             shape_attrition = 2, duration_years = 9, model_p = model_p,
#'             mode = p$mode, halflife_insecticide = p$halflife_insecticide)
#'     })
#' om_ento  = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
#'                                activity = def_activity_patterns())
#' schedule = data.frame(date = c("2021-01-01", "2024-01-01", "2027-01-01"),
#'                       coverage = c("@cov2021@", "@cov2024@", "@cov2027@"))
#' baseList = define_OM_vector_control(baseList, nets, om_ento, schedule,
#'                                     end_date = "2030-01-01", name = "ITN")
#' baseList = deploy_OM_vector_control(baseList, "ITN", schedule)
#' values = om_vector_control_values(nets, om_ento, schedule, "2030-01-01",
#'     coverages = expand.grid(cov2021 = c(0.5, 0.8), cov2024 = 0.6,
#'                             cov2027 = 0.6), name = "ITN")
#' }
#'
#' @export
#'
define_OM_vector_control = function(baseList, interv, om_ento, schedule, end_date,
                                    intervention_type = c("ITN", "IRS"),
                                    name = NULL) {
    intervention_type = match.arg(intervention_type)
    x    = om_nets(interv)
    sch  = om_schedule(schedule, end_date)
    name = om_name(x, name, intervention_type)
    pn   = om_ph_name(name)
    mode = x$nets[[1]]$settings$mode
    mosq   = om_ento$shares$mosquito
    active = om_active(om_ento, intervention_type)

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
        baseList = add_OM_GVI(baseList, stats::setNames(list(gvi), name), merge = FALSE)
    } else {
        baseList = add_OM_GVI(baseList, om_gvi_list(x$nets[[1]], om_ento,
                                                     intervention_type, name))
    }
    if (mode != "dummy_host") return(baseList)

    # Host-seeking killing: one seekingDeathRateIncrease step per campaign
    muvA = om_muvA(om_ento, active)
    facs = lapply(x$nets, om_hsk_factors, sch = sch)
    numeric_values = !x$template && !any(sch$placeholder)
    if (numeric_values) d_step = as.vector(facs[[1]]$B %*% sch$num)
    vp = baseList$interventions$vectorPop
    existing = vapply(vp[names(vp) == "intervention"],
                      function(v) as.character(v$name), character(1))
    K = length(sch$dates)
    step_names = paste0(name, "_hsk_", seq_len(K))
    if (any(step_names %in% existing))
        stop("baseList already has host-seeking killing steps named ", name, "_hsk_*")
    for (k in seq_len(K)) {
        anoph = lapply(seq_along(mosq), function(j) list(
            mosquito = mosq[j],
            seekingDeathRateIncrease = list(
                initial = if (!active[j]) "0"
                          else if (numeric_values) om_num(d_step[k] / muvA[j])
                          else paste0("@", pn, "_hsk_", k, "_pop", j, "@"),
                decay = list("function" = "step", L = om_num(facs[[1]]$L_years[k])))))
        names(anoph) = rep("anopheles", length(anoph))
        step = list(name = step_names[k], description = anoph,
                    timed = list(deploy = list(time = format(sch$dates[k]))))
        # Schedule the values were computed for, checked by
        # deploy_OM_vector_control() (an R attribute, not written to the XML)
        attr(step, "om_schedule") = list(dates = sch$dates, coverage = sch$coverage)
        vp = c(vp, list(intervention = step))
    }
    baseList$interventions$vectorPop = vp

    # Approximation check: VC reduction from host-seeking killing alone
    if (!any(active)) return(baseList)
    p1  = which(active)[1]
    cov = ifelse(sch$placeholder, 0.8, sch$num)
    mp  = om_ento$model_params[[mosq[p1]]]
    chk = vapply(facs, om_hsk_check, numeric(2), cov = cov, model_p = mp)
    labels = if (x$template) paste0(names(interv), ": ") else ""
    message("define_OM_vector_control: host-seeking killing as ", K,
            " seekingDeathRateIncrease steps (vectorPop ", name, "_hsk_1..", K,
            ", deployed on the schedule dates).\n",
            "  Average VC reduction from host-seeking killing alone (", mosq[p1],
            ", coverages ", paste(om_num(cov), collapse = ", "), "), exact vs steps: ",
            paste0(labels, om_num(100 * chk[1, ]), "% vs ", om_num(100 * chk[2, ]), "%",
                   collapse = "; "))
    if (!numeric_values)
        message("  Placeholders @", pn, "_hsk_<k>_pop<j>@ (fill with ",
                "om_vector_control_values()): ",
                paste0("pop", which(active), " = ", mosq[active], collapse = ", "))
    baseList
}


#########deploy_OM_vector_control####
#' @title Deploy a vector control intervention in an OpenMalaria scenario
#'
#' @description Writes the deployment of an intervention defined with
#' \code{\link{define_OM_vector_control}}: \strong{one} OpenMalaria
#' \code{<deployment>} containing all its GVI components (one per effect, or
#' one merged component), so that all effects reach the same people, with one
#' \code{<deploy>} per campaign of \code{schedule}. The host-seeking killing
#' steps of dummy-host interventions are deployed by
#' \code{define_OM_vector_control()} (their values depend on the dates and
#' coverages); this function checks that \code{schedule} has the same dates
#' and coverages, and rejects \code{subpop}, \code{cumulative}, \code{minAge}
#' and \code{maxAge} for them (not accounted for in the steps).
#'
#' @param baseList openMalariaUtilities \code{baseList}, after
#'   \code{define_OM_vector_control()}.
#' @param name Name of the intervention given to (or printed by)
#'   \code{define_OM_vector_control()}.
#' @param schedule Data frame with \code{date} and \code{coverage}, as in
#'   \code{define_OM_vector_control()}.
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
deploy_OM_vector_control = function(baseList, name, schedule, subpop = NULL,
                                    cumulative = FALSE, minAge = NULL,
                                    maxAge = NULL) {
    if (!is.character(name) || length(name) != 1L || is.na(name))
        stop("name must be a single string")
    sch = om_schedule(schedule)

    human = baseList$interventions$human
    ids = vapply(human[names(human) == "component"],
                 function(cm) as.character(cm$id), character(1))
    ids = ids[ids == name | ids %in% paste0(name, "-", om_gvi_effects)]
    if (!length(ids))
        stop("No component '", name, "' or '", name, "-<effect>' in baseList: ",
             "call define_OM_vector_control() first")

    # Host-seeking killing steps (dummy host): their values were computed by
    # define_OM_vector_control() for its schedule and the whole population, so
    # the deployment must have the same dates and coverages, unrestricted
    vp = baseList$interventions$vectorPop
    vp = vp[names(vp) == "intervention"]
    step_names = vapply(vp, function(v) as.character(v$name), character(1))
    prefix = paste0(name, "_hsk_")
    is_step = startsWith(step_names, prefix) &
        grepl("^[0-9]+$", substring(step_names, nchar(prefix) + 1L))
    if (any(is_step)) {
        if (!is.null(subpop) || isTRUE(cumulative) || !is.null(minAge) || !is.null(maxAge))
            stop("subpop, cumulative, minAge and maxAge are not supported for ",
                 "dummy-host interventions: the host-seeking killing steps assume ",
                 "campaigns covering the whole population at the schedule coverages")
        defined = attr(vp[is_step][[1]], "om_schedule")
        if (is.null(defined))
            stop("Cannot check the schedule of the host-seeking killing steps ",
                 name, "_hsk_*: call define_OM_vector_control() again")
        if (length(defined$dates) != length(sch$dates) ||
            any(defined$dates != sch$dates) || any(defined$coverage != sch$coverage))
            stop("schedule differs from the one of define_OM_vector_control(), ",
                 "used for the host-seeking killing (dates ",
                 paste(format(defined$dates), collapse = ", "), "; coverages ",
                 paste(defined$coverage, collapse = ", "), ")")
    }

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
    baseList$interventions$human = c(human, list(deployment = c(
        list(name = name), components, list(timed = timed))))
    message("deploy_OM_vector_control: deployment '", name, "' with components ",
            paste(ids, collapse = ", "), " at ", paste(format(sch$dates), collapse = ", "))
    baseList
}


#########om_vector_control_values####
#' @title Values of the placeholders of a vector control intervention
#'
#' @description Computes the values of the placeholders written by
#' \code{\link{define_OM_vector_control}}, to fill an OpenMalaria scenario
#' table: one row per intervention (for a named list) and coverage scenario.
#'
#' @inheritParams define_OM_vector_control
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
    mode = x$nets[[1]]$settings$mode
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
        net = x$nets[[i]]
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
            d_step = cov_mat %*% t(om_hsk_factors(net, sch)$B)   # scenarios x steps
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
