# Functions for building custom intervention objects and running the
# entomological model across multiple parameter sets.

#' @title Parametric decay functions for intervention effects
#'
#' @description Evaluates one of several decay functions at a vector of time
#' points. Supported shapes: \code{"weibull"}, \code{"linear"}, \code{"hill"},
#' \code{"smooth-compact"}, \code{"none"}.
#'
#' @param L Half-life (or decay scale) parameter.
#' @param kappa Shape exponent (required for Weibull, Hill, smooth-compact;
#' ignored for linear and none).
#' @param datej Numeric vector of time points (days) at which to evaluate the
#' decay.
#' @param decay String specifying the decay shape. One of \code{"weibull"},
#' \code{"linear"}, \code{"hill"}, \code{"smooth-compact"}, \code{"none"}.
#'
#' @return Numeric vector of decay values in \code{[0, 1]}.
#'
#' @export
decay_function = function(L, kappa = NULL, datej, decay) {
    if (decay %in% c("weibull", "Weibull")) {
        return(exp(-(datej / L)^kappa * log(2)))
    }
    if (decay %in% c("linear", "Linear")) {
        return(1 - datej / L)
    }
    if (decay %in% c("hill", "Hill")) {
        return(1 / (1 + (datej / L)^kappa))
    }
    if (decay %in% c("smooth-compact", "smoothcompact")) {
        return(exp(kappa - kappa / (1 - (datej / L)^2)))
    }
    if (decay %in% c("none", "no decay")) {
        return(rep(1, length(datej)))
    }
    stop(paste("Unknown decay type:", decay))
}


#' @title Initialise a baseline intervention effects object
#'
#' @description Builds a no-intervention baseline effects object from the host
#' parameters in \code{model_p}. All three host types start with identical
#' parameters — no intervention applied. Used internally by
#' \code{build_intervention}.
#'
#' When \code{dummy_host = TRUE} a fourth host type ("dummy host") is appended
#' with all parameters set to zero. Its availability is subsequently set by
#' \code{build_intervention()} when \code{"HostSeekingKilling"} is supplied in
#' the \code{efficacy} vector.
#'
#' @param model_p Model object from \code{build_model_obj()}.
#' @param npoints Number of interpolation time points.
#' @param dummy_host Logical. If \code{TRUE}, add a fourth dummy-host column
#' (all zeros) to all effects matrices. Default \code{FALSE}.
#'
#' @return A list with fields \code{id}, \code{description},
#' \code{parameterisation}, and \code{effects}.
#'
#' @keywords internal
init_baseline_intervention = function(model_p, npoints, dummy_host = FALSE) {
    h = model_p$host_params

    host_types = c("protected humans", "unprotected humans", "animals")
    Kvi_vec    = c(h$Kvi[1],    h$Kvi)
    alphai_row = c(h$alphai[1], h$alphai)
    PBi_row    = c(h$PBi[1],   h$PBi)
    PCi_row    = c(h$PCi[1],   h$PCi)
    PDi_row    = c(h$PDi[1],   h$PDi)
    PEi_row    = c(h$PEi[1],   h$PEi)

    if (dummy_host) {
        host_types = c(host_types, "dummy host")
        Kvi_vec    = c(Kvi_vec,    0)
        alphai_row = c(alphai_row, 0)
        PBi_row    = c(PBi_row,    0)
        PCi_row    = c(PCi_row,    0)
        PDi_row    = c(PDi_row,    0)
        PEi_row    = c(PEi_row,    0)
    }

    list(
        id               = "custom",
        description      = "custom",
        parameterisation = "custom",
        effects = list(
            host_types = host_types,
            Kvi      = Kvi_vec,
            alphai   = rep.row(alphai_row, npoints),
            PBi      = rep.row(PBi_row,   npoints),
            PCi      = rep.row(PCi_row,   npoints),
            PDi      = rep.row(PDi_row,   npoints),
            PEi      = rep.row(PEi_row,   npoints),
            survival = rep(1, npoints)
        )
    )
}


# Internal: compute attrition survival and per-component insecticide decay
# multiplier matrices from a param_sets data frame.
#
# Returns a list:
#   surv_mat              [np x npoints] — attrition survival at each timepoint
#   mult_det/pre/post/hsk [np x npoints] — dimensionless efficacy multipliers
#   use_dummy             logical
#
# Column conventions (same as run_interventions param_sets):
#   halflife_attrition / shape_attrition / decay_attrition  (attrition decay)
#   halflife_det/pre/post/hsk  (linear insecticide half-life, years; legacy)
#   decay_det/pre/post/hsk + L_det/... + kappa_det/...  (general comp. decay)
make_mult_matrices = function(ps, days) {
    npoints = length(days)
    np      = nrow(ps)

    use_dummy = !is.null(ps$host_seeking_killing) &&
                any(!is.na(ps$host_seeking_killing))

    col_L     = if (!is.null(ps$halflife_attrition)) ps$halflife_attrition else ps$L
    col_kappa = if (!is.null(ps$shape_attrition))    ps$shape_attrition    else ps$kappa
    col_decay = if (!is.null(ps$decay_attrition))    ps$decay_attrition    else ps$decay

    surv_mat = t(vapply(seq_len(np), function(p) {
        kp = col_kappa[p]
        decay_function(L     = col_L[p],
                       kappa = if (is.na(kp)) NULL else kp,
                       datej = days,
                       decay = as.character(col_decay[p]))
    }, numeric(npoints)))

    comp_decay_mat = function(decay_col, L_col, kappa_col, hl_yrs) {
        mat = matrix(1.0, np, npoints)
        for (p in seq_len(np)) {
            dc = if (!is.null(decay_col) && !is.na(decay_col[p])) decay_col[p] else NA
            Lc = if (!is.null(L_col)     && !is.na(L_col[p]))     L_col[p]     else NA
            if (!is.na(dc) && !is.na(Lc)) {
                kc = if (!is.null(kappa_col) && !is.na(kappa_col[p])) kappa_col[p] else NULL
                mat[p, ] = pmax(decay_function(L = Lc, kappa = kc, datej = days, decay = dc), 0)
            } else if (!is.na(hl_yrs[p])) {
                mat[p, ] = pmax(1 - days / (365 * hl_yrs[p]), 0)
            }
        }
        mat
    }
    gc = function(name) if (!is.null(ps[[name]])) ps[[name]]               else rep(NA_real_,      np)
    gs = function(name) if (!is.null(ps[[name]])) as.character(ps[[name]]) else rep(NA_character_, np)

    ins_det  = comp_decay_mat(gs("decay_det"),  gc("L_det"),  gc("kappa_det"),  gc("halflife_det"))
    ins_pre  = comp_decay_mat(gs("decay_pre"),  gc("L_pre"),  gc("kappa_pre"),  gc("halflife_pre"))
    ins_post = comp_decay_mat(gs("decay_post"), gc("L_post"), gc("kappa_post"), gc("halflife_post"))
    ins_hsk  = comp_decay_mat(gs("decay_hsk"),  gc("L_hsk"),  gc("kappa_hsk"),  gc("halflife_hsk"))

    eff_det  = ps$deterrency
    eff_pre  = ps$preprandial
    eff_post = ps$postprandial
    eff_hsk  = if (!is.null(ps$host_seeking_killing))
                   ifelse(is.na(ps$host_seeking_killing), 0, ps$host_seeking_killing)
               else rep(0, np)

    mult_det  = pmax(1 - outer(eff_det,  rep(1, npoints)) * ins_det,  0)
    mult_pre  = pmax(1 - outer(eff_pre,  rep(1, npoints)) * ins_pre,  0)
    mult_post = pmax(1 - outer(eff_post, rep(1, npoints)) * ins_post, 0)
    mult_hsk  = pmax(outer(eff_hsk, rep(1, npoints)) * ins_hsk, 0)
    mult_det [is.na(mult_det)]  = 1
    mult_pre [is.na(mult_pre)]  = 1
    mult_post[is.na(mult_post)] = 1
    mult_hsk [is.na(mult_hsk)]  = 0

    list(surv_mat  = surv_mat,
         mult_det  = mult_det, mult_pre  = mult_pre,
         mult_post = mult_post, mult_hsk = mult_hsk,
         use_dummy = use_dummy)
}


# Internal: multiply decay multiplier matrices by baseline host parameters.
# Returns [np x npoints] matrices of time-varying intervention effects,
# ready to index as eff$alphai_1[param_idx, t] inside the time loop.
apply_eff_matrices = function(mm, alpha_h, PBi_h, PCi_h) {
    list(
        alphai_1  = alpha_h * mm$mult_det,
        PBi_1     = PBi_h   * mm$mult_pre,
        PCi_1     = PCi_h   * mm$mult_post,
        alphai_4  = mm$mult_hsk,   # dummy-host column (zero when not dummy)
        surv_mat  = mm$surv_mat,
        use_dummy = mm$use_dummy
    )
}


#' @title Build an intervention object from efficacy parameters
#'
#' @description Constructs an intervention effects object from user-supplied
#' initial efficacy values (deterrency, pre-prandial killing, post-prandial
#' killing) and a parametric attrition decay function. The three columns of
#' each effects matrix correspond to: (1) protected humans, (2) unprotected
#' humans, (3) animals.
#'
#' Two model structures are supported and are \strong{mutually exclusive}:
#' \itemize{
#'   \item \strong{Standard (3-host)}: uses \code{Deterrency},
#'     \code{PrePrandial}, and \code{PostPrandial}.
#'   \item \strong{Dummy-host (4-host)}: uses \code{Deterrency},
#'     \code{HostSeekingKilling}, and \code{PostPrandial}.
#'     A fourth dummy-host column is added to all effects matrices.
#'     Do not supply \code{PrePrandial} together with \code{HostSeekingKilling}.
#' }
#'
#' @param efficacy Named numeric vector with any subset of the following
#' efficacy parameters:
#' \itemize{
#'   \item \code{"Deterrency"} in \code{[0,1]} — reduces host availability
#'     (\code{alphai}) for protected humans.
#'   \item \code{"PrePrandial"} in \code{[0,1]} — reduces biting probability
#'     (\code{PBi}) for protected humans. Standard model only.
#'   \item \code{"PostPrandial"} in \code{[0,1]} — reduces post-feeding
#'     survival (\code{PCi}) for protected humans.
#'   \item \code{"HostSeekingKilling"} \eqn{\geq 0} — killing during
#'     host-seeking via a dummy-host mechanism. Dummy-host model only.
#'     Values > 1 are valid (dummy host more attractive than unprotected humans).
#' }
#' @param halflife_attrition Attrition decay scale parameter (days).
#' @param shape_attrition Attrition decay shape exponent (for Weibull / Hill /
#' smooth-compact; set to \code{NULL} for linear / none).
#' @param decay_attrition Attrition decay shape string (see \code{decay_function()}).
#' Default \code{"weibull"}.
#' @param decay_insecticide Decay shape for the per-component insecticide decay
#' triggered by \code{halflife_insecticide}. Default \code{"linear"}.
#' @param duration_years Duration of the intervention in years. One
#' interpolation point per day is used, so \code{npoints = round(365 *
#' duration_years)}. Default 3 years.
#' @param model_p Model object from \code{build_model_obj()}.
#' @param name Optional label for the intervention.
#' @param mode Optional character; \code{"standard"} or \code{"dummy_host"}.
#' When supplied, overrides auto-detection from the names of \code{efficacy}.
#' The object returned by \code{get_ITN_params()} includes a \code{mode} field
#' that can be passed here directly.
#' @param halflife_insecticide Optional named numeric vector with insecticide-
#' specific half-lives in years, using the same names as \code{efficacy}.
#' Triggers an additional per-component insecticide decay (shape controlled by
#' \code{decay_insecticide}) on top of the main attrition.
#' For more flexible per-component decay, use \code{component_decay} instead.
#' @param component_decay Optional named list specifying an independent decay
#' function for each efficacy component, using the same names as \code{efficacy}.
#' Each element is a list with fields \code{decay} (string), \code{L} (scale in
#' days), and optionally \code{kappa}. Overrides \code{halflife_insecticide} for
#' the same component.
#' @param npoints Deprecated. Use \code{duration_years} instead.
#' @param L Deprecated. Use \code{halflife_attrition} instead.
#' @param kappa Deprecated. Use \code{shape_attrition} instead.
#' @param decay Deprecated. Use \code{decay_attrition} instead.
#'
#' @return A list of length 1 containing the intervention specification, ready
#' to pass to \code{calculate_impact()}. Standard model: 3-column effects
#' matrices. Dummy-host model: 4-column effects matrices.
#'
#' @keywords internal
#' @export
build_intervention = function(efficacy,
                              halflife_attrition = NULL,
                              shape_attrition    = NULL,
                              decay_attrition    = "weibull",
                              decay_insecticide  = "linear",
                              duration_years     = 3,
                              model_p            = NULL,
                              name               = NULL,
                              mode               = NULL,
                              halflife_insecticide = NULL,
                              component_decay    = NULL,
                              npoints            = NULL,
                              L = NULL, kappa = NULL, decay = NULL) {
    # Backward compatibility: old parameter names
    if (!is.null(L)) {
        warning("'L' is deprecated in build_intervention(); use 'halflife_attrition' instead.")
        if (is.null(halflife_attrition)) halflife_attrition = L
    }
    if (!is.null(kappa)) {
        warning("'kappa' is deprecated in build_intervention(); use 'shape_attrition' instead.")
        if (is.null(shape_attrition)) shape_attrition = kappa
    }
    if (!is.null(decay)) {
        warning("'decay' is deprecated in build_intervention(); use 'decay_attrition' instead.")
        decay_attrition = decay
    }

    if (is.null(model_p)) stop("model_p must be provided")
    prob_components = c("Deterrency", "PrePrandial", "PostPrandial")
    bad = names(efficacy)[names(efficacy) %in% prob_components & (efficacy < 0 | efficacy > 1)]
    if (length(bad))
        stop("efficacy values must be in [0, 1]; out-of-range components: ",
             paste(bad, collapse = ", "))
    if (!is.null(efficacy["HostSeekingKilling"]) &&
        !is.na(efficacy["HostSeekingKilling"]) &&
        efficacy["HostSeekingKilling"] < 0)
        stop("HostSeekingKilling must be >= 0")
    valid_components = c("Deterrency", "PrePrandial", "PostPrandial", "HostSeekingKilling")
    unknown = setdiff(names(efficacy), valid_components)
    if (length(unknown))
        stop("unrecognised efficacy component(s): ", paste(unknown, collapse = ", "),
             ". Valid names: ", paste(valid_components, collapse = ", "))
    if (!is.null(npoints)) {
        warning("'npoints' is deprecated in build_intervention(); use 'duration_years' instead.")
        npoints = max(npoints, 1L)
    } else {
        npoints = max(round(365 * duration_years), 1L)
    }

    use_dummy_host = if (!is.null(mode)) {
        mode == "dummy_host"
    } else {
        "HostSeekingKilling" %in% names(efficacy)
    }
    if (use_dummy_host) {
        message("build_intervention: using dummy-host (4-host) model with HostSeekingKilling.")
    } else {
        message("build_intervention: using standard (3-host) model with PrePrandial.")
    }

    # Convert scalar arguments to the 1-row param_sets format used by make_mult_matrices
    ps = data.frame(
        name         = if (!is.null(name)) name else "custom",
        deterrency   = if ("Deterrency"   %in% names(efficacy)) unname(efficacy["Deterrency"])   else 0,
        preprandial  = if ("PrePrandial"  %in% names(efficacy)) unname(efficacy["PrePrandial"])  else 0,
        postprandial = if ("PostPrandial" %in% names(efficacy)) unname(efficacy["PostPrandial"]) else 0,
        halflife_attrition = halflife_attrition,
        shape_attrition    = if (!is.null(shape_attrition)) shape_attrition else NA_real_,
        decay_attrition    = decay_attrition,
        stringsAsFactors   = FALSE
    )
    if (use_dummy_host)
        ps$host_seeking_killing = unname(efficacy["HostSeekingKilling"])

    # halflife_insecticide (named vector, years) → per-component columns
    if (!is.null(halflife_insecticide)) {
        nm_map = c(Deterrency         = "halflife_det",
                   PrePrandial        = "halflife_pre",
                   PostPrandial       = "halflife_post",
                   HostSeekingKilling = "halflife_hsk")
        for (comp in intersect(names(halflife_insecticide), names(nm_map)))
            ps[[nm_map[[comp]]]] = unname(halflife_insecticide[comp])
        if ("Deterrency"         %in% names(halflife_insecticide) && is.null(component_decay$Deterrency))
            ps$decay_det = decay_insecticide
        if ("PrePrandial"        %in% names(halflife_insecticide) && is.null(component_decay$PrePrandial))
            ps$decay_pre = decay_insecticide
        if ("PostPrandial"       %in% names(halflife_insecticide) && is.null(component_decay$PostPrandial))
            ps$decay_post = decay_insecticide
        if ("HostSeekingKilling" %in% names(halflife_insecticide) && is.null(component_decay$HostSeekingKilling))
            ps$decay_hsk = decay_insecticide
    }

    # component_decay (list of lists) → per-component decay/L/kappa columns
    if (!is.null(component_decay)) {
        sfx = c(Deterrency         = "det",
                PrePrandial        = "pre",
                PostPrandial       = "post",
                HostSeekingKilling = "hsk")
        for (comp in intersect(names(component_decay), names(sfx))) {
            cd = component_decay[[comp]]
            s  = sfx[[comp]]
            ps[[paste0("decay_", s)]] = cd$decay
            ps[[paste0("L_",     s)]] = cd$L
            ps[[paste0("kappa_", s)]] = if (!is.null(cd$kappa)) cd$kappa else NA_real_
        }
    }

    days = seq(0L, npoints - 1L)
    mm   = make_mult_matrices(ps, days)
    h    = model_p$host_params
    eff  = apply_eff_matrices(mm, alpha_h = h$alphai[1], PBi_h = h$PBi[1], PCi_h = h$PCi[1])

    interv_list = list(init_baseline_intervention(model_p, npoints,
                                                  dummy_host = use_dummy_host))
    interv = interv_list[[1]]$effects

    interv$survival    = eff$surv_mat[1L, ]
    interv$alphai[, 1] = eff$alphai_1[1L, ]
    interv$PBi[, 1]    = eff$PBi_1[1L, ]
    interv$PCi[, 1]    = eff$PCi_1[1L, ]
    if (use_dummy_host)
        interv$alphai[, 4] = eff$alphai_4[1L, ]

    interv_list[[1]]$effects  = interv
    interv_list[[1]]$duration = npoints
    if (!is.null(name)) {
        interv_list[[1]]$parameterisation = name
        interv_list[[1]]$description      = name
    }
    return(interv_list)
}


#' @title Run the entomological model across multiple parameter sets
#'
#' @description Fully vectorised runner: evaluates the entomological model for
#' every combination of intervention parameter set, coverage value, and
#' (optionally) vector parameter draw. All \code{param_sets × coverage}
#' combinations are evaluated in a single matrix call to
#' \code{eval_entomological_model()} per timepoint per draw, avoiding
#' per-row R loop overhead and scaling efficiently to tens of thousands of
#' parameter sets.
#'
#' Two model structures are supported and are \strong{mutually exclusive}. The
#' model is auto-detected from \code{param_sets}:
#' \itemize{
#'   \item \strong{Standard (3-host)}: no \code{host_seeking_killing} column
#'     (or all values \code{NA}). Uses \code{deterrency}, \code{preprandial},
#'     \code{postprandial}.
#'   \item \strong{Dummy-host (4-host)}: \code{host_seeking_killing} column
#'     present with at least one non-\code{NA} value. Uses \code{deterrency},
#'     \code{host_seeking_killing}, \code{postprandial}. Set
#'     \code{preprandial = 0}.
#' }
#'
#' @param param_sets Data frame where each row defines one parameter set.
#' Required columns:
#' \itemize{
#'   \item \code{name}: label for the parameter set
#'   \item \code{deterrency}: initial deterrency efficacy in \code{[0,1]}
#'   \item \code{preprandial}: initial pre-prandial killing efficacy in
#'     \code{[0,1]} (set to \code{0} for the dummy-host model)
#'   \item \code{postprandial}: initial post-prandial killing efficacy in
#'     \code{[0,1]}
#'   \item \code{halflife_attrition}: attrition decay scale parameter (days)
#'   \item \code{shape_attrition}: attrition decay shape parameter (set to
#'     \code{NA} for linear / none)
#'   \item \code{decay_attrition}: decay shape string (see
#'     \code{decay_function()})
#' }
#' Optional columns:
#' \itemize{
#'   \item \code{host_seeking_killing}: activates the dummy-host (4-host) model
#'   \item \code{halflife_det}, \code{halflife_pre}, \code{halflife_post},
#'     \code{halflife_hsk}: per-component insecticide half-life (years, linear
#'     decay). \code{halflife_hsk} applies to \code{host_seeking_killing} in
#'     dummy-host mode.
#'   \item \code{decay_det}, \code{L_det}, \code{kappa_det} (and equivalently
#'     \code{_pre}, \code{_post}, \code{_hsk}): general per-component decay
#'     function, half-life (days), and shape. \code{_hsk} controls the decay of
#'     \code{host_seeking_killing}. Overrides the corresponding
#'     \code{halflife_*} column when both are present.
#' }
#' @param model_p Model object from \code{build_model_obj()}.
#' @param coverage_vec Numeric vector of deployment coverages in \code{[0,1]}.
#' Coverage \code{0} is used internally as the no-intervention baseline for
#' computing \code{avg_impact}; it is added automatically if not present, and
#' the corresponding row is included in the output only when explicitly supplied.
#' @param vec_param_sets Optional data frame of vector parameter draws (same
#' columns as \code{vec_ent_param}). Each row is one draw. Adds a \code{draw}
#' column to the output.
#' @param interv_param_sets Optional data frame with \code{n_draw} rows and the
#' same columns as \code{param_sets}. Row \code{d} replaces \code{param_sets}
#' for draw \code{d}. A list of \code{n_draw} single-row data frames is also
#' accepted for backward compatibility.
#' @param activity_sets Reserved for future use.
#' @param coverage_sets Optional list of \code{n_draw} coverage vectors; each
#' replaces \code{coverage_vec} for that draw.
#' @param duration_years Duration of the intervention period in years
#' (one time point per day). Default 3 years.
#' @param npoints Deprecated. Use \code{duration_years} instead.
#'
#' @return A \code{data.table} with one row per
#' \code{(parameter set × coverage [× draw])}. All columns of \code{param_sets}
#' are included; when \code{vec_param_sets} is supplied the draw-specific vector
#' parameter columns are appended. Output columns \code{coverage},
#' \code{avg_vc}, \code{avg_impact}, and (when any uncertainty source is
#' supplied) \code{draw} follow.
#'
#' @export
run_interventions = function(param_sets, model_p, coverage_vec,
                             vec_param_sets    = NULL,
                             interv_param_sets = NULL,
                             activity_sets     = NULL,
                             coverage_sets     = NULL,
                             duration_years = 3,
                             npoints = NULL) {
    if (!is.null(npoints)) {
        warning("'npoints' is deprecated in run_interventions(); use 'duration_years' instead.")
    } else {
        npoints = round(365 * duration_years)
    }

    # Normalise interv_param_sets: accept a data frame (n_draw rows) or a list
    if (is.data.frame(interv_param_sets))
        interv_param_sets = lapply(seq_len(nrow(interv_param_sets)),
                                   function(i) interv_param_sets[i, , drop = FALSE])

    # Emit model-type message once, based on param_sets (or first draw)
    ps_check = if (!is.null(interv_param_sets)) interv_param_sets[[1]] else param_sets
    if (!is.null(ps_check$host_seeking_killing) && any(!is.na(ps_check$host_seeking_killing))) {
        message("run_interventions: using dummy-host (4-host) model ",
                "(host_seeking_killing column detected). ",
                "Set preprandial = 0 for all rows.")
    } else {
        message("run_interventions: using standard (3-host) model.")
    }

    # Determine n_draw from any supplied uncertainty source
    n_draw_sources = c(
        if (!is.null(vec_param_sets))    nrow(vec_param_sets)       else NULL,
        if (!is.null(interv_param_sets)) length(interv_param_sets)  else NULL,
        if (!is.null(activity_sets))     length(activity_sets)       else NULL,
        if (!is.null(coverage_sets))     length(coverage_sets)       else NULL
    )
    if (length(n_draw_sources) > 0) {
        if (length(unique(n_draw_sources)) != 1L)
            stop("All uncertainty sources (vec_param_sets, interv_param_sets, ",
                 "activity_sets, coverage_sets) must have the same number of draws. ",
                 "Got: ", paste(n_draw_sources, collapse = ", "))
        n_draw   = n_draw_sources[1L]
        draw_ids = seq_len(n_draw)
    } else {
        n_draw   = 1L
        draw_ids = NULL
    }

    days = seq(0L, npoints - 1L)

    # Pre-compute multiplier matrices once when intervention params are fixed
    fixed_mult = if (is.null(interv_param_sets)) make_mult_matrices(param_sets, days) else NULL

    # Build draw-specific vector-param model objects
    vec_model_list = if (!is.null(vec_param_sets)) {
        lapply(seq_len(n_draw), function(d) {
            vec_p   = def_vector_params(
                mosquito_species = model_p$host_params$species_name[1],
                vector_table     = vec_param_sets[d, ],
                verbose          = FALSE)
            hosts_p = calc_hosts_ent_params(vec_p, model_p$host_params)
            list(vec_params = vec_p, host_params = hosts_p)
        })
    } else if (n_draw > 1L) {
        rep(list(model_p), n_draw)
    } else {
        list(model_p)
    }

    results_list = vector("list", n_draw)

    for (d in seq_len(n_draw)) {
        ps_d      = if (!is.null(interv_param_sets)) interv_param_sets[[d]] else param_sets
        mm        = if (!is.null(interv_param_sets)) make_mult_matrices(ps_d, days) else fixed_mult
        n_param_d = nrow(ps_d)
        n_host_d  = if (mm$use_dummy) 4L else 3L

        cov_d     = if (!is.null(coverage_sets)) coverage_sets[[d]] else coverage_vec
        has_zero  = 0 %in% cov_d
        cov_eval  = if (has_zero) cov_d else c(0, cov_d)
        zero_col  = which(cov_eval == 0)[1L]
        n_cov_d   = length(cov_eval)

        mp     = vec_model_list[[d]]
        vec_p  = mp$vec_params
        host_p = mp$host_params

        alpha_h = host_p$alphai[1]; alpha_a = host_p$alphai[2]
        PBi_h   = host_p$PBi[1];   PBi_a   = host_p$PBi[2]
        PCi_h   = host_p$PCi[1];   PCi_a   = host_p$PCi[2]
        PDi_h   = host_p$PDi[1];   PDi_a   = host_p$PDi[2]
        PEi_h   = host_p$PEi[1];   PEi_a   = host_p$PEi[2]
        Kvi_h   = host_p$Kvi[1];   Kvi_a   = host_p$Kvi[2]

        # Pre-multiply decay multipliers by baseline host params once, before time loop
        eff = apply_eff_matrices(mm, alpha_h, PBi_h, PCi_h)

        n_rows    = n_param_d * n_cov_d
        param_idx = rep(seq_len(n_param_d), each  = n_cov_d)
        cov_idx   = rep(seq_len(n_cov_d),   times = n_param_d)

        if (n_host_d == 4L) {
            PDi_big = rep.row(c(PDi_h, PDi_h, PDi_a, 0), n_rows)
            PEi_big = rep.row(c(PEi_h, PEi_h, PEi_a, 0), n_rows)
            Kvi_big = rep.row(c(Kvi_h, Kvi_h, Kvi_a, 0), n_rows)
        } else {
            PDi_big = rep.row(c(PDi_h, PDi_h, PDi_a), n_rows)
            PEi_big = rep.row(c(PEi_h, PEi_h, PEi_a), n_rows)
            Kvi_big = rep.row(c(Kvi_h, Kvi_h, Kvi_a), n_rows)
        }

        vc_sum     = matrix(0, n_param_d, n_cov_d)
        impact_sum = matrix(0, n_param_d, n_cov_d)

        for (t in seq_len(npoints)) {
            # Index pre-multiplied [n_param_d × npoints] matrices at timepoint t
            a1 = eff$alphai_1[param_idx, t]
            b1 = eff$PBi_1   [param_idx, t]
            c1 = eff$PCi_1   [param_idx, t]

            if (n_host_d == 4L) {
                alphai_big = cbind(a1, alpha_h, alpha_a, eff$alphai_4[param_idx, t])
                PBi_big    = cbind(b1, PBi_h,   PBi_a,  0)
                PCi_big    = cbind(c1, PCi_h,   PCi_a,  0)
            } else {
                alphai_big = cbind(a1, alpha_h, alpha_a)
                PBi_big    = cbind(b1, PBi_h,   PBi_a)
                PCi_big    = cbind(c1, PCi_h,   PCi_a)
            }

            surv_t   = eff$surv_mat[param_idx, t]
            frac1    = cov_eval[cov_idx] * surv_t
            frac2    = 1 - frac1
            frac3    = rep(1, n_rows)
            frac_big = if (n_host_d == 4L) cbind(frac1, frac2, frac3, frac1)
                       else                cbind(frac1, frac2, frac3)

            t_avail   = alphai_big * frac_big
            host_frac = frac_big / 2L

            ent = eval_entomological_model(
                list(PBi = PBi_big, PCi = PCi_big,
                     PDi = PDi_big, PEi = PEi_big, Kvi = Kvi_big),
                vec_p, host_p, 1L, t_avail, host_frac, 2L)

            vc_mat     = t(matrix(ent$vc_all, nrow = n_cov_d, ncol = n_param_d))
            vc_base    = vc_mat[, zero_col]
            impact_mat = 1 - vc_mat / vc_base
            impact_mat[!is.finite(impact_mat)] = 0

            vc_sum     = vc_sum     + vc_mat
            impact_sum = impact_sum + impact_mat
        }

        avg_vc     = vc_sum     / npoints
        avg_impact = impact_sum / npoints

        # Drop the synthetic coverage-0 column if user did not request it
        if (!has_zero) {
            avg_vc     = avg_vc    [, -zero_col, drop = FALSE]
            avg_impact = avg_impact[, -zero_col, drop = FALSE]
            cov_out    = cov_eval[-zero_col]
            n_cov_out  = length(cov_out)
        } else {
            cov_out   = cov_eval
            n_cov_out = n_cov_d
        }

        out_param_idx = rep(seq_len(n_param_d), each = n_cov_out)
        ps_expanded           = ps_d[out_param_idx, , drop = FALSE]
        rownames(ps_expanded) = NULL
        if (!is.null(vec_param_sets)) {
            vp_row             = vec_param_sets[d, , drop = FALSE]
            rownames(vp_row)   = NULL
            ps_expanded        = cbind(ps_expanded,
                                       vp_row[rep(1L, nrow(ps_expanded)), , drop = FALSE])
        }
        ps_expanded$coverage   = rep(cov_out,          times = n_param_d)
        ps_expanded$avg_vc     = as.vector(t(avg_vc))
        ps_expanded$avg_impact = as.vector(t(avg_impact))
        if (!is.null(draw_ids)) ps_expanded$draw = draw_ids[d]
        results_list[[d]] = data.table::as.data.table(ps_expanded)
    }

    data.table::rbindlist(results_list)
}
