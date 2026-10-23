# Functions for building custom intervention objects and running the
# entomological model across multiple parameter sets.

#' @title Parametric decay functions for intervention effects
#'
#' @description Evaluates one of several decay functions at a vector of time
#' points, following the OpenMalaria decay conventions. With
#' \eqn{x = t / L}:
#' \tabular{lll}{
#'   \strong{shape} \tab \strong{value} \tab \strong{meaning of L} \cr
#'   \code{"weibull"} \tab \eqn{\exp(-x^\kappa \log 2)} \tab half-life \cr
#'   \code{"exponential"} \tab \eqn{\exp(-x \log 2)} \tab half-life \cr
#'   \code{"hill"} \tab \eqn{1 / (1 + x^\kappa)} \tab half-life \cr
#'   \code{"linear"} \tab \eqn{\max(1 - x, 0)} \tab time to zero \cr
#'   \code{"step"} \tab 1 if \eqn{t < L}, else 0 \tab time to zero \cr
#'   \code{"smooth-compact"} \tab \eqn{\exp(\kappa - \kappa / (1 - x^2))} if
#'     \eqn{t < L}, else 0 \tab time to zero \cr
#'   \code{"none"} / \code{"constant"} \tab 1 \tab unused \cr
#' }
#' Shape names are case-insensitive; \code{"smoothcompact"} and
#' \code{"no decay"} are accepted as aliases.
#'
#' @param L Half-life or time to zero (days), depending on the shape (see
#' table). Ignored for \code{"none"} / \code{"constant"}.
#' @param kappa Shape exponent (required for Weibull, Hill, smooth-compact;
#' ignored otherwise).
#' @param datej Numeric vector of time points (days) at which to evaluate the
#' decay.
#' @param decay String specifying the decay shape (see table).
#'
#' @return Numeric vector of decay values in \code{[0, 1]}.
#'
#' @export
decay_function = function(L, kappa = NULL, datej, decay) {
    valid = c("weibull", "exponential", "hill", "linear", "step",
              "smooth-compact", "none")
    if (length(decay) != 1L || is.na(decay))
        stop("decay must be a single string. Valid types: ",
             paste(valid, collapse = ", "))
    shape = tolower(as.character(decay))
    shape = switch(shape, "smoothcompact" = "smooth-compact",
                   "no decay" = , "constant" = "none", shape)
    if (!(shape %in% valid))
        stop("Unknown decay type: ", paste(decay, collapse = ", "),
             ". Valid types: ", paste(valid, collapse = ", "))

    if (shape == "none") return(rep(1, length(datej)))

    if (!is.numeric(L) || length(L) != 1L || is.na(L) || L <= 0)
        stop("decay '", shape, "' requires L to be a single positive number")
    if (shape %in% c("weibull", "hill", "smooth-compact") &&
        (!is.numeric(kappa) || length(kappa) != 1L || is.na(kappa) || kappa <= 0))
        stop("decay '", shape, "' requires kappa to be a single positive number")

    x = datej / L
    switch(shape,
        "weibull"     = exp(-x^kappa * log(2)),
        "exponential" = exp(-x * log(2)),
        "hill"        = 1 / (1 + x^kappa),
        "linear"      = pmax(1 - x, 0),
        "step"        = as.numeric(datej < L),
        "smooth-compact" = {
            out    = numeric(length(x))
            inside = x < 1
            out[inside] = exp(kappa - kappa / (1 - x[inside]^2))
            out
        })
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
            alphai   = rep_row(alphai_row, npoints),
            PBi      = rep_row(PBi_row,   npoints),
            PCi      = rep_row(PCi_row,   npoints),
            PDi      = rep_row(PDi_row,   npoints),
            PEi      = rep_row(PEi_row,   npoints),
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
#   ins                   list(det, pre, post, hsk) of [np x npoints]
#                         insecticide decay factors (all 1 when no decay)
#   use_dummy             logical
#
# Column conventions (same as run_interventions param_sets):
#   halflife_attrition / shape_attrition / decay_attrition  (attrition decay)
#   halflife_det/pre/post/hsk  (linear insecticide time-to-zero, years)
#   decay_det/pre/post/hsk + L_det/... + kappa_det/...  (general comp. decay)
#
# insecticide_decay = FALSE ignores all per-component columns: efficacies stay
# at their initial (unwashed) values and only attrition decays.
make_mult_matrices = function(ps, days, insecticide_decay = TRUE) {
    npoints = length(days)
    np      = nrow(ps)

    use_dummy = !is.null(ps$host_seeking_killing) &&
                any(!is.na(ps$host_seeking_killing))

    # Attrition columns, matched exactly ([[ ]]: $ partially matches, e.g. a
    # missing shape_attrition would pick up kappa_det). decay_attrition is
    # required; halflife_attrition and shape_attrition may be absent (NA) when
    # the shape does not need them.
    col_decay = ps[["decay_attrition"]]
    if (is.null(col_decay))
        stop("Missing column decay_attrition (attrition decay shape, e.g. ",
             "\"weibull\" or \"none\")", call. = FALSE)
    col_L     = if (!is.null(ps[["halflife_attrition"]])) ps[["halflife_attrition"]] else rep(NA_real_, np)
    col_kappa = if (!is.null(ps[["shape_attrition"]]))    ps[["shape_attrition"]]    else rep(NA_real_, np)

    # Attrition survival [np x npoints], built explicitly (t(vapply()) would
    # simplify to a vector, i.e. a 1 x np matrix, when npoints is 1)
    surv_mat = matrix(NA_real_, np, npoints)
    for (p in seq_len(np)) {
        kp = col_kappa[p]
        surv_mat[p, ] = tryCatch(
            decay_function(L     = col_L[p],
                           kappa = if (is.na(kp)) NULL else kp,
                           datej = days,
                           decay = as.character(col_decay[p])),
            error = function(err) stop(
                "Attrition decay (decay_attrition, halflife_attrition, ",
                "shape_attrition), parameter set row ", p, ": ",
                conditionMessage(err), call. = FALSE))
    }

    comp_decay_mat = function(decay_col, L_col, kappa_col, hl_yrs, component) {
        mat = matrix(1.0, np, npoints)
        if (!insecticide_decay) return(mat)
        for (p in seq_len(np)) {
            dc = if (!is.null(decay_col) && !is.na(decay_col[p])) decay_col[p] else NA
            Lc = if (!is.null(L_col)     && !is.na(L_col[p]))     L_col[p]     else NA
            kc_given = !is.null(kappa_col) && !is.na(kappa_col[p])
            mat[p, ] = tryCatch({
                # L / kappa without a shape would otherwise be silently ignored
                if (is.na(dc) && (!is.na(Lc) || kc_given))
                    stop("L or kappa given without a decay shape (decay_",
                         "<component> / component_decay$decay)")
                if (!is.na(dc)) {
                    # An explicit shape always wins. decay_function validates
                    # its parameters and lets constant shapes omit L / kappa.
                    kc = if (!is.null(kappa_col) && !is.na(kappa_col[p])) kappa_col[p] else NULL
                    decay_function(L = Lc, kappa = kc, datej = days, decay = dc)
                } else if (!is.na(hl_yrs[p]) || is.nan(hl_yrs[p])) {
                    hl = hl_yrs[p]
                    if (!is.numeric(hl) || is.nan(hl) || hl == 0)
                        stop("linear insecticide half-life must be numeric and non-zero; ",
                             "NA means unspecified and Inf means no decay")
                    # Negative values (washed > unwashed) remain increasing;
                    # infinite half-lives remain constant.
                    pmax(1 - days / (365 * hl), 0)
                } else {
                    rep(1, npoints)
                }
            }, error = function(err) stop(
                "Insecticide decay for component '", component,
                "', parameter set row ", p, ": ", conditionMessage(err),
                call. = FALSE))
        }
        mat
    }
    gc = function(name) if (!is.null(ps[[name]])) ps[[name]]               else rep(NA_real_,      np)
    gs = function(name) if (!is.null(ps[[name]])) as.character(ps[[name]]) else rep(NA_character_, np)

    ins_det  = comp_decay_mat(gs("decay_det"),  gc("L_det"),  gc("kappa_det"),  gc("halflife_det"), "Deterrency")
    ins_pre  = comp_decay_mat(gs("decay_pre"),  gc("L_pre"),  gc("kappa_pre"),  gc("halflife_pre"), "PrePrandial")
    ins_post = comp_decay_mat(gs("decay_post"), gc("L_post"), gc("kappa_post"), gc("halflife_post"), "PostPrandial")
    ins_hsk  = comp_decay_mat(gs("decay_hsk"),  gc("L_hsk"),  gc("kappa_hsk"),  gc("halflife_hsk"), "HostSeekingKilling")

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
    # Inputs are validated upstream, so no multiplier can be NA; were one to
    # appear, fail instead of silently treating it as "no effect"
    if (anyNA(mult_det) || anyNA(mult_pre) || anyNA(mult_post) || anyNA(mult_hsk))
        stop("make_mult_matrices: missing efficacy multipliers (check the efficacy ",
             "and decay columns)", call. = FALSE)

    list(surv_mat  = surv_mat,
         mult_det  = mult_det, mult_pre  = mult_pre,
         mult_post = mult_post, mult_hsk = mult_hsk,
         ins       = list(det = ins_det, pre = ins_pre,
                          post = ins_post, hsk = ins_hsk),
         use_dummy = use_dummy)
}


# Internal: fit a Weibull a * exp(-(t/L)^kappa * log(2)) to a decay curve by
# least squares (as in ITNcascades, RCT_validation/helpfunction_omu_gvi.R).
# Used to express attrition x insecticide decay as a single Weibull per
# efficacy component, e.g. for an OpenMalaria GVI export.
#
# Returns c(L, kappa, a, rmse); L in days.
fit_weibull_decay = function(curve, days, L_start = NULL, kappa_start = 1) {
    if (all(abs(curve - 1) < 1e-12))
        return(c(L = Inf, kappa = NA_real_, a = 1, rmse = 0))
    if (is.null(L_start) || !is.finite(L_start) || L_start <= 0) {
        below   = which(curve <= 0.5 * curve[1])
        L_start = if (length(below)) max(days[below[1]], 1) else 2 * max(days)
    }
    sse = function(par)
        sum((par[3] * exp(-(days / par[1])^par[2] * log(2)) - curve)^2)
    fit = optim(c(L_start, kappa_start, min(curve[1], 1)), sse,
                       method = "L-BFGS-B",
                       lower = c(1e-6, 1e-6, 0), upper = c(Inf, Inf, 1),
                       control = list(parscale = c(L_start, 1, 1)))
    # Only the iteration limit (code 1) is reported: L-BFGS-B line-search codes
    # (51, 52) typically occur at the optimum (e.g. an exact Weibull, rmse 0),
    # and the rmse is reported by the callers anyway.
    if (fit$convergence == 1L)
        warning("fit_weibull_decay: the Weibull fit reached the iteration limit ",
                "without converging; rmse ", signif(sqrt(fit$value / length(days)), 3),
                call. = FALSE)
    c(L = fit$par[1], kappa = fit$par[2], a = fit$par[3],
      rmse = sqrt(fit$value / length(days)))
}


# Internal: multiply decay multiplier matrices by the baseline human host
# parameters (host_p from build_model_obj()). Returns [np x npoints] matrices
# of the time-varying parameters of protected humans, in the eff format of
# simulate_vc() (PDi / PEi are not affected by interventions).
apply_eff_matrices = function(mm, host_p) {
    const = function(x) matrix(x, nrow(mm$mult_det), ncol(mm$mult_det))
    list(
        alphai_1  = host_p$alphai[1] * mm$mult_det,
        PBi_1     = host_p$PBi[1]    * mm$mult_pre,
        PCi_1     = host_p$PCi[1]    * mm$mult_post,
        PDi_1     = const(host_p$PDi[1]),
        PEi_1     = const(host_p$PEi[1]),
        # Dummy-host availability (zero when not dummy): the absolute host-
        # seeking killing is used directly as a per-day rate. EHT fits are per
        # hut-night (no td); by provisional convention a hut-night is treated
        # as a day (no division by vec_p$td), see build_intervention() docs.
        alphai_4  = mm$mult_hsk,
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
#' @param efficacy Non-empty numeric vector with unique component names and
#' finite, non-missing values, using any subset of the following
#' efficacy parameters:
#' \itemize{
#'   \item \code{"Deterrency"} in \code{[0,1]} — reduces host availability
#'     (\code{alphai}) for protected humans; deterred mosquitoes keep
#'     searching.
#'   \item \code{"PrePrandial"} in \code{[0,1]} — pre-prandial killing:
#'     reduces the probability that a mosquito that has found a protected
#'     human bites and survives (\code{PBi}); mosquitoes failing this stage
#'     die. Standard model only.
#'   \item \code{"PostPrandial"} in \code{[0,1]} — post-prandial killing:
#'     reduces the probability of surviving after feeding on a protected
#'     human to find a resting place (\code{PCi}).
#'   \item \code{"HostSeekingKilling"} \eqn{\geq 0} — killing during
#'     host-seeking via a dummy-host mechanism. Dummy-host model only. An
#'     \strong{absolute} rate, not a probability: it is used directly as the
#'     availability of the dummy host (one per protected person), in the
#'     same units as the host availabilities \code{alphai}. Values from
#'     experimental hut fits that are relative to the control availability
#'     \code{alpha_0} must first be multiplied by \code{alpha_0}
#'     (\code{get_ITN_params()} does this).
#' }
#'
#' Note on time units: the experimental hut model has no time scale (its
#' rates are per hut-night), whereas the rates of this package are per day,
#' with \code{td} the duration of host seeking. By convention, a hut-night is
#' treated as a day: \code{HostSeekingKilling} is used directly as a per-day
#' rate, without dividing by \code{td}. This is provisional, until the
#' experimental hut fitting model is improved. With this convention, the field
#' impacts of the dummy-host parameterisations are of the same order as those
#' of the standard parameterisations of the same trials, whereas dividing by
#' \code{td} gives systematically higher impacts (the standard model is not a
#' validated benchmark either).
#' @param halflife_attrition Attrition decay parameter \code{L} (days): the
#' half-life for Weibull / exponential / Hill, the time to zero for linear /
#' step / smooth-compact (see \code{decay_function()}).
#' @param shape_attrition Shape parameter \code{kappa} of the attrition decay
#' function (for Weibull / Hill / smooth-compact; set to \code{NULL} otherwise).
#' @param decay_attrition Attrition decay function: \code{"weibull"},
#' \code{"exponential"}, \code{"hill"}, \code{"linear"}, \code{"step"},
#' \code{"smooth-compact"} or \code{"none"} (see \code{decay_function()}).
#' Default \code{"weibull"}.
#' @param duration_years Single finite positive duration in years. One
#' interpolation point per day is used, with at least one point:
#' \code{npoints = max(round(365 * duration_years), 1)}. Default 3 years.
#' @param model_p Model object from \code{build_model_obj()}.
#' @param name Optional label for the intervention.
#' @param mode Optional character; \code{"standard"} or \code{"dummy_host"}.
#' When supplied, overrides auto-detection from the names of \code{efficacy}.
#' The object returned by \code{get_ITN_params()} includes a \code{mode} field
#' that can be passed here directly.
#' @param halflife_insecticide Optional named numeric vector (years), using the
#' same names as \code{efficacy}, triggering an additional per-component
#' \strong{linear} insecticide decay on top of the main attrition: the efficacy
#' of a component is multiplied by \eqn{1 - t / (365 \cdot hl)}. Despite its
#' name, \eqn{hl} is the time at which the effect reaches zero (the effect
#' halves at \eqn{hl / 2}). This is the convention of the Champagne et al.
#' (2025) parameterisations returned by \code{get_ITN_params()}, where
#' \eqn{hl = a / (1 - Washed / Unwashed)}, with \eqn{a} the aging time of the
#' trial's washed arm (3 years, 2 for Sovegnon), so that efficacy after
#' \eqn{a} years equals the washed (20x) value. Negative values (washed efficacy higher than
#' unwashed) make the effect increase linearly over time; \code{Inf} means no
#' insecticide decay. Zero and \code{NaN} values are invalid; \code{NA} means
#' unspecified. For other decay shapes, use \code{component_decay}.
#' @param component_decay Optional named list specifying an independent decay
#' function for each efficacy component, using the same names as \code{efficacy}.
#' Each element is a list with fields \code{decay} (string), \code{L} (days;
#' meaning depends on the shape, see \code{decay_function()}), and optionally
#' \code{kappa}. An explicit shape always overrides \code{halflife_insecticide}
#' for the same component. Constant shapes (\code{"none"}, \code{"constant"})
#' need no \code{L} or \code{kappa}; other shapes require a positive \code{L}
#' and, where applicable, a positive \code{kappa}. Incomplete specifications
#' raise an error instead of falling back to linear or constant decay.
#' @param insecticide_decay Logical (default \code{TRUE}). If \code{FALSE},
#' \code{halflife_insecticide} and \code{component_decay} are ignored: the
#' efficacies keep their initial (e.g. unwashed) values throughout and only
#' attrition decays. If \code{TRUE}, the per-component insecticide decay is
#' applied and, in addition, a Weibull decay is fitted for each efficacy
#' component to the product attrition \eqn{\times} insecticide decay (see
#' Value).
#'
#' @return A list of length 1 containing the intervention specification, ready
#' to pass to \code{calculate_impact()}. Standard model: 3-column effects
#' matrices. Dummy-host model: 4-column effects matrices.
#' When \code{insecticide_decay = TRUE}, the element \code{fitted_decay} is a
#' data frame with one row per efficacy component and columns
#' \code{component}, \code{L} (Weibull half-life, days), \code{kappa},
#' \code{a} (amplitude) and \code{rmse}: the single Weibull per component
#' that best approximates attrition \eqn{\times} insecticide decay over the
#' intervention duration (as used for OpenMalaria GVI parameterisations).
#' The model itself always uses the exact (unfitted) decay.
#' The element \code{settings} keeps the inputs (efficacy, mode, attrition and
#' per-component decays, \code{insecticide_decay}), used by
#' \code{get_OM_GVI_snippet()}. The element \code{param_set} is the same
#' intervention as a one-row \code{param_sets} table of
#' \code{run_interventions()}, which \code{calculate_impact()} evaluates.
#' Several interventions are combined with \code{c()}.
#'
#' @importFrom stats optim
#' @keywords internal
#' @export
build_intervention = function(efficacy,
                              halflife_attrition = NULL,
                              shape_attrition    = NULL,
                              decay_attrition    = "weibull",
                              duration_years     = 3,
                              model_p            = NULL,
                              name               = NULL,
                              mode               = NULL,
                              halflife_insecticide = NULL,
                              component_decay    = NULL,
                              insecticide_decay  = TRUE) {
    if (is.null(model_p)) stop("model_p must be provided")
    if (!is.numeric(efficacy) || !is.null(dim(efficacy)) || length(efficacy) == 0L)
        stop("efficacy must be a non-empty named numeric vector")
    if (is.null(names(efficacy)) || anyNA(names(efficacy)) ||
        any(names(efficacy) == "") || anyDuplicated(names(efficacy)))
        stop("efficacy must have non-empty, unique component names")
    prob_components = c("Deterrency", "PrePrandial", "PostPrandial")
    valid_components = c("Deterrency", "PrePrandial", "PostPrandial", "HostSeekingKilling")
    unknown = setdiff(names(efficacy), valid_components)
    if (length(unknown))
        stop("unrecognised efficacy component(s): ", paste(unknown, collapse = ", "),
             ". Valid names: ", paste(valid_components, collapse = ", "))
    for (comp in names(efficacy))
        check_efficacy_values(efficacy[comp], paste0("efficacy$", comp),
                             probability = comp %in% prob_components)
    npoints = intervention_npoints(duration_years)
    # Single values (a vector would silently build several parameter rows, of
    # which only the first gives $effects)
    for (arg in c("halflife_attrition", "shape_attrition", "name")) {
        v = get(arg)
        if (!is.null(v) && length(v) != 1L) stop(arg, " must be a single value")
    }
    if (!is.character(decay_attrition) || length(decay_attrition) != 1L ||
        is.na(decay_attrition))
        stop("decay_attrition must be a single decay shape name (e.g. \"weibull\")")
    if (!is.logical(insecticide_decay) || length(insecticide_decay) != 1L ||
        is.na(insecticide_decay))
        stop("insecticide_decay must be TRUE or FALSE")
    if (!is.null(halflife_insecticide) &&
        (!is.numeric(halflife_insecticide) || !is.null(dim(halflife_insecticide))))
        stop("halflife_insecticide must be a named numeric vector (years)")

    has_hsk = "HostSeekingKilling" %in% names(efficacy)
    if (!is.null(mode)) {
        if (length(mode) != 1L || !(mode %in% c("standard", "dummy_host")))
            stop("mode must be \"standard\" or \"dummy_host\"")
        if (mode == "standard" && has_hsk)
            stop("HostSeekingKilling requires mode = \"dummy_host\"")
    }
    use_dummy_host = if (!is.null(mode)) mode == "dummy_host" else has_hsk
    if (use_dummy_host && "PrePrandial" %in% names(efficacy) &&
        efficacy[["PrePrandial"]] != 0)
        stop("The dummy-host model (HostSeekingKilling) and PrePrandial are ",
             "mutually exclusive: omit PrePrandial or set it to 0")
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
        halflife_attrition = if (!is.null(halflife_attrition)) halflife_attrition else NA_real_,
        shape_attrition    = if (!is.null(shape_attrition)) shape_attrition else NA_real_,
        decay_attrition    = decay_attrition,
        stringsAsFactors   = FALSE
    )
    if (use_dummy_host)
        ps$host_seeking_killing = if (has_hsk) unname(efficacy["HostSeekingKilling"]) else 0

    # Component names of the decays must be valid (a misspelled name would
    # silently leave that effect without its decay)
    check_component_names(halflife_insecticide, "halflife_insecticide", valid_components)
    check_component_names(component_decay, "component_decay", valid_components)

    # halflife_insecticide (named vector, years) → per-component columns
    if (!is.null(halflife_insecticide)) {
        nm_map = c(Deterrency         = "halflife_det",
                   PrePrandial        = "halflife_pre",
                   PostPrandial       = "halflife_post",
                   HostSeekingKilling = "halflife_hsk")
        for (comp in intersect(names(halflife_insecticide), names(nm_map)))
            ps[[nm_map[[comp]]]] = unname(halflife_insecticide[comp])
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
            # Each element: list(decay, L, kappa), decay required (an element
            # without it, or with a misspelled field, was silently ignored)
            if (!is.list(cd) || is.null(cd$decay) || length(cd$decay) != 1L ||
                is.na(cd$decay))
                stop("component_decay$", comp, " must be a list with a decay shape ",
                     "(e.g. list(decay = \"weibull\", L = 180, kappa = 2))")
            unknown = setdiff(names(cd), c("decay", "L", "kappa"))
            if (length(unknown))
                stop("component_decay$", comp, ": unknown field(s) ",
                     paste(unknown, collapse = ", "), " (fields: decay, L, kappa)")
            for (f in c("L", "kappa"))
                if (!is.null(cd[[f]]) && length(cd[[f]]) != 1L)
                    stop("component_decay$", comp, "$", f, " must be a single number")
            ps[[paste0("decay_", s)]] = cd$decay
            ps[[paste0("L_",     s)]] = cd$L
            ps[[paste0("kappa_", s)]] = if (!is.null(cd$kappa)) cd$kappa else NA_real_
        }
    }

    days = seq(0L, npoints - 1L)
    mm   = make_mult_matrices(ps, days, insecticide_decay = insecticide_decay)
    h    = model_p$host_params
    eff  = apply_eff_matrices(mm, h)

    interv_list = list(init_baseline_intervention(model_p, npoints,
                                                  dummy_host = use_dummy_host))
    interv = interv_list[[1]]$effects

    interv$survival    = eff$surv_mat[1L, ]
    interv$alphai[, 1] = eff$alphai_1[1L, ]
    interv$PBi[, 1]    = eff$PBi_1[1L, ]
    interv$PCi[, 1]    = eff$PCi_1[1L, ]
    if (use_dummy_host)
        interv$alphai[, 4] = eff$alphai_4[1L, ]

    interv_list[[1]]$effects   = interv
    interv_list[[1]]$duration  = npoints
    # The parameter row these effects were computed from, in the param_sets
    # format: calculate_impact() evaluates it with run_interventions()
    interv_list[[1]]$param_set = ps

    # Single Weibull per component approximating attrition x insecticide decay
    if (insecticide_decay) {
        sfx = c(Deterrency = "det", PrePrandial = "pre",
                PostPrandial = "post", HostSeekingKilling = "hsk")
        is_weibull = tolower(decay_attrition) == "weibull"
        comps = intersect(names(sfx), names(efficacy))
        fits = lapply(comps, function(comp) {
            curve = mm$surv_mat[1L, ] * mm$ins[[sfx[[comp]]]][1L, ]
            fit_weibull_decay(curve, days,
                              L_start     = if (is_weibull) halflife_attrition else NULL,
                              kappa_start = if (is_weibull && !is.null(shape_attrition))
                                                shape_attrition else 1)
        })
        interv_list[[1]]$fitted_decay = data.frame(
            component = comps,
            do.call(rbind, fits),
            row.names = NULL, stringsAsFactors = FALSE)
    }

    # Inputs, kept to reproduce the decays exactly (e.g. get_OM_GVI_snippet())
    interv_list[[1]]$settings = list(
        efficacy             = efficacy,
        mode                 = if (use_dummy_host) "dummy_host" else "standard",
        decay_attrition      = decay_attrition,
        halflife_attrition   = halflife_attrition,
        shape_attrition      = shape_attrition,
        halflife_insecticide = halflife_insecticide,
        component_decay      = component_decay,
        insecticide_decay    = insecticide_decay)

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
#'     \code{host_seeking_killing}, \code{postprandial}. \code{preprandial}
#'     must be \code{0} for all rows.
#' }
#'
#' @param param_sets Data frame where each row defines one parameter set.
#' Required columns:
#' \itemize{
#'   \item \code{deterrency}: initial deterrency efficacy in \code{[0,1]}
#'   \item \code{preprandial}: initial pre-prandial killing efficacy in
#'     \code{[0,1]} (set to \code{0} for the dummy-host model)
#'   \item \code{postprandial}: initial post-prandial killing efficacy in
#'     \code{[0,1]}
#'   \item \code{halflife_attrition}: attrition decay parameter \code{L}
#'     (days): half-life for Weibull / exponential / Hill, time to zero for
#'     linear / step / smooth-compact
#'   \item \code{shape_attrition}: attrition decay shape parameter (required
#'     for Weibull / Hill / smooth-compact; \code{NA} otherwise)
#'   \item \code{decay_attrition}: decay shape string (see
#'     \code{decay_function()})
#' }
#' Optional columns:
#' \itemize{
#'   \item \code{name}: label of the parameter set (used by
#'     \code{plot_impact()}); any other column is also carried to the output.
#'   \item \code{host_seeking_killing}: activates the dummy-host (4-host)
#'     model; absolute availability of the dummy host (see
#'     \code{HostSeekingKilling} in \code{build_intervention()}). Non-missing
#'     values must be finite and non-negative; \code{NA} means inactive.
#'   \item \code{halflife_det}, \code{halflife_pre}, \code{halflife_post},
#'     \code{halflife_hsk}: per-component linear insecticide decay (years);
#'     despite the name, the time at which the effect reaches zero (see
#'     \code{halflife_insecticide} in \code{build_intervention()}).
#'     \code{halflife_hsk} applies to \code{host_seeking_killing} in
#'     dummy-host mode.
#'   \item \code{decay_det}, \code{L_det}, \code{kappa_det} (and equivalently
#'     \code{_pre}, \code{_post}, \code{_hsk}): general per-component decay
#'     function, \code{L} (days; meaning depends on the shape, see
#'     \code{decay_function()}), and shape. \code{_hsk} controls the decay of
#'     \code{host_seeking_killing}. Overrides the corresponding
#'     \code{halflife_*} column whenever a shape is specified. Constant shapes
#'     need no \code{L_*} or \code{kappa_*}; other shapes require their
#'     parameters (incomplete specifications raise an error).
#' }
#' @param model_p Model object from \code{build_model_obj()}.
#' @param coverage_vec Non-empty numeric vector of finite deployment coverages
#' in \code{[0,1]}.
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
#' @param exposure_sets Optional numeric vector (or list) of \code{n_draw}
#' exposure factors; each replaces \code{exposure} for that draw, e.g. to
#' propagate uncertainty in activity patterns via \code{calculate_exposure()}.
#' @param coverage_sets Optional list of \code{n_draw} coverage vectors; each
#' replaces \code{coverage_vec} for that draw.
#' @param duration_years Single finite positive duration in years
#' (one time point per day, with at least one point). Default 3 years.
#' @param insecticide_decay Logical (default \code{TRUE}). If \code{FALSE}, the
#' per-component insecticide decay columns (\code{halflife_*}, \code{decay_*},
#' \code{L_*}, \code{kappa_*}) are ignored: efficacies keep their initial
#' (e.g. unwashed) values and only attrition decays.
#' @param exposure Proportion of human exposure to bites that occurs where
#' the intervention protects, in \code{[0, 1]} (default 1). The intervention
#' acts on an effective coverage \code{coverage * exposure}. Typically
#' \code{calculate_exposure(...)$Exposure_Indoor_whileinbed} for ITNs, and
#' \code{$indoor_resting} or \code{$Exposure_Indoor_total} for IRS (see
#' \code{\link{calculate_exposure}}).
#' @param keep_time Logical (default \code{FALSE}): also return the
#' vectorial capacity and impact on each day (memory-heavy for large sweeps).
#'
#' @return A \code{data.table} with one row per
#' \code{(parameter set × coverage [× draw])}. All columns of \code{param_sets}
#' are included; when \code{vec_param_sets} is supplied the draw-specific vector
#' parameter columns are appended. Output columns \code{coverage} (deployment
#' coverage), \code{exposure}, \code{avg_vc}, \code{avg_impact}, and (when any
#' uncertainty source is supplied) \code{draw} follow.
#'
#' With \code{keep_time = TRUE}, a list with this table as \code{impact} and a
#' long \code{data.table} \code{time} with columns \code{param_set} (row of
#' \code{param_sets}), \code{name} (if \code{param_sets} has one),
#' \code{coverage}, \code{day} (0 = deployment), \code{vc}, \code{impact} and,
#' with uncertainty draws, \code{draw}.
#'
#' @export
run_interventions = function(param_sets, model_p, coverage_vec,
                             vec_param_sets    = NULL,
                             interv_param_sets = NULL,
                             exposure_sets     = NULL,
                             coverage_sets     = NULL,
                             duration_years = 3,
                             insecticide_decay = TRUE,
                             exposure = 1,
                             keep_time = FALSE) {
    npoints = intervention_npoints(duration_years)
    if (!is.logical(keep_time) || length(keep_time) != 1L || is.na(keep_time))
        stop("keep_time must be TRUE or FALSE")
    if (!is.logical(insecticide_decay) || length(insecticide_decay) != 1L ||
        is.na(insecticide_decay))
        stop("insecticide_decay must be TRUE or FALSE")
    if (is.null(coverage_sets)) {
        check_coverage(coverage_vec)
    } else {
        if (!is.list(coverage_sets) || length(coverage_sets) == 0L)
            stop("coverage_sets must be a non-empty list of coverage vectors")
        if (!missing(coverage_vec)) check_coverage(coverage_vec)
        for (d in seq_along(coverage_sets))
            check_coverage(coverage_sets[[d]], paste0("coverage_sets[[", d, "]]"))
    }
    check_exposure(exposure, scalar = TRUE)
    if (!is.null(exposure_sets)) {
        exposure_sets = unlist(exposure_sets)
        check_exposure(exposure_sets)
    }

    # Normalise interv_param_sets: accept a data frame (n_draw rows) or a list
    if (is.data.frame(interv_param_sets))
        interv_param_sets = lapply(seq_len(nrow(interv_param_sets)),
                                   function(i) interv_param_sets[i, , drop = FALSE])
    if (!is.null(interv_param_sets) &&
        (!is.list(interv_param_sets) || length(interv_param_sets) == 0L))
        stop("interv_param_sets must be a non-empty data frame or list of data frames")
    if (!is.null(vec_param_sets) &&
        (!is.data.frame(vec_param_sets) || nrow(vec_param_sets) == 0L))
        stop("vec_param_sets must be a non-empty data frame")

    # Model type (message once) and mutual exclusivity of the two models
    all_ps = if (!is.null(interv_param_sets)) interv_param_sets else list(param_sets)
    for (d in seq_along(all_ps))
        check_intervention_params(all_ps[[d]], if (is.null(interv_param_sets))
            "param_sets" else paste0("interv_param_sets[[", d, "]]"))
    is_dummy = function(ps) !is.null(ps$host_seeking_killing) &&
                            any(!is.na(ps$host_seeking_killing))
    # All draws must use the same model structure
    dummy_draws = vapply(all_ps, is_dummy, logical(1))
    if (length(unique(dummy_draws)) > 1L)
        stop("interv_param_sets mixes the standard and dummy-host models (draws ",
             paste(which(dummy_draws), collapse = ", "), " have host_seeking_killing): ",
             "use one model structure for all draws")
    if (is_dummy(all_ps[[1]])) {
        message("run_interventions: using dummy-host (4-host) model ",
                "(host_seeking_killing column detected).")
    } else {
        message("run_interventions: using standard (3-host) model.")
    }
    for (ps in all_ps)
        if (is_dummy(ps) && any(ps$preprandial != 0, na.rm = TRUE))
            stop("The dummy-host model (host_seeking_killing) and pre-prandial ",
                 "killing are mutually exclusive: set preprandial = 0 for all rows.")

    # Determine n_draw from any supplied uncertainty source
    n_draw_sources = c(
        if (!is.null(vec_param_sets))    nrow(vec_param_sets)       else NULL,
        if (!is.null(interv_param_sets)) length(interv_param_sets)  else NULL,
        if (!is.null(exposure_sets))     length(exposure_sets)       else NULL,
        if (!is.null(coverage_sets))     length(coverage_sets)       else NULL
    )
    if (length(n_draw_sources) > 0) {
        if (length(unique(n_draw_sources)) != 1L)
            stop("All uncertainty sources (vec_param_sets, interv_param_sets, ",
                 "exposure_sets, coverage_sets) must have the same number of draws. ",
                 "Got: ", paste(n_draw_sources, collapse = ", "))
        n_draw   = n_draw_sources[1L]
        draw_ids = seq_len(n_draw)
    } else {
        n_draw   = 1L
        draw_ids = NULL
    }

    days = seq(0L, npoints - 1L)

    # Pre-compute multiplier matrices once when intervention params are fixed
    fixed_mult = if (is.null(interv_param_sets))
                     make_mult_matrices(param_sets, days, insecticide_decay) else NULL

    # Build draw-specific vector-param model objects
    vec_model_list = if (!is.null(vec_param_sets)) {
        lapply(seq_len(n_draw), function(d) {
            vec_p   = tryCatch(def_vector_params(
                mosquito_species = model_p$host_params$species_name[1],
                vector_table     = vec_param_sets[d, , drop = FALSE],
                verbose          = FALSE),
                error = function(err) stop("run_interventions, vec_param_sets draw ", d, ": ",
                                           conditionMessage(err), call. = FALSE))
            hosts_p = calc_hosts_ent_params(vec_p, model_p$host_params)
            list(vec_params = vec_p, host_params = hosts_p)
        })
    } else if (n_draw > 1L) {
        rep(list(model_p), n_draw)
    } else {
        list(model_p)
    }

    results_list = vector("list", n_draw)
    time_list    = vector("list", n_draw)

    for (d in seq_len(n_draw)) {
        ps_d      = if (!is.null(interv_param_sets)) interv_param_sets[[d]] else param_sets
        mm        = if (!is.null(interv_param_sets))
                        make_mult_matrices(ps_d, days, insecticide_decay) else fixed_mult
        n_param_d = nrow(ps_d)
        cov_d     = if (!is.null(coverage_sets)) coverage_sets[[d]] else coverage_vec
        exp_d     = if (!is.null(exposure_sets)) exposure_sets[[d]] else exposure

        mp     = vec_model_list[[d]]
        vec_p  = mp$vec_params
        host_p = mp$host_params

        # Pre-multiply decay multipliers by baseline host params, then run the
        # shared time loop on the effective coverage (coverage 0 baseline
        # added / dropped internally, so outputs map 1:1 onto cov_d)
        # Errors name the rows of param_sets (simulate_vc) and the draw
        sim = tryCatch(
            simulate_vc(apply_eff_matrices(mm, host_p), host_base(host_p),
                        vec_p, host_p, cov_d * exp_d, keep_time = keep_time),
            error = function(err) stop(
                "run_interventions", if (!is.null(draw_ids)) paste0(", draw ", d), ": ",
                conditionMessage(err), call. = FALSE))
        avg_vc     = sim$avg_vc
        avg_impact = sim$avg_impact
        cov_out    = cov_d
        n_cov_out  = length(cov_out)

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
        ps_expanded$exposure   = exp_d
        ps_expanded$avg_vc     = as.vector(t(avg_vc))
        ps_expanded$avg_impact = as.vector(t(avg_impact))
        if (!is.null(draw_ids)) ps_expanded$draw = draw_ids[d]
        results_list[[d]] = data.table::as.data.table(ps_expanded)

        if (keep_time) {
            # sim$vc_t / impact_t: [param set, coverage, day] -> day fastest
            idx = expand.grid(day = seq_len(npoints), cov = seq_len(n_cov_out),
                              param_set = seq_len(n_param_d))
            tt = data.table::data.table(param_set = idx$param_set)
            if (!is.null(ps_d$name)) tt$name = ps_d$name[idx$param_set]
            tt$coverage = cov_out[idx$cov]
            tt$day      = idx$day - 1L
            tt$vc       = as.vector(aperm(sim$vc_t,     c(3, 2, 1)))
            tt$impact   = as.vector(aperm(sim$impact_t, c(3, 2, 1)))
            if (!is.null(draw_ids)) tt$draw = draw_ids[d]
            time_list[[d]] = tt
        }
    }

    impact = data.table::rbindlist(results_list)
    if (!keep_time) return(impact)
    list(impact = impact, time = data.table::rbindlist(time_list))
}
