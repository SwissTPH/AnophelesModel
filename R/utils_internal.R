######################################
# Toolbox with basic auxiliary functions used throughout the package:
# - activity pattern helpers
# - matrix utilities
######################################

# Shared validation for intervention inputs. NA is permitted only for the
# optional batch host-seeking column, where it denotes an inactive component.
check_efficacy_values = function(x, label, probability = TRUE, allow_na = FALSE) {
    if (allow_na && is.logical(x) && is.null(dim(x)) && all(is.na(x)))
        return(invisible(x))
    if (!is.numeric(x) || !is.null(dim(x)) ||
        (!allow_na && anyNA(x)) || any(is.nan(x)) ||
        any(!is.finite(x[!is.na(x)])))
        stop(label, " must contain finite numeric efficacy values",
             if (!allow_na) " without missing values" else " (NA means inactive)")
    if (any(x < 0, na.rm = TRUE) ||
        (probability && any(x > 1, na.rm = TRUE)))
        stop(label, if (probability) " efficacy values must be in [0, 1]"
                    else " must be >= 0 (host-seeking killing is a rate)")
    invisible(x)
}

# Shared validation of an activity pattern (def_activity_patterns()): a list
# with HBI, HBO, humans_indoors and humans_in_bed, each once. Several patterns
# combined with c() give duplicated entries, of which R would silently use
# the first ones (e.g. the first species' pattern for all species).
check_activity_pattern = function(x, label = "activity") {
    fields = c("HBI", "HBO", "humans_indoors", "humans_in_bed")
    if (!is.list(x) || !all(fields %in% names(x)))
        stop(label, " must be an activity pattern from def_activity_patterns() (with ",
             paste(fields, collapse = ", "), ")")
    dup = unique(names(x)[duplicated(names(x))])
    if (length(dup))
        stop(label, " has duplicated entries (", paste(dup, collapse = ", "), "): ",
             "several activity patterns were combined with c(); give one pattern ",
             "per taxon with list(), e.g. list(pattern_1, pattern_2)")
    invisible(x)
}

# Shared validation of the names of per-component inputs (halflife_insecticide,
# component_decay): a misspelled name (e.g. "Preprandial") would otherwise be
# silently ignored, leaving that effect without its decay
check_component_names = function(x, label, valid) {
    if (is.null(x)) return(invisible())
    nm = names(x)
    if (length(x) && (is.null(nm) || anyNA(nm) || any(nm == "")))
        stop(label, " must be named by component (", paste(valid, collapse = ", "), ")")
    unknown = setdiff(nm, valid)
    if (length(unknown))
        stop(label, ": unknown component(s) ", paste(unknown, collapse = ", "),
             ". Valid names: ", paste(valid, collapse = ", "))
    if (anyDuplicated(nm))
        stop(label, ": duplicated component(s) ", paste(unique(nm[duplicated(nm)]),
                                                        collapse = ", "))
    invisible()
}

# Shared validation of a custom vector table (def_vector_params(vector_table =
# ), the draws of run_interventions(vec_param_sets = )): the parameters the
# model needs must be present and in range. A missing tau silently became a
# 1-day resting duration (calc_psts rounds max(1, numeric(0))), other missing
# columns gave cryptic errors.
.VECTOR_MODEL_COLUMNS = c("M", "Chi", "A0", "zeta.3", "td", "tau", "ts")
check_vector_table = function(vt, label = "vector_table") {
    missing_cols = setdiff(.VECTOR_MODEL_COLUMNS, names(vt))
    if (length(missing_cols))
        stop(label, ": missing column(s) ", paste(missing_cols, collapse = ", "),
             " (needed by the model: ", paste(.VECTOR_MODEL_COLUMNS, collapse = ", "), ")")
    for (nm in .VECTOR_MODEL_COLUMNS)
        if (!is.numeric(vt[[nm]]) || anyNA(vt[[nm]]))
            stop(label, "$", nm, " must be numeric without missing values")
    rows = function(bad) paste(utils::head(which(bad), 10), collapse = ", ")
    for (nm in c("M", "Chi"))
        if (any(vt[[nm]] < 0 | vt[[nm]] > 1))
            stop(label, "$", nm, " must be in [0, 1] (rows ", rows(vt[[nm]] < 0 | vt[[nm]] > 1), ")")
    if (any(vt$A0 <= 0 | vt$A0 >= 1))
        stop(label, "$A0 must be in (0, 1), strictly (rows ", rows(vt$A0 <= 0 | vt$A0 >= 1), ")")
    for (nm in c("td", "tau", "ts"))
        if (any(vt[[nm]] <= 0))
            stop(label, "$", nm, " must be > 0 (rows ", rows(vt[[nm]] <= 0), ")")
    invisible(vt)
}

# Decay columns of batch tables (run_interventions): a misspelled column
# (e.g. halflife_pree) would otherwise be carried to the output unused
.DECAY_COLUMNS = c("halflife_attrition", "decay_attrition",
                   as.vector(outer(c("halflife_", "decay_", "L_", "kappa_"),
                                   c("det", "pre", "post", "hsk"), paste0)))

check_intervention_params = function(ps, label) {
    if (!is.data.frame(ps) || nrow(ps) == 0L)
        stop(label, " must be a non-empty data frame")
    if (anyDuplicated(names(ps))) stop(label, " must have unique column names")
    required = c("deterrency", "preprandial", "postprandial")
    missing_cols = setdiff(required, names(ps))
    if (length(missing_cols))
        stop(label, ": missing efficacy column(s): ", paste(missing_cols, collapse = ", "))
    for (nm in required)
        check_efficacy_values(ps[[nm]], paste0(label, "$", nm))
    decay_like = grep("^(halflife|decay|L|kappa)_", names(ps), value = TRUE)
    unknown = setdiff(decay_like, .DECAY_COLUMNS)
    if (length(unknown))
        stop(label, ": unknown decay column(s) ", paste(unknown, collapse = ", "),
             " (decay columns end in _det, _pre, _post, _hsk, or are ",
             "halflife_attrition / decay_attrition)")
    if (!is.null(ps$host_seeking_killing))
        check_efficacy_values(ps$host_seeking_killing,
                             paste0(label, "$host_seeking_killing"),
                             probability = FALSE, allow_na = TRUE)
    invisible(ps)
}

check_coverage = function(x, label = "coverage_vec", scalar = FALSE) {
    if (!is.numeric(x) || !is.null(dim(x)) || length(x) == 0L ||
        any(!is.finite(x)) || any(x < 0 | x > 1) ||
        (scalar && length(x) != 1L))
        stop(label, " must be ", if (scalar) "a single finite number" else
             "a non-empty vector of finite numbers", " in [0, 1]")
    invisible(x)
}

intervention_npoints = function(duration_years) {
    if (!is.numeric(duration_years) || length(duration_years) != 1L ||
        !is.null(dim(duration_years)) || !is.finite(duration_years) || duration_years <= 0)
        stop("duration_years must be a single finite positive number")
    days = round(365 * duration_years)
    if (!is.finite(days) || days > .Machine$integer.max)
        stop("duration_years produces too many daily time points")
    max(days, 1L)
}

# Returns the time-series vector for an activity pattern given either an
# integer database ID or a numeric vector provided directly by the user.
define_activity = function(input_pattern) {
    if (length(input_pattern) == 1) {
        activity_pattern = activity_patterns[which(activity_patterns$id ==
                                                       input_pattern), "value"]
        # An unknown ID would otherwise give an empty pattern (and NaN
        # exposures later)
        if (!length(activity_pattern))
            stop("Unknown activity pattern ID ", input_pattern,
                 " (see list_activity() for the IDs of activity_patterns)")
    } else {
        assertVector(input_pattern)
        assertNumeric(input_pattern, lower = 0, upper = 1, any.missing = FALSE)
        activity_pattern = input_pattern
    }
    return(activity_pattern)
}

##### Matrix helpers #####
rep_row = function(x, n) {
    return(matrix(rep(x, each = n), nrow = n))
}

rep_col = function(x, n) {
    matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}
