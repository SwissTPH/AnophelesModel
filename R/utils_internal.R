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
