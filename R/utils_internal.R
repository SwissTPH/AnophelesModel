######################################
# Toolbox with basic auxiliary functions used throughout the package:
# - activity pattern helpers
# - matrix / data frame utilities
# - logit / ilogit transforms
# - average exponential decay
######################################

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

##### Matrix and data frame helpers #####
rep.row = function(x, n) {
    return(matrix(rep(x, each = n), nrow = n))
}

rep.col = function(x, n) {
    matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}

split_tibble = function(tibble, column) {
    split(tibble, tibble[, column])
}

# Converts a data frame to a named list of its rows (names from first column).
#' @importFrom stats setNames
get_list_from_table = function(df) {
    df.list = setNames(split(df[, -1], seq(nrow(df)), drop = TRUE), df[, 1])
    return(df.list)
}

##### Transforms #####
logit = function(x) log(x / (1 - x))

ilogit = function(x) exp(x) / (1 + exp(x))

# Average effect over [0, duration] for an exponential decay with given halflife.
avg_exp_decay = function(initialEffects, halflife, duration) {
    integral = initialEffects * (halflife / log(2)) *
        (1 - exp(-log(2) * duration / halflife))
    return(integral / duration)
}
