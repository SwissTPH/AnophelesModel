######################################
# Toolbox with basic auxiliary functions used throughout the package:
# - activity pattern helpers
# - matrix utilities
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

##### Matrix helpers #####
rep_row = function(x, n) {
    return(matrix(rep(x, each = n), nrow = n))
}

rep_col = function(x, n) {
    matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}
