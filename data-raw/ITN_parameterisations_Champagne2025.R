# Run this script once from the package root to (re)generate the dataset:
#   source("data-raw/ITN_parameterisations_Champagne2025.R")
#
# Source: ITNcascades, EHT_fit/fitted_parameters_posteriormax.csv
# (https://github.com/SwissTPH/ITNcascades), copied to data-raw/.

raw = read.csv("data-raw/fitted_parameters_posteriormax.csv",
               stringsAsFactors = FALSE, check.names = FALSE)

# Fit of each row. A trial (EHT) can have several fits, each with its own
# control arm (e.g. Sovegnon: Interceptor G2 with control treatment -1,
# Pyrethroid-only with control treatment -2); in the table, the rows of a fit
# end with its control rows. get_ITN_params() uses the control alpha_0 of the
# same fit to rescale host-seeking killing.
raw$fit = NA_integer_
for (eht in unique(raw$EHT)) {
    rows    = which(raw$EHT == eht)            # in table order
    is_ctrl = raw$netType[rows] == "control"
    # A block of control rows ends a fit
    block_end = is_ctrl & !c(is_ctrl[-1], FALSE)
    raw$fit[rows] = 1L + c(0L, cumsum(block_end)[-length(rows)])
}

# Drop obsolete and nuisance columns
drop_cols = c("halflife_insecticide_old", "aged_time", "treatment")
ITN_parameterisations_Champagne2025 = raw[, setdiff(names(raw), drop_cols)]

# Checks: each fit of a trial has exactly one control alpha_0
ctrl = ITN_parameterisations_Champagne2025[
    ITN_parameterisations_Champagne2025$netType == "control" &
    ITN_parameterisations_Champagne2025$parameter == "alpha_0", ]
stopifnot(!anyDuplicated(ctrl[, c("EHT", "fit")]))
nets = unique(ITN_parameterisations_Champagne2025[
    ITN_parameterisations_Champagne2025$netType != "control", c("EHT", "fit")])
stopifnot(nrow(merge(nets, ctrl[, c("EHT", "fit")])) == nrow(nets))

save(ITN_parameterisations_Champagne2025,
     file = "data/ITN_parameterisations_Champagne2025.RData")

message("Saved data/ITN_parameterisations_Champagne2025.RData")
