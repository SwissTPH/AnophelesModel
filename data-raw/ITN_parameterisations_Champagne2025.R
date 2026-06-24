# Run this script once from the package root to (re)generate the dataset:
#   source("data-raw/ITN_parameterisations_Champagne2025.R")

raw = read.csv("data/fitted_parameters_posteriormax.csv",
               stringsAsFactors = FALSE, check.names = FALSE)

# Drop obsolete and nuisance columns
drop_cols = c("halflife_insecticide_old", "aged_time", "treatment")
ITN_parameterisations_Champagne2025 = raw[, setdiff(names(raw), drop_cols)]

# Tidy column names to snake_case
names(ITN_parameterisations_Champagne2025)[
    names(ITN_parameterisations_Champagne2025) == "halflife_insecticide"
] = "halflife_insecticide"   # keep as-is (days)

save(ITN_parameterisations_Champagne2025,
     file = "data/ITN_parameterisations_Champagne2025.RData")

message("Saved data/ITN_parameterisations_Champagne2025.RData")
