#################################################
#### SENSITIVITY ANALYSIS FOR ANOPHELESMODEL ####
#################################################


#### SETUP ####


## Clear working space.
rm(list = ls())

## Load packages.
library(AnophelesModel)
library(dplyr)
library(sensitivity)
library(multisensi)

# Create a matrix of vector parameters.
stephensi_params <- data.frame(
    param = c("M", "M.sd", "Chi", "A0", "A0.sd", "zeta.3", "td", "tau", "ts", "to",
              "endophily", "endophily.sd", "endophagy", "endophagy.sd", "exposure"),
    value = c(0.5577161, 0.0322432, 0.2507095, 0.4951913, 0.05195866, 1,
              0.33, 3, 10, 5, 0.6944887, 0.1345502, 0.399699, 0.04230379, 0.9),
    lower_bound = c(0, 0, 0, 0, 0, 1, 0, 0, 8, 5, 0, 0, 0, 0, 0),
    upper_bound = c(1, 1, 1, 1, 1, 1, 1, 45, 12, 5, 1, 1, 1, 1, 1)
)


#### CALC_SOBOL_IDX FUNCTION ####


# Calculate the main and total effects for global sensitivity analysis.
calc_sobol_idx <- function(GP_model, param_spec, num_points = 10){
    S_mat <- T_mat <- NULL

    # Define wrapper for the GP_model prediction to be called by Sobol analysis.
    GP_f <- function(X) {
        out = predict(x = as.matrix(X), object = GP_model)
        return(out$mean)
    }

    # Construct the two random LHS samples.
    X1 <- lhs(num_ip_points, as.matrix(param_spec))
    X2 <- lhs(num_ip_points, as.matrix(param_spec))

    # Compute the Sobol indices.
    SA <- soboljansen(model = GP_f, as.data.frame(X1), as.data.frame(X2), nboot = 2000)
    S_eff <- SA$S$original
    S_eff[S_eff < 0] <- 0
    T_eff <- SA$T$original
    return(list(S_eff = S_eff, T_eff = T_eff))
}


#### SENSITIVITY ANALYSIS ####


# Run the calc_sobol_idx function.
# calc_sobol_idx(GP_model = my_default_model_stephensi, param_spec = stephensi_params, num_points = 10)



