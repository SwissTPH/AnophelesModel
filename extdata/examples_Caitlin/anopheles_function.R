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
library(tgp)

# Create a matrix of vector parameters.
# Missing upper and lower bounds for zeta.3 (availability of non-human hosts) and to (oocyst development time).
vec_params <- data.frame(
    param = c("M", "M.sd", "Chi", "A0", "A0.sd", "zeta.3", "td", "tau", "ts", "to",
              "endophily", "endophily.sd", "endophagy", "endophagy.sd", "exposure"),
    value = c(0.613499, 0.003819127, 0.7947577, 0.6419328, 0.07280429, 1,
              0.33, 3, 10, 5, 0.7743085, 0.01233146, 0.5604133, 0.00505659, 0.9),
    lower_bound = c(0, 0, 0, 0, 0, 1, 0, 0, 10, 5, 0, 0, 0, 0, 0),
    upper_bound = c(1, 1, 1, 1, 1, 1, 1, 45, 10, 5, 1, 1, 1, 1, 1)
)


#### CALC_VC FUNCTION ####


# Create the calc_vc function.
calc_vc <- function(param_spec) {

    # Initialise results.
    results <- numeric(nrow(param_spec))

    # Loop for each combination of vector parameters (i.e., each row of vec_param).
    for (i in 1:nrow(param_spec)) {

        # Define vector parameters.
        vec_p <- list(M = param_spec[i, "M"],
                      M_sd = param_spec[i, "M.sd"],
                      Chi = param_spec[i, "Chi"],
                      A0 = param_spec[i, "A0"],
                      A0_sd = param_spec[i, "A0.sd"],
                      zeta_3 = param_spec[i, "zeta.3"],
                      td = param_spec[i, "td"],
                      tau = param_spec[i, "tau"],
                      ts = param_spec[i, "ts"],
                      to = param_spec[i, "to"],
                      endophily = param_spec[i, "endophily"],
                      endophily_sd = param_spec[i, "endophily.sd"],
                      endophagy = param_spec[i, "endophagy"],
                      endophagy_sd = param_spec[i, "endophagy.sd"],
                      exposure = param_spec[i, "exposure"])

        # Define activity patterns.
        activity_p <- list(HBI = c(0.005608974, 0.061698718, 0.031250000, 0.080929487, 0.095352564, 0.040064103,
                                   0.039262821, 0.042467949, 0.024839744, 0.036057692, 0.051282051, 0.031250000),
                           HBO = c(0.000000000, 0.036057692, 0.083333333, 0.036057692, 0.036057692, 0.016826923,
                                   0.033653846, 0.033653846, 0.061698718, 0.053685897, 0.041666667, 0.027243590),
                           humans_indoors = c(0.61904762, 0.84761905, 0.96190476, 1.00000000, 1.00000000, 1.00000000,
                                              1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.96190476, 0.00952381),
                           humans_in_bed = c(0.00000000, 0.22713330, 0.66663330, 1.00000000, 1.00000000, 1.00000000,
                                             1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.88331670, 0.20066670))

        # Define hosts parameters.
        hosts_p <- def_host_params(mosquito_species = "Anopheles gambiae")

        # Build the model object based on the vector, host, and activity parameters for each species.
        my_default_model <- build_model_obj(vec_p = vec_p, hosts_p = hosts_p,
                                            activity = activity_p, total_pop = 2000)

        # Define the intervention effects using the intervention list with examples included in the package.
        intervention_effects <- def_interventions_effects(intervention_list = intervention_obj_examples,
                                                          model_p = my_default_model,
                                                          num_ip_points = 100, verbose = TRUE,
                                                          specified_multiplier = param_spec[i, "exposure"])

        # Calculate and the impact of interventions using the custom biting patterns.
        impacts <- calculate_impact(interventions_vec = intervention_effects,
                                    coverage_vec = seq(0, 1, by = 0.1),
                                    model_p = my_default_model,
                                    Nv0 = 10000, num_ip_points = 100)

        # Calculate and store the mean reduction in vectorial capacity.
        mean_vc_reduction <- mean(impacts$reduction, na.rm = TRUE)
        results[i] <- mean_reduction
        impacts
    }

    # Return the results.
    return(results)

}


#### MAIN AND TOTAL EFFECTS FOR GLOBAL SENSITIVITY ANALYSIS ####


# Set up.
S_mat <- T_mat <- NULL

# Construct the two random LHS samples.
num_points <- 10
X1 <- lhs(num_points, as.matrix(vec_params %>% select(lower_bound, upper_bound)))
X1 <- as.data.frame(X1)
colnames(X1) <- vec_params$param
X2 <- lhs(num_points, as.matrix(vec_params %>% select(lower_bound, upper_bound)))
X2 <- as.data.frame(X2)
colnames(X2) <- vec_params$param

# Compute the Sobol indices.
SA <- soboljansen(model = calc_vc, X1, X2, nboot = 10)
S_eff <- SA$S$original
S_eff[S_eff < 0] <- 0
T_eff <- SA$T$original
return(list(S_eff = S_eff, T_eff = T_eff))



