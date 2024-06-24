#################################################
#### SENSITIVITY ANALYSIS FOR ANOPHELESMODEL ####
#################################################


#### SETUP ####


## Clear working space.
rm(list = ls())

## Load packages.
library(AnophelesModel)
library(dplyr)
library(tidyr)
library(sensitivity)
library(multisensi)
library(tgp)
library(ggplot2)

# Create a dataframe of vector parameters.
vec_params <- data.frame(
    param = c("M", "Chi", "A0", "endophily", "endophagy", "exposure", "coverage"),
    value = c(0.613499, 0.7947577, 0.6419328, 0.7743085, 0.5604133, 0.9, 0.7),
    lower_bound = c(0, 0, 0, 0, 0, 0, 0),
    upper_bound = c(1, 1, 1, 1, 1, 1, 1)
)

# Initialise tracking of iterations.
total_iterations <- 0
current_iteration <- 0


#### CALC_VC FUNCTION ####


# Create the calc_vc function to calculate the impact of vector control interventions on vectorial capacity.
calc_vc <- function(param_spec) {

    # Initialise results.
    # results <- NULL
    results <- numeric(nrow(param_spec))

    # Update the number of iterations.
    total_iterations <<- total_iterations + nrow(param_spec)

    # Loop for each combination of vector parameters (i.e., each row of vec_param).
    for (i in 1:nrow(param_spec)) {

        # Define vector parameters.
        vec_p <- list(M = param_spec[i, "M"],
                      Chi = param_spec[i, "Chi"],
                      A0 = param_spec[i, "A0"],
                      zeta.3 = 1,
                      td = 0.33,
                      tau = 3,
                      ts = 10,
                      to = 5,
                      endophily = param_spec[i, "endophily"],
                      endophagy = param_spec[i, "endophagy"],
                      exposure = param_spec[i, "exposure"],
                      coverage = param_spec[i, "coverage"])

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

        # Build the model object based on the vector, host, and activity parameters.
        my_default_model <- build_model_obj(vec_p = vec_p, hosts_p = hosts_p,
                                            activity = activity_p, total_pop = 2000)

        # Define the intervention effects for LLINs and IRS as included in the package.
        selected_interventions <- list(LLINs_example = intervention_obj_examples$LLINs_example,
                                       IRS_example = intervention_obj_examples$IRS_example)
        intervention_effects <- def_interventions_effects(intervention_list = selected_interventions,
                                                          model_p = my_default_model,
                                                          num_ip_points = 100, verbose = TRUE,
                                                          specified_multiplier = param_spec[i, "exposure"])

        # Calculate and the impact of interventions.
        impacts <- calculate_impact(interventions_vec = intervention_effects,
                                    coverage_vec = c(0, param_spec[i, "coverage"]),
                                    model_p = my_default_model,
                                    Nv0 = 10000, num_ip_points = 100)

        # Store the mean reduction in vectorial capacity under the intervention of interest as results.
        # Normalise the impact between 0 and 1.
        impact_value <- impacts$interventions_vec$LLINs_example$effects$avg_impact[2]
        # impact_value <- impacts$interventions_vec$IRS_example$effects$avg_impact[2]
        results[i] <- min(max(impact_value, 0), 1)

        # Update and print progress.
        current_iteration <<- current_iteration + 1
        cat("Current iteration:", current_iteration, "/", total_iterations, "\n")
    }

    # Return the results.
    return(results)

}


#### GLOBAL SENSITIVITY ANALYSIS ####


# Set up.
S_mat <- T_mat <- NULL

# Define the create_lhs_samples function to create LHS samples.
create_lhs_samples <- function(num_points, vec_params) {
    lhs_samples <- lhs(num_points, as.matrix(vec_params %>% select(lower_bound, upper_bound)))
    lhs_samples <- as.data.frame(lhs_samples)
    colnames(lhs_samples) <- vec_params$param
    return(lhs_samples)
}

# Construct the two random LHS samples.
num_points <- 50000
X1 <- create_lhs_samples(num_points, vec_params)
X2 <- create_lhs_samples(num_points, vec_params)

# Compute the Sobol indices (main and total effects).
SA <- soboljansen(model = calc_vc, X1 = X1, X2 = X2, nboot = 5000, conf = 0.95)

# Extract the sensitivity indices with their corresponding confidence intervals.
SA_S <- SA$S
SA_T <- SA$T
SA_S_min <- min(SA_S$`min. c.i.`)
SA_S_max <- max(SA_S$`max. c.i.`)
SA_T_min <- min(SA_T$`min. c.i.`)
SA_T_max <- max(SA_T$`max. c.i.`)
SA_plot <- data.frame(
    Parameter = rep(rownames(SA_S), 2),
    Effect = rep(c("Single Effect", "Total Effect"), each = nrow(SA_S)),
    Value = c(SA_S$original, SA_T$original),
    MinCI = c(SA_S$`min. c.i.`, SA_T$`min. c.i.`),
    MaxCI = c(SA_S$`max. c.i.`, SA_T$`max. c.i.`))
SA_min <- min(SA_plot$MinCI, na.rm = TRUE)
SA_max <- max(SA_plot$MaxCI, na.rm = TRUE)

# Plot the sensitivity indices with their corresponding confidence intervals.
ggplot(SA_plot, aes(x = Parameter, y = Value, color = Effect)) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = MinCI, ymax = MaxCI), width = 0.2, position = position_dodge(width = 0.5)) +
    coord_cartesian(ylim = c(SA_min, SA_max)) +
    scale_color_manual(values = c("Single Effect" = "hotpink", "Total Effect" = "royalblue")) +
    labs(y = "Sensitivity Index", x = "Parameter") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### SENSITVITY PLOTS ####


# Extract single and total effect indices.
S_eff <- SA$S$original
S_eff[S_eff < 0] <- 0
T_eff <- SA$T$original
list(S_eff = S_eff, T_eff = T_eff)

# Create a data frame for plotting.
S_eff <- as.data.frame(S_eff)
T_eff <- as.data.frame(T_eff)
S_eff$Parameter <- vec_params$param
T_eff$Parameter <- vec_params$param
sensitivity_df <- data.frame(Parameter = S_eff$Parameter, FirstOrder = S_eff, TotalOrder = T_eff)

# Create the first-order sensitivity bar plot.
ggplot(sensitivity_df, aes(x = reorder(Parameter, -FirstOrder.S_eff), y = FirstOrder.S_eff)) +
    geom_bar(stat = "identity", fill = "hotpink") +
    labs(title = "Main Sensitivity Indices", x = "Parameter", y = "Main Sensitivity Index") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create the total-order sensitivity bar plot.
ggplot(sensitivity_df, aes(x = reorder(Parameter, -TotalOrder.T_eff), y = TotalOrder.T_eff)) +
    geom_bar(stat = "identity", fill = "royalblue") +
    labs(title = "Total Sensitivity Indices", x = "Parameter", y = "Total Sensitivity Index") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a combined plot with both first-order and total-order sensitivity indices.
sensitivity_df_long <- sensitivity_df %>% pivot_longer(cols = c("FirstOrder.S_eff", "TotalOrder.T_eff"),
                                                       names_to = "IndexType", values_to = "SensitivityIndex")

# Create the combined bar plot
ggplot(sensitivity_df_long, aes(x = reorder(Parameter, -SensitivityIndex), y = SensitivityIndex, fill = IndexType)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Single Effect and Total Effect Sensitivity Indices",
         x = "Parameter", y = "Sensitivity Index") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = c("FirstOrder.S_eff" = "hotpink", "TotalOrder.T_eff" = "royalblue"),
                      labels = c("Single Effect", "Total Effect"), name = "Effect")



