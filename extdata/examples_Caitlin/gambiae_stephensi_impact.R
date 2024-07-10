##########################################################
#### RUNNING ANOPHELESMODEL FOR GAMBIAE VS. STEPHENSI ####
##########################################################


#### SETUP ####


## Clear working space.
rm(list = ls())

## Load packages.
library(AnophelesModel)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(reshape2)

# Filter for activity patterns in Kenya.
activity_kenya <- activity_patterns %>% filter(country == "Kenya")

# Define vector parameters for the two species
vec_p_gambiae <- def_vector_params(mosquito_species = "Anopheles gambiae")
vec_p_stephensi <- def_vector_params(mosquito_species = "Anopheles stephensi")


#### SETUP CUSTOM ACTIVITY PATTERN LISTS ####


## The activity patterns consist of the mosquito and human activity.
## The mosquito activity is represented by the indoor and outdoor biting of humans (HBI and HBO).
## The human activity is represented by the time they spend sleeping (humans_in_bed) and indoors (humans_indoors).
## The following defines an activity pattern object with custom values for HBI, HBO, humans_in_bed, and humans_indoors.

# Define an empty list which will contain the activity patterns for each species.
activity_p_gambiae <- def_activity_patterns(activity = "default_Anopheles_gambiae")
activity_p_stephensi <- def_activity_patterns(activity = "default_Anopheles_gambiae")

# Define hosts parameters for each mosquito species.
hosts_p_gambiae <- def_host_params(mosquito_species = "Anopheles gambiae")
hosts_p_stephensi <- def_host_params(mosquito_species = "Anopheles gambiae")


#### INPUT PARAMETERS ####


# Input HBI and HBO data for Anopheles gambiae based on the species' activity patterns in Rarieda, Kenya.
# Use data for 18:00 to 06:00.
activity_p_gambiae$HBI <- c(0.005608974, 0.061698718, 0.031250000, 0.080929487, 0.095352564, 0.040064103,
                            0.039262821, 0.042467949, 0.024839744, 0.036057692, 0.051282051, 0.031250000)
activity_p_gambiae$HBO <- c(0.000000000, 0.036057692, 0.083333333, 0.036057692, 0.036057692, 0.016826923,
                            0.033653846, 0.033653846, 0.061698718, 0.053685897, 0.041666667, 0.027243590)

# Input humans_indoors data based on human activity patterns in Rarieda, Kenya.
# Use data for 18:00 to 06:00.
activity_p_gambiae$humans_indoors <- c(0.61904762, 0.84761905, 0.96190476, 1.00000000, 1.00000000, 1.00000000,
                                       1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.96190476, 0.00952381)

# Input humans_in_bed data based on human activity patterns in Rachuonyo, Kenya.
# Use data for 18:00 to 06:00.
activity_p_gambiae$humans_in_bed <- c(0.00000000, 0.22713330, 0.66663330, 1.00000000, 1.00000000, 1.00000000,
                                      1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.88331670, 0.20066670)

# Input HBI and HBO data for Anopheles stephensi based on the species' activity patterns in Goa, India.
# Assume that the species' biting behaviour in Africa (Kenya) is similar to that exhibited in Asia (India).
# Use data for 18:00 to 06:00.
activity_p_stephensi$HBI <- c(0.273, 0.25475, 0.2365, 0.21825, 0.2, 0.17725,
                              0.1545, 0.13175, 0.109, 0.18625, 0.2635, 0.418)
activity_p_stephensi$HBO <- c(0.727, 0.74525, 0.7635, 0.78175, 0.8, 0.82275,
                              0.8455, 0.86825, 0.891, 0.81375, 0.7365, 0.582)

# Input humans_indoors data based on human activity patterns in Rarieda, Kenya.
# Use data for 18:00 to 06:00.
activity_p_stephensi$humans_indoors <- c(0.61904762, 0.84761905, 0.96190476, 1.00000000, 1.00000000, 1.00000000,
                                         1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.96190476, 0.00952381)

# Input humans_in_bed data based on human activity patterns in Rachuonyo, Kenya.
# Use data for 18:00 to 06:00.
activity_p_stephensi$humans_in_bed <- c(0.00000000, 0.22713330, 0.66663330, 1.00000000, 1.00000000, 1.00000000,
                                        1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.88331670, 0.20066670)

# Build the model object based on the vector, host, and activity parameters for each species.
my_default_model_gambiae <- build_model_obj(vec_p = vec_p_gambiae, hosts_p = hosts_p_gambiae,
                                            activity = activity_p_gambiae, total_pop = 2000)
my_default_model_stephensi <- build_model_obj(vec_p = vec_p_stephensi, hosts_p = hosts_p_stephensi,
                                              activity = activity_p_stephensi, total_pop = 2000)


#### USE THE PACKAGE INTERVENTION OBJECT ####


# Define the intervention effects for LLINs and IRS as included in the package.
selected_interventions <- list(LLINs_example = intervention_obj_examples$LLINs_example,
                               IRS_example = intervention_obj_examples$IRS_example)
intervention_effects_gambiae <- def_interventions_effects(intervention_list =  selected_interventions,
                                                          model_p = my_default_model_gambiae,
                                                          num_ip_points = 100, verbose = TRUE,
                                                          specified_multiplier = NULL)
intervention_effects_stephensi <- def_interventions_effects(intervention_list = selected_interventions,
                                                            model_p = my_default_model_stephensi,
                                                            num_ip_points = 100, verbose = TRUE,
                                                            specified_multiplier = NULL)

# Plot the impact for both species, accounting for confidence intervals.
# In both cases, 100 samples have been used to estimate the confidence intervals of the vectorial capacity.
impacts_gambiae_ci <- calculate_impact_var(mosquito_species = "Anopheles gambiae",
                                           activity_patterns = activity_p_gambiae,
                                           interventions = intervention_effects_gambiae,
                                           n_sample_points = 100, plot_result = FALSE)
impacts_stephensi_ci <- calculate_impact_var(mosquito_species = "Anopheles stephensi",
                                             activity_patterns = activity_p_stephensi,
                                             interventions = intervention_effects_stephensi,
                                             n_sample_points = 100, plot_result = FALSE)
impacts_gambiae_ci_plot <- plot_impact_var("Anopheles gambiae", impacts_gambiae_ci)
impacts_stephensi_ci_plot <- plot_impact_var("Anopheles stephensi", impacts_stephensi_ci)
impacts_ci <- ggarrange(plotlist = list(impacts_gambiae_ci_plot, impacts_stephensi_ci_plot), ncol = 2, nrow = 1)
impacts_ci

# Calculate and plot the impact of interventions using the custom biting patterns.
impacts_gambiae <- calculate_impact(interventions_vec = intervention_effects_gambiae,
                                    coverage_vec = c(seq(0, 1, by = 0.1)),
                                    model_p = my_default_model_gambiae,
                                    Nv0 = 10000,
                                    num_ip_points = 100)
impacts_stephensi <- calculate_impact(interventions_vec = intervention_effects_stephensi,
                                      coverage_vec = c(seq(0, 1, by = 0.1)),
                                      model_p = my_default_model_stephensi,
                                      Nv0 = 10000,
                                      num_ip_points = 100)
impacts_gambiae_plot <- plot_impact_species(impacts_gambiae, "VC_red")
impacts_stephensi_plot <- plot_impact_species(impacts_stephensi, "VC_red")
impacts <- ggarrange(plotlist = list(impacts_gambiae_plot, impacts_stephensi_plot), ncol = 2, nrow = 1)
impacts

