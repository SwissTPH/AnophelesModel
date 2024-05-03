###################
# Example script which runs AnophelesModel 
# 
# created 03.05.2024
###################

# Clear working space
#rm(list = ls())

# Load packages
library(AnophelesModel)

# Define vector parameters for the two species
vec_p_gambiae = def_vector_params(mosquito_species = "Anopheles gambiae")
vec_p_stephensi = def_vector_params(mosquito_species = "Anopheles stephensi")

#### Defining custom activity patterns. 
# The activity patterns consist of the mosquito and human activity.
# The mosquito activity is represented by the indoor and outdoor biting of humans (HBI and HBO).
# The human activity is represented by the time they spend sleeping (humans_in_bed) and indoors (humans_indoors).

# The following defines an activity pattern object with custom values 
# for HBI, HBO, humans_in_bed and humans_indoors and 

# First we define an empty list which will contain the patterns
activity_p_gambiae = def_activity_patterns(activity = "default_Anopheles_gambiae")
# No data for Stephensi biting patterns, to be modified
activity_p_stephensi = def_activity_patterns(activity = "default_Anopheles_gambiae")

# Define hosts parameters for each mosquito species
hosts_p_gambiae = def_host_params(mosquito_species = "Anopheles gambiae")
# No values for stephensi yet, to be updated
hosts_p_stephensi = def_host_params(mosquito_species = "Anopheles gambiae")

# To include custom vector biting patterns you will need to modify the $HBI and $HBO vectors of activity_p
#IMPORTANT: HBI, HBO, humans_indoors, humans_in_bed should have the same 
# length and their values should correspond to the same time points 
# Example: 
# activity_p_stephensi$HBI = c(0.00000000, 0.28380952, 0.61904762, 0.7, 
#                               0.7, 1.00000000, 1.00000000, 1.00000000, 
#                               1.00000000, 1.00000000, 1.00000000, 1.00000000,
#                               0.96190476, 0.00952381, 0.00000000, 0.00000000)
# activity_p_stephensi$HBO = c(0.0000000, 0.0000000, 0.0000000, 0, 
#                              0, 0.5, 0.5, 0.5, 
#                              0.5, 1.0000000, 1.0000000, 1.0000000,
#                              0.8833167, 0.2006667, 0.0000000, 0.0000000)


# Build the model object based on the vector, host and activity parameters for each species
my_default_model_gambiae = build_model_obj(vec_p = vec_p_gambiae,
                                  hosts_p = hosts_p_gambiae,
                                  activity = activity_p_gambiae,
                                  total_pop = 2000)

my_default_model_stephensi = build_model_obj(vec_p = vec_p_stephensi,
                                             hosts_p = hosts_p_stephensi,
                                             activity = activity_p_stephensi,
                                             total_pop = 2000)

# Use the package intervention object

# Define other parameters
coverages= c(seq(0, 1, by = 0.1))
n_ip = 100
host_pop = 2000
vec_pop = 10000

# Define the intervention effects, either from the package database or custom.
# In this example we use the intervention list with examples included in the package
intervention_effects_vec_gambiae = def_interventions_effects(intervention_obj_examples, my_default_model_gambiae, n_ip)
intervention_effects_vec_stephensi = def_interventions_effects(intervention_obj_examples, my_default_model_stephensi, n_ip)


# Calculate the impact of interventions using the custom biting patterns
impacts_gambiae = calculate_impact(intervention_effects_vec_gambiae, coverages, my_default_model_gambiae, vec_pop, n_ip)
impacts_stephensi = calculate_impact(intervention_effects_vec_stephensi, coverages, my_default_model_stephensi, vec_pop, n_ip)

# Plotting impact
plot_impact_species(impacts_gambiae, "VC_red")
plot_impact_species(impacts_stephensi, "VC_red")

