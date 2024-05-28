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


# Define other parameters.
coverages <- c(seq(0, 1, by = 0.1))
n_ip <- 100
host_pop <- 2000
vec_pop <- 10000

# Define the intervention effects using the intervention list with examples included in the package.
intervention_effects_vec_gambiae <- def_interventions_effects(intervention_list =  intervention_obj_examples,
                                                              model_p = my_default_model_gambiae,
                                                              num_ip_points = n_ip,
                                                              verbose = TRUE,
                                                              specified_multiplier = NULL)
intervention_effects_vec_stephensi <- def_interventions_effects(intervention_list = intervention_obj_examples,
                                                                model_p = my_default_model_stephensi,
                                                                num_ip_points = n_ip,
                                                                verbose = TRUE,
                                                                specified_multiplier = NULL)

# Calculate and plot the impact of interventions using the custom biting patterns.
impacts_gambiae <- calculate_impact(interventions_vec = intervention_effects_vec_gambiae,
                                    coverage_vec = coverages,
                                    model_p = my_default_model_gambiae,
                                    Nv0= vec_pop,
                                    num_ip_points = n_ip)
impacts_stephensi <- calculate_impact(interventions_vec = intervention_effects_vec_stephensi,
                                      coverage_vec = coverages,
                                      model_p = my_default_model_stephensi,
                                      Nv0 = vec_pop,
                                      num_ip_points = n_ip)
impacts_gambiae_plot <- plot_impact_species(impacts_gambiae, "VC_red")
impacts_stephensi_plot <- plot_impact_species(impacts_stephensi, "VC_red")
impacts <- ggarrange(plotlist = list(impacts_gambiae_plot, impacts_stephensi_plot), ncol = 2, nrow = 1)
impacts

# Plot the impact for both species, account for confidence intervals.
# In both cases, 100 samples have been used to estimate the confidence intervals of the vectorial capacity.
impacts_gambiae_ci <- calculate_impact_var(mosquito_species = "Anopheles gambiae",
                                           activity_patterns = activity_p_gambiae,
                                           interventions = intervention_obj_examples,
                                           n_sample_points = 100,
                                           plot_result = FALSE)
impacts_stephensi_ci <- calculate_impact_var(mosquito_species = "Anopheles stephensi",
                                             activity_patterns = activity_p_stephensi,
                                             interventions = intervention_obj_examples,
                                             n_sample_points = 100,
                                             plot_result = FALSE)
impacts_gambiae_ci_plot <- plot_impact_var("Anopheles gambiae", impacts_gambiae_ci)
impacts_stephensi_ci_plot <- plot_impact_var("Anopheles stephensi", impacts_stephensi_ci)
impacts_ci <- ggarrange(plotlist = list(impacts_gambiae_ci_plot, impacts_stephensi_ci_plot), ncol = 2, nrow = 1)
impacts_ci


#### DECAY OF INTERVENTION EFFECTS ####


# Create the calculate_impact_vec funcion.
calculate_impact_vec <- function(tab_activity, mosquito_model_params, host_params) {
    HBI_vec <- tab_activity$HBI
    HBO_vec <- tab_activity$HBO
    humans_indoors <- tab_activity$humans_indoors
    humans_in_bed <- tab_activity$humans_in_bed
    patterns_vec <- list(HBI_vec = HBI_vec, HBO_vec = HBO_vec, humans_indoors = humans_indoors, humans_in_bed = humans_in_bed)
    activity_vec <- def_activity_patterns(patterns_vec)
    model_params_vec <- build_model_obj(mosquito_model_params, host_params, activity_vec, total_pop = 2000)
    intervention_vec <- def_interventions_effects(intervention_list = intervention_obj_examples,
                                                  model_p = model_params_vec, num_ip_points = 100)
    impacts_vec <- calculate_impact(intervention_vec, coverage_vec=c(seq(0, 1, by = 0.01)),
                                    model_params_vec, Nv0= 10000, num_ip_points = 100)
    return(impacts_vec)
}

# Define the LLIN intervention.
LLIN_intervention <- intervention_obj_examples$LLINs_example
LLIN_list <- list(LLIN_intervention = LLIN_intervention)

# Calculate intervention effects.
LLIN_effects_gambiae <- def_interventions_effects(LLIN_list, my_default_model_gambiae, 100)
LLIN_effects_stephensi <- def_interventions_effects(LLIN_list, my_default_model_stephensi, 100)

# Calculate LLIN intervention impact with variance.
LLIN_impact_gambiae <- calculate_impact_var(mosquito_species = "Anopheles gambiae",
                                            activity_patterns = activity_p_gambiae,
                                            interventions = LLIN_list,
                                            n_sample_points = 100,
                                            plot_result = FALSE)
LLIN_impact_gambiae$`Mosquito Species` <- "Anopheles gambiae"
LLIN_impact_stephensi <- calculate_impact_var(mosquito_species = "Anopheles stephensi",
                                              activity_patterns = activity_p_stephensi,
                                              interventions = LLIN_list,
                                              n_sample_points = 100,
                                              plot_result = FALSE)
LLIN_impact_stephensi$`Mosquito Species` = "Anopheles stephensi"
LLIN_impact_df <- rbind.data.frame(LLIN_impact_gambiae, LLIN_impact_stephensi)
LLIN_impact_df$`Mosquito Species` <- factor(LLIN_impact_df$`Mosquito Species`,
                                            levels = c("Anopheles gambiae", "Anopheles stephensi"))

# Entomology
entomology_xml_gambiae <- get_OM_ento_snippet(vec_p_gambiae, hosts_p_gambiae)
print(entomology_xml_gambiae)
entomology_xml_stephensi <- get_OM_ento_snippet(vec_p_stephensi, hosts_p_stephensi)
print(entomology_xml_stephensi)

# GVI
GVI_impacts_gambiae <- calculate_impact(interventions_vec = LLIN_effects_gambiae,
                                        coverage_vec = c(seq(0, 0.9, by = 0.1), 0.95, 0.99),
                                        model_p = my_default_model_gambiae,
                                        Nv0 = vec_pop, num_ip_points = 100)
GVI_snippets_gambiae <- get_OM_GVI_snippet("Anopheles gambiae", GVI_impacts_gambiae$interventions_vec$LLIN_intervention,
                                           100, plot_f = TRUE)
GVI_impacts_stephensi <- calculate_impact(interventions_vec = LLIN_effects_stephensi,
                                          coverage_vec = c(seq(0, 0.9, by = 0.1), 0.95, 0.99),
                                          model_p = my_default_model_stephensi,
                                          Nv0 = vec_pop, num_ip_points = 100)
GVI_snippets_stephensi <- get_OM_GVI_snippet("Anopheles stephensi", GVI_impacts_stephensi$interventions_vec$LLIN_intervention,
                                             100, plot_f = TRUE)

# Plot intervention effects for Anopheles gambiae.
plot_df_effects_gambiae <- cbind.data.frame(LLIN_effects_gambiae$LLIN_intervention$effects$alphai_decay,
                                            LLIN_effects_gambiae$LLIN_intervention$effects$PBi_decay,
                                            LLIN_effects_gambiae$LLIN_intervention$effects$PCi_decay,
                                            "Anopheles gambiae",
                                            c(1:length(LLIN_effects_gambiae$LLIN_intervention$effects$alphai_decay)))
colnames(plot_df_effects_gambiae) <- c("Deterrency", "Pre-Prandial \nKilling Effect",
                                       "Post-Prandial \nKilling Effect", "Mosquito Species", "Time Point")
plot_df_effects_gambiae_melted <- melt(plot_df_effects_gambiae, id.vars = c("Time Point", "Mosquito Species"))

# Plot intervention effects for Anopheles stephensi.
plot_df_effects_stephensi <- cbind.data.frame(LLIN_effects_stephensi$LLIN_intervention$effects$alphai_decay,
                                              LLIN_effects_stephensi$LLIN_intervention$effects$PBi_decay,
                                              LLIN_effects_stephensi$LLIN_intervention$effects$PCi_decay,
                                              "Anopheles stephensi",
                                              c(1:length(LLIN_effects_stephensi$LLIN_intervention$effects$alphai_decay)))
colnames(plot_df_effects_stephensi) <- c("Deterrency", "Pre-Prandial \nKilling Effect",
                                         "Post-Prandial \nKilling Effect", "Mosquito Species", "Time Point")
plot_df_effects_stephensi_melted <- melt(plot_df_effects_stephensi, id.vars = c("Time Point", "Mosquito Species"))

# Plot intervention effects for both species.
plot_df_effects <- rbind.data.frame(plot_df_effects_gambiae_melted, plot_df_effects_stephensi_melted)
plot_df_effects$`Mosquito Species` = factor(plot_df_effects$`Mosquito Species`,
                                            levels = c("Anopheles gambiae", "Anopheles stephensi"))
p_effects <- ggplot(plot_df_effects) +
             geom_line(aes(x = `Time Point`, y = value*100, col = `Mosquito Species`)) +
             facet_wrap(~ variable) + theme_light() + theme_bw(base_size = 16) +
             theme(legend.position = "top") + ylim(c(0, 100)) +
             theme(strip.background = element_rect(colour = "white", fill = "white")) +
             scale_color_manual("Mosquito Species",
                                labels = c(expression(italic("An. gambiae")), expression(italic("An. stephensi"))),
                                values = c("#00BFC4", "#F8766D")) +
             labs(x = "Time Point", y = "Effect (%)")

# Plot the vectorial capacity reduction by species.
p_vc <- ggplot(LLIN_impact_df, aes(x = intervention_coverage, y = intervention_impact*100,
                                   group = `Mosquito Species`, col = `Mosquito Species`, fill = `Mosquito Species`)) +
        theme_light() + theme_linedraw() + theme_bw(base_size = 16) +
        ylim(c(0, 100)) + theme(legend.position = "top") +
        stat_summary(fun.data = mean_sdl, fun.args = list(mult = 2),
                     geom = "ribbon", alpha = 0.5, colour = NA) +
        scale_fill_manual("Mosquito Species",
                          labels = c(expression(italic("An. gambiae")), expression(italic("An. stephensi"))),
                          values = c("#00BFC4", "#F8766D")) +
        scale_color_manual("Mosquito Species",
                           labels = c(expression(italic("An. gambiae")), expression(italic("An. stephensi"))),
                           values = c("#00BFC4", "#F8766D")) +
        labs(x = "Coverage", y = "Mean Reduction in\n Vectorial Capacity (%)")

# Plot.
p_decay <- ggarrange(plotlist = list(p_effects, p_vc), ncol = 1, nrow = 2, labels = c("A", "B"))
p_decay

