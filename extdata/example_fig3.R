###############################
# Script for generating Fig. 3 which presents the decay of intervention effects
# for LLIN in PNG and KEN as well as corresponding reduction in vectorial capacity.
# This script also generates the GVI snippets to be used for parameterising OpenMalaria.
#
# monica.golumbeanu@unibas.ch
# 22.08.2022
##############################

library(ggplot2)
library(AnophelesModel)
library(dplyr)
library(reshape2)
library(ggpubr)

calculate_impact_vec = function(tab_activity, mosquito_model_params, host_params) {
    HBI_vec = tab_activity$HBI
    HBO_vec = tab_activity$HBO
    humans_indoors = tab_activity$humans_indoors
    humans_in_bed = tab_activity$humans_in_bed
    patterns_vec = list(HBI_vec = HBI_vec, HBO_vec = HBO_vec, humans_indoors = humans_indoors, humans_in_bed = humans_in_bed)
    activity_vec = def_activity_patterns(patterns_vec)

    model_params_vec = build_model_obj(mosquito_model_params, host_params, activity_vec, total_pop = 2000)

    intervention_vec = def_interventions_effects(intervention_list = intervention_obj_examples,
                                                         model_p = model_params_vec, num_ip_points= 100)

    impacts_vec = calculate_impact(intervention_vec, coverage_vec=c(seq(0, 1, by = 0.01)),
                                       model_params_vec, Nv0= 10000, num_ip_points= 100)

    return(impacts_vec)
}

# Define host and vector entomological parameters
gambiae_ent_params = def_vector_params(mosquito_species = "Anopheles gambiae")
farauti_ent_params = def_vector_params(mosquito_species = "Anopheles farauti")
default_host_params = def_host_params()

# Define size of the host and mosquito population
host_pop = 2000
vec_pop = 10000

# Define activity patterns
activity_list = NULL
activity_list$HBI = 65
activity_list$HBO = 196
activity_list$humans_indoors = 2
activity_list$humans_in_bed = 24
activity_gambiae = def_activity_patterns(activity_list)

activity_list = NULL
activity_list$HBI =312
activity_list$HBO = 325
sf_selected_activity = read.csv("~/GitRepos/anophelesmodel/paper_scripts/PNG_human_patterns.csv")
# activity_list$humans_indoors = 2
# activity_list$humans_in_bed = 24
activity_list$humans_indoors = sf_selected_activity[which(sf_selected_activity$sampling == "IND"), "value"]
activity_list$humans_in_bed = sf_selected_activity[which(sf_selected_activity$sampling == "BED"), "value"]
activity_farauti = def_activity_patterns(activity_list)
activity_farauti$HBI[which(is.na(activity_farauti$HBI))] = 0
activity_farauti$HBO[which(is.na(activity_farauti$HBO))] = 0

# Parameterize model for both species
model_params_gambiae = build_model_obj(gambiae_ent_params, default_host_params, activity_gambiae, host_pop)
model_params_farauti = build_model_obj(farauti_ent_params, default_host_params, activity_farauti, host_pop)

# Define LLIN intervention
LLIN_intervention = intervention_obj_examples$LLINs_example
LLIN_list = list(LLIN_intervention = LLIN_intervention)

# Calculate intervention effects
intervention_effects_gambiae = def_interventions_effects(LLIN_list, model_params_gambiae, 100)
intervention_effects_farauti = def_interventions_effects(LLIN_list, model_params_farauti, 100)

# Calculate impact with variance
impact_gambiae = calculate_impact_var(mosquito_species = "Anopheles gambiae",
                                      activity_patterns = activity_gambiae,
                                      interventions = LLIN_list,
                                      n_sample_points = 10,
                                      plot_result = FALSE)
impact_gambiae$`Mosquito species` = "Anopheles gambiae"

impact_farauti = calculate_impact_var(mosquito_species = "Anopheles farauti",
                                      activity_patterns = activity_farauti,
                                      interventions = LLIN_list,
                                      n_sample_points = 10,
                                      plot_result = FALSE)
impact_farauti$`Mosquito species` = "Anopheles farauti"
impact_df = rbind.data.frame(impact_gambiae, impact_farauti)
impact_df$`Mosquito species` = factor(impact_df$`Mosquito species`, levels = c("Anopheles gambiae", "Anopheles farauti"))

# SNIPPETS
# Entomology
entomology_xml_gambiae = get_OM_ento_snippet(gambiae_ent_params, default_host_params)
print(entomology_xml_gambiae)
entomology_xml_farauti = get_OM_ento_snippet(farauti_ent_params, default_host_params)
print(entomology_xml_farauti)

# GVI
impacts_gambiae = calculate_impact(interventions_vec = intervention_effects_gambiae,
                                   coverage_vec = c(seq(0, 0.9, by = 0.1), 0.95, 0.99),
                                   model_p = model_params_gambiae, Nv0 = vec_pop, num_ip_points = 100)

GVI_snippets_gambiae = get_OM_GVI_snippet("Anopheles gambiae", impacts_gambiae$interventions_vec$LLIN_intervention,
                                  100, plot_f = TRUE)

impacts_farauti = calculate_impact(interventions_vec = intervention_effects_farauti,
                                   coverage_vec = c(seq(0, 0.9, by = 0.1), 0.95, 0.99),
                                   model_p = model_params_gambiae, Nv0 = vec_pop, num_ip_points = 100)

GVI_snippets_farauti = get_OM_GVI_snippet("Anopheles farauti", impacts_farauti$interventions_vec$LLIN_intervention,
                                  100, plot_f = TRUE)

# PLOTTING
# Plot intervention effects by species
plot_df_effects_gambiae = cbind.data.frame(intervention_effects_gambiae$LLIN_intervention$effects$alphai_decay,
                                           intervention_effects_gambiae$LLIN_intervention$effects$PBi_decay,
                                           intervention_effects_gambiae$LLIN_intervention$effects$PCi_decay,
                                           "Anopheles gambiae", c(1:length(intervention_effects_gambiae$LLIN_intervention$effects$alphai_decay)))
colnames(plot_df_effects_gambiae) = c("Deterrency", "Pre-prandial \nkilling effect", "Post-prandial \nkilling effect", "Mosquito species", "Time point")
plot_df_effects_gambiae_melted = melt(plot_df_effects_gambiae, id.vars = c("Time point", "Mosquito species"))

plot_df_effects_farauti = cbind.data.frame(intervention_effects_farauti$LLIN_intervention$effects$alphai_decay,
                                           intervention_effects_farauti$LLIN_intervention$effects$PBi_decay,
                                           intervention_effects_farauti$LLIN_intervention$effects$PCi_decay,
                                           "Anopheles farauti", c(1:length(intervention_effects_farauti$LLIN_intervention$effects$alphai_decay)))
colnames(plot_df_effects_farauti) = c("Deterrency", "Pre-prandial \nkilling effect", "Post-prandial \nkilling effect", "Mosquito species", "Time point")
plot_df_effects_farauti_melted = melt(plot_df_effects_farauti, id.vars = c("Time point", "Mosquito species"))
plot_df_effects = rbind.data.frame(plot_df_effects_gambiae_melted, plot_df_effects_farauti_melted)
plot_df_effects$`Mosquito species` = factor(plot_df_effects$`Mosquito species`, levels = c("Anopheles gambiae", "Anopheles farauti"))

p_effects = ggplot(plot_df_effects) +
            geom_line(aes(x = `Time point`, y = value*100, col = `Mosquito species`)) +
            facet_wrap(~ variable) +
            theme_light() +
            theme_bw(base_size = 16) +
            theme(legend.position = "top") +
            ylim(c(0, 100)) +
            theme(strip.background = element_rect(colour="white", fill="white")) +
            scale_color_manual("Mosquito species",
                         labels = c(expression(italic("An. gambiae")),
                                    expression(italic("An. farauti"))),
                         values = c("#00BFC4", "#F8766D")) +
            labs(x = "Time point", y = "Effect (%)")

# Plot the vectorial capacity reduction by species
p_vc = ggplot(impact_df, aes(x = intervention_coverage, y = intervention_impact*100,
                                group = `Mosquito species`,
                                col = `Mosquito species`,
                                fill = `Mosquito species`)) +
                theme_light() + theme_linedraw() + theme_bw(base_size = 16) +
                ylim(c(0, 100)) + theme(legend.position = "top") +
                stat_summary(fun.data = mean_sdl, fun.args = list(mult = 2),
                             geom = "ribbon", alpha = 0.5, colour = NA) +
                stat_summary(fun = mean, geom = "line", lwd = 0.5) +
                scale_fill_manual("Mosquito species",
                         labels = c(expression(italic("An. gambiae")),
                                    expression(italic("An. farauti"))),
                         values = c("#00BFC4", "#F8766D")) +
                scale_color_manual("Mosquito species",
                        labels = c(expression(italic("An. gambiae")),
                                   expression(italic("An. farauti"))),
                        values = c("#00BFC4", "#F8766D")) +
                labs(x = "Coverage", y="Mean reduction in\nvectorial capacity (%)")

p_fig3 = ggarrange(plotlist = list(p_effects, p_vc), ncol = 1, nrow = 2, labels = c("A", "B"))
# ggsave("~/paper_AnophelesModel/Figures/Fig3.pdf", width = 8, height = 7)


