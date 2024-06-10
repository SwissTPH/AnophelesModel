###############################
# Script for generating Fig. 4 which presents the decay of intervention effects
# for LLIN, IRS, House screening and a combination of LLIN and IRS
# in PNG and KEN as well as corresponding reduction in vectorial capacity.
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
default_host_params_farauti = default_host_params
default_host_params_farauti$species_name = "Anopheles farauti"

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
model_params_farauti = build_model_obj(farauti_ent_params, default_host_params_farauti, activity_farauti, host_pop)

# Define LLIN intervention
LLIN_intervention = intervention_obj_examples$LLINs_example
IRS_intervention = intervention_obj_examples$IRS_example
Screening_intervention = intervention_obj_examples$Screening_example


intervention_list = list(LLIN_intervention = LLIN_intervention,
                         IRS_intervention = IRS_intervention,
                         Screening_intervention = Screening_intervention)

# Calculate intervention effects
intervention_effects_gambiae = def_interventions_effects(intervention_list, model_params_gambiae, 100)
intervention_effects_farauti = def_interventions_effects(intervention_list, model_params_farauti, 100)

# Calculate impact with variance
impact_gambiae = calculate_impact_var(mosquito_species = "Anopheles gambiae",
                                      activity_patterns = activity_gambiae,
                                      interventions = intervention_list,
                                      n_sample_points = 10,
                                      plot_result = FALSE)
impact_gambiae$`Mosquito species` = "Anopheles gambiae"

impact_farauti = calculate_impact_var(mosquito_species = "Anopheles farauti",
                                      host_table = as.data.frame(default_host_params_farauti),
                                      activity_patterns = activity_farauti,
                                      interventions = intervention_list,
                                      n_sample_points = 10,
                                      plot_result = FALSE)
impact_farauti$`Mosquito species` = "Anopheles farauti"

impact_combined_gambiae = calculate_combined_impact_var(mosquito_species = "Anopheles gambiae",
                                                activity_patterns = activity_gambiae,
                                                interventions = intervention_list,
                                                n_sample_points = 10)
impact_combined_gambiae$`Mosquito species` = "Anopheles gambiae"

impact_combined_farauti = calculate_combined_impact_var(mosquito_species = "Anopheles farauti",
                                                        host_table = as.data.frame(default_host_params_farauti),
                                                        activity_patterns = activity_farauti,
                                                        interventions = intervention_list,
                                                        n_sample_points = 10)
impact_combined_farauti$`Mosquito species` = "Anopheles farauti"

impact_df = rbind.data.frame(impact_gambiae, impact_farauti, impact_combined_gambiae, impact_combined_farauti)
impact_df$`Mosquito species` = factor(impact_df$`Mosquito species`, levels = c("Anopheles gambiae", "Anopheles farauti"))
impact_df$intervention_impact = impact_df$intervention_impact*100
impact_df[which(impact_df$intervention_name == "combination"), "intervention_name"] = "IRS + LLINs"
impact_df[which(impact_df$intervention_impact < 0 ), "intervention_impact"] = 0
impact_df$intervention_impact = round(impact_df$intervention_impact, digits = 3)
# plotting the impacts
# plot_impact_var("Anopheles gambiae", impact_gambiae)
# plot_impact_var("Anopheles farauti", impact_farauti)

intervention_colors2 = intervention_colors_fill =
    c("#f768a1", "#31a354", "#3182bd", "#004D40", "#de2d26",
      "#74c476", "#bdbdbd", "#d7b5d8")

intervention_colors = intervention_colors_fill =
    c("#D81B60", "#1E88E5", "#D4A92A", "#004D40", "#de2d26",
      "#74c476", "#bdbdbd", "#d7b5d8")

p = ggplot(impact_df, aes(x=intervention_coverage, y=intervention_impact,
                          group = interaction(intervention_name, `Mosquito species`),
                          col = intervention_name,
                          fill = intervention_name,
                          lty = `Mosquito species`)) +
    theme_light() + theme_linedraw() + theme_bw(base_size=14) +
    ylim(c(0, 100)) + theme(legend.position = "bottom", legend.direction = "vertical") +
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 2),
                 geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(fun = mean, geom = "line", size = 0.5) +
    scale_color_manual("Interventions", values = intervention_colors) +
    scale_fill_manual("Interventions", values = intervention_colors) +
    scale_linetype_manual("Mosquito species",
                          labels = c(expression(italic("Anopheles gambiae")),
                                     expression(italic("Anopheles farauti"))),
                          values = c("solid", "dashed")) +
    guides(col="none") +
    guides(linetype = guide_legend(nrow = 2, byrow = TRUE)) +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    labs(title = "",
         x="Coverage", y="Mean reduction in\nvectorial capacity",
         fill = "")


# Ratio of impacts
# res_df2 = NULL
# for(i in seq(0.1, 1, 0.1)) {
#   for(j in seq(0.1, 1, 0.1)) {
#       # Calculate impact
#       impact_vc_farauti = calculate_combined_impact(combination_name = "combination",
#                                             intervention1 = intervention_effects_gambiae[[1]],
#                                             intervention2 = intervention_effects_gambiae[[2]],
#                                             cov_intervention1 = i,
#                                             cov_intervention2 = j,
#                                             N_vec = 10000)
#       impact_vc_gambiae = calculate_combined_impact(combination_name = "combination",
#                                                     intervention1 = intervention_effects_farauti[[1]],
#                                                     intervention2 = intervention_effects_farauti[[2]],
#                                                     cov_intervention1 = i,
#                                                     cov_intervention2 = j,
#                                                     N_vec = 1000)
#     res_df2 = rbind.data.frame(res_df2, cbind(i, j, impact_vc_farauti/impact_vc_gambiae))
#   }
# }
# ggplot(res_df2, aes(i, j, fill= V3)) +
#   geom_tile()
#
# p_fig3 = ggarrange(plotlist = list(p_effects, p_vc), ncol = 1, nrow = 2, labels = c("A", "B"))
ggsave("~/paper_AnophelesModel/Figures/Fig4.pdf", width = 10, height = 7)
#

