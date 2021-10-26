#################################
# internal_plot_functions
#
# Contains various plotting functions.
#
# created on 06.03.2019 by Monica Golumbeanu
# monica.golumbeanu@unibas.ch
################################

# For each intervention, plot the proportion of vectorial capacity reduction
# for the same species.
#
# INPUTS:
#   plot_df = data frame containing 4 columns corresponding to intervention name,
#               coverage, % reduction in VC, and people behavior before bedtime
# OUTPUT:
#   ggplot object
plot_VC_red = function(plot_df, species_name) {
    intervention_colors = c("#3182bd", "#31a354", "#de2d26", "#c51b8a",
                            "#74c476", "#bdbdbd", "#d7b5d8")
    p = ggplot(plot_df) +
        theme_light() + theme_linedraw() + theme_bw(base_size = 9) +
        theme(legend.position = "bottom") +
        ylim(c(0, 100)) +
        geom_line(aes(x=intervention_coverage, y=intervention_impact*100,
                      col=factor(intervention_name)), size=1) +
        scale_color_manual(values = intervention_colors) +
        guides(color = guide_legend(nrow = 2,byrow = TRUE)) +
        labs(title = paste(species_name), fill="",
             x="Coverage", y="Mean reduction in \nvectorial capacity (%)",
             col="")

    return(p)
}


# Plot the fits to the raw data with given decay functions
plot_fits = function(data_tab, decay_tab, plot_title) {
    lines_colors = c("black", "#c51b8a", "#2c7fb8",
                     "#31a354", "#7fcdbb",  "#feb24c", "red",
                     "blue", "green", "black")
    p = ggplot() +
        theme_light() + theme_linedraw() +
        geom_point(data = data_tab, aes(x = time, y=value),
                   col="grey", size = 1.5) +
        geom_line(data = decay_tab, aes(x = time, y=value,
                                    col = factor(decay_function)), size=0.5) +
        theme_bw(base_size = 9) +
        scale_color_manual(values = lines_colors) +
        guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
        theme(legend.position = "bottom") +
        labs(title = plot_title, fill="",
             x="Time", y="Value",
             col="", linetype="")
    return(p)
}
