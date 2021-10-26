#########plot_impact_species#########
#' @title produce various impact figures per species
#'
#' @description \code{plot_impact_species}
#' @param mosquito_species string corresponding to the name of the mosquito
#' species that the parameters should be loaded. Argument is case sensitive and
#' Must be one of the species provided with the package: Gambiae, Albimanus, ...
#'
#' @return plot object
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @references TO DO
#'
#' @import ggplot2
#'
#' @examples
#'
#' @export
#'
plot_impact_species = function(impacts_obj, plot_type) {
    # Plot reduction in vectorial capacity for all interventions and behaviors
    if (plot_type == "VC_red") {
        to_plot_df = NULL
        for(interv_imp in impacts_obj$interventions_vec) {
            intervention_name = interv_imp$description
            intervention_coverage = interv_imp$coverages
            intervention_impact = round(interv_imp$effects$avg_impact,
                                        digits = 3)
            plot_tab = cbind.data.frame(intervention_name,
                                        intervention_coverage,
                                        intervention_impact)
            to_plot_df = rbind.data.frame(to_plot_df, plot_tab)
        }
        p = plot_VC_red(to_plot_df, impacts_obj$vec_p$species_name)
    }
    return(p)
}


#########plot_impact_var#########
#' @title produce various impact figures per species
#'
#' @description \code{plot_impact_var}
#' @param species_name name of the mosquito species
#' @param plot_df plot data frame
#'
#' @return plot object
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @import ggplot2
#'
#' @examples
#'
#' @export
#'
plot_impact_var = function(species_name, plot_df) {
    intervention_colors = intervention_colors_fill =
        c("#3182bd", "#31a354", "#de2d26", "#c51b8a",
          "#74c476", "#bdbdbd", "#d7b5d8")
    plot_df$intervention_impact = plot_df$intervention_impact*100
    p = ggplot(plot_df, aes(x=intervention_coverage, y=intervention_impact,
                            group = factor(intervention_name),
                            col = factor(intervention_name),
                            fill = factor(intervention_name))) +
        theme_light() + theme_linedraw() + theme_bw(base_size=9) +
        ylim(c(0, 100)) + theme(legend.position = "bottom") +
        stat_summary(fun.data = mean_sdl, fun.args = list(mult = 2),
                     geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun = mean, geom = "line", lwd = 0.5) +
        scale_color_manual(values = intervention_colors) +
        scale_fill_manual(values = intervention_colors) +
        guides(col="none") +
        guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
        labs(title = paste(species_name),
             x="Coverage", y="Mean reduction in\nvectorial capacity",
             fill="")
    return(p)
}
