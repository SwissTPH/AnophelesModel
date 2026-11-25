#########plot_impact#########
#' @title Plot the reduction in vectorial capacity against coverage
#'
#' @description Plots the average reduction in vectorial capacity
#' (\code{avg_impact}) against coverage, one line per intervention, from the
#' output of \code{\link{calculate_impact}} or \code{\link{run_interventions}}.
#' With uncertainty draws (a \code{draw} column), the line is the median over
#' the draws and the band their 95\% interval.
#'
#' @param impacts Output of \code{calculate_impact()} or
#'   \code{run_interventions()} (the table, or the list returned with
#'   \code{keep_time = TRUE}). Lines are labelled by the \code{name} column
#'   (give each intervention a distinct name); without it, by the
#'   \code{intervention} column.
#' @param title Optional plot title, e.g. the mosquito species.
#'
#' @return A ggplot object.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @import ggplot2
#'
#' @examples
#' model_p = build_model_obj(def_vector_params(), def_host_params())
#' itn = build_intervention(
#'     efficacy = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
#'     halflife_attrition = 2 * 365, shape_attrition = 2,
#'     duration_years = 1, model_p = model_p, name = "ITN")
#' plot_impact(calculate_impact(itn, seq(0, 1, by = 0.2), model_p))
#'
#' @export
#'
plot_impact = function(impacts, title = NULL) {
    if (is.list(impacts) && !is.data.frame(impacts) && !is.null(impacts$impact))
        impacts = impacts$impact
    if (!is.data.frame(impacts) ||
        !all(c("coverage", "avg_impact") %in% names(impacts)))
        stop("impacts must be the output of calculate_impact() or run_interventions()")
    label = if (!is.null(impacts$name)) impacts$name
            else if (!is.null(impacts$intervention)) impacts$intervention
            else stop("impacts needs a name (or intervention) column to label the lines")

    df = data.frame(name = as.character(label), coverage = impacts$coverage,
                    impact = 100 * impacts$avg_impact, stringsAsFactors = FALSE)
    draws = !is.null(impacts$draw)
    if (draws) {
        # Quantiles over the draws, all computed from the per-draw values
        q = function(p) stats::aggregate(impact ~ name + coverage, all_draws,
                                         stats::quantile, probs = p)
        all_draws = df
        df    = q(0.5)
        df$lo = q(0.025)$impact
        df$hi = q(0.975)$impact
    }
    df = df[order(df$name, df$coverage), ]

    p = ggplot(df, aes(x = .data$coverage, y = .data$impact,
                       colour = .data$name, group = .data$name)) +
        theme_bw(base_size = 9) + theme(legend.position = "bottom") +
        labs(title = title, x = "Coverage",
             y = "Mean reduction in\nvectorial capacity (%)", colour = "", fill = "")
    if (draws)
        p = p + geom_ribbon(aes(ymin = .data$lo, ymax = .data$hi, fill = .data$name),
                            alpha = 0.3, colour = NA)
    p + geom_line(linewidth = 1)
}
