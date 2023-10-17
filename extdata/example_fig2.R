###############################
# Script for generating Fig. 2 which describes the heterogeneity of bionomics,
# mosquito biting and intervention effects and emphasizes the need of a
# way to incorporate those when estimating the effect of vector control
#
# monica.golumbeanu@unibas.ch
# 18.08.2022
##############################
library(ggplot2)
library(AnophelesModel)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
library(ggpubr)
library(reporter)

plot_activities_human = function(country, id_tab, act_file) {

    # Kenya values from the package
    sf_selected_activity1 = activity_patterns %>%
        filter(species  == "Homo sapiens" &
                   country == country &
                   id %in% id_tab)
    sf_selected_activity1$hour = str_sub(sf_selected_activity1$hour, 1, str_locate(sf_selected_activity1$hour, "_")[, 2]-1)
    sf_selected_activity1$hour = str_replace(sf_selected_activity1$hour, "\\.", ":")
    sf_selected_activity1$hour = factor(sf_selected_activity1$hour, levels = unique(sf_selected_activity1$hour))
    sf_selected_activity1$id = NULL
    a = sf_selected_activity1 %>%
        pivot_wider(names_from = "sampling", values_from = "value")

    # PNG values from file
    sf_selected_activity2 = read.csv(act_file)
    sf_selected_activity2$hour = factor(sf_selected_activity2$hour, levels = unique(sf_selected_activity2$hour))
    sf_selected_activity2$id = NULL
    b = sf_selected_activity2 %>%
        pivot_wider(names_from = "sampling", values_from = "value")

    # Add location and width information
    cc = rbind.data.frame(a, b) %>% pivot_longer(names_to = "sampling", cols = c("IND", "BED"))
    cc$Location = paste0(cc$country, ", ", cc$sampling)
    cc$Location = str_replace(cc$Location, "Papua New Guineea", "PNG")
    cc$Location = str_replace(cc$Location, "BED", "in bed")
    cc$Location = str_replace(cc$Location, "IND", "indoors")
    cc[which(cc$sampling == "BED"), "width"] = 0.4
    cc[which(cc$sampling == "IND"), "width"] = 0.8

    # Only plot up to 5am to match the biting patterns
    cc = cc %>% filter(!(hour %in% c("16:00", "17:00", "06:00", "07:00")))

    # Plot human activity patterns in both countries
    ggplot(cc, aes(x = hour, y = value, fill = Location, group = country, width = width)) +
        theme_bw(base_size = 16) +
        labs( x = "Hour", y = "% humans") +
        theme(axis.text.x = element_text(angle = 80, hjust=1)) +
        geom_col(position = "dodge") + theme(legend.position = "top") +
        guides(fill=guide_legend(ncol=2)) +
        scale_fill_manual(values = c("#c51b8a", "#fbb4b9", "#74c476", "#c2e699"))
}

# Function to plot mosquito and human activity patterns
plot_activity_mosq = function(mosquito_species, country, id_tab, y_lab, y_max = NULL) {
    # Activity patterns
    sf_selected_activity = activity_patterns %>%
        filter(species  == mosquito_species &
                   country == country &
                   id %in% id_tab)
    sf_selected_activity$hour = str_sub(sf_selected_activity$hour, 1, str_locate(sf_selected_activity$hour, "_")[, 2]-1)
    sf_selected_activity$hour = str_replace(sf_selected_activity$hour, "\\.", ":")
    sf_selected_activity$hour = factor(sf_selected_activity$hour, levels = unique(sf_selected_activity$hour))


    # Plot the biting rates
    p_biting = ggplot() +
        theme_bw(base_size = 16) +
        labs( x = "Hour", y = y_lab) +
        theme(axis.text.x = element_text(angle = 80, hjust=1))

    if(y_lab == "% humans") {
        p_biting = p_biting +
            geom_bar(data = sf_selected_activity, mapping = aes(x = hour, y = value*100, fill = sampling),
                     stat = "identity", position = "dodge") +
            scale_fill_manual("Location", values = c("#fdd49e", "#fc8d59"), labels = c("In bed", "Indoors"))
    } else {
        if (mosquito_species == "Anopheles gambiae")
            mosquito_title = "An. gambiae"
        if (mosquito_species == "Anopheles farauti")
            mosquito_title = "An. farauti"
        sf_selected_activity = sf_selected_activity %>% filter(!(hour %in% c("16:00", "17:00", "06:00", "07:00")))
        # sf_selected_activity$mosquito_title = sf_selected_activity$species
        # levels(sf_selected_activity$mosquito_title) = c(levels(sf_selected_activity$mosquito_title), "An. gambiae", "An. farauti")
        # sf_selected_activity[which(sf_selected_activity$mosquito_title == "Anopheles gambiae"), "mosquito_title"] = "An. gambiae"
        # sf_selected_activity[which(sf_selected_activity$mosquito_title == "Anopheles farauti"), "mosquito_title"] = "An. farauti"
        p_biting = p_biting +
            geom_point(data = sf_selected_activity, mapping = aes(x = hour, y = value, color = sampling, group = sampling)) +
            geom_line(data = sf_selected_activity, mapping = aes(x = hour, y = value, color = sampling, group = sampling)) +
            geom_rect(data = sf_selected_activity, mapping = aes(xmin = "20:00", xmax = "05:00", ymin = min(value, na.rm = TRUE),
                                                                 ymax = max(c(value, y_max), na.rm = TRUE)), alpha=0.01) +
            scale_color_manual("Location", values = c("#41b6c4", "#225ea8"), labels = c("Indoors", "Outdoors")) +
            ggtitle(mosquito_title) +
            theme(plot.title = element_text(face = "italic")) +
            theme(plot.title = element_text(size = 14))
        if(!is.null(y_max)) {
            p_biting = p_biting + ylim(0, y_max)
        }
    }

    return(p_biting)
}

#### Plot bionomics

# Build data frame bionomics
df_bionomics = vec_ent_param[, c("species_name", "M", "A0", "Chi", "endophagy")]
df_bionomics = df_bionomics %>% filter(grepl(pattern = "Anopheles", x = species_name))
df_bionomics = melt(df_bionomics, id.vars = c("species_name"))

df_bionomics$variable = factor(df_bionomics$variable)
df_bionomics$variable = recode_factor(df_bionomics$variable, M = "Parous rate",
                                      A0 = "Sac rate",
                                      Chi = "Human blood\nindex",
                                      endophagy = "Endophagy")
# Selected species bionomics
sf_selected_bio = df_bionomics %>% filter(species_name %in% c("Anopheles gambiae", "Anopheles farauti"))
sf_selected_bio$species_name = factor(sf_selected_bio$species_name,
                                      levels = c("Anopheles farauti", "Anopheles gambiae"))

# Histograms of parous rates, human blood index, sac rate and endophagy
p_bio = ggplot(df_bionomics, aes(x = value)) + geom_histogram(bins = 20) +
    geom_segment(data = sf_selected_bio, aes(x = value, y = 30, xend = value,
                                             yend = 20, color = species_name),
                 arrow = arrow(length = unit(0.15, "cm")), size = 1) +
    facet_wrap(~ variable, ncol = 4) +
    theme_bw(base_size = 16) +
    labs( x = "Value", y = "Number of \nAnopheles species") +
    scale_color_discrete("Mosquito\nspecies",
                         labels = c(expression(italic("An. farauti")), expression(italic("An. gambiae")))) +
    theme(strip.background = element_rect(colour="white", fill="white")) +
    theme(legend.text.align = 0)

#### Plot biting rhythms

# For selecting the entries
# sf_selected_biting = activity_patterns %>% filter(species %in% c(mosquito_species) & country %in% c("Kenya"))

# Plotting mosquito biting patterns
p_act_Kenya = plot_activity_mosq("Anopheles gambiae", "Kenya", c(65, 196), "Mosquito biting rate", y_max = 0.21)
p_act_PNG = plot_activity_mosq("Anopheles farauti", "PNG", c(312, 325), "Mosquito biting rate", y_max = NULL)
p_act_mosquitoes = ggarrange(p_act_Kenya, p_act_PNG, ncol = 2, nrow = 1, common.legend = TRUE, legend = "top")

# Plotting human activity
p_act_humans = plot_activities_human("Kenya", c(2, 24),
                                     "~/GitRepos/anophelesmodel/paper_scripts/PNG_human_patterns.csv")
p_act = ggarrange(p_act_mosquitoes, p_act_humans, labels = c("B", "C"), ncol = 2, nrow = 1, widths = c(2, 1.6), legend = "top")

#### Plot net properties for a net brand and several countries
nets_prop = interventions_param$LLINs_params$durability_estim
nets_prop_PermaNet = nets_prop %>% filter(Net_Type == "PermaNet 2.0")
nets_prop_PermaNet = nets_prop_PermaNet %>% filter(Parameter != "SD of log transformed holed area")
nets_prop_PermaNet$L95 = pmax(nets_prop_PermaNet$L95, 0)
nets_prop_PermaNet = nets_prop_PermaNet %>% filter(Semester < 7)
nets_prop_PermaNet[which(nets_prop_PermaNet$Parameter == "Mean of log transformed holed area"), "Title"] = "Mean LLIN log holed area"
nets_prop_PermaNet[which(nets_prop_PermaNet$Parameter == "Survival"), "Title"] = "LLIN survival"

p_net_decay = ggplot(nets_prop_PermaNet, aes(x = Semester, y = Value, fill = Country)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
    geom_errorbar(aes(ymin = L95, ymax = U95), width=.2, position = position_dodge(width=0.9)) +
    facet_wrap(~ Title, scales = "free") +
    scale_fill_brewer(palette = "Dark2") +
    theme_bw(base_size = 16) +
    scale_x_continuous(breaks = c(1:6)) +
    labs( x = "Semester", y = "Value") +
    theme(strip.background = element_rect(colour="white", fill="white")) +
    theme(legend.text.align = 0)

# For extracting all the interpolated curves:
# all_net_countries = unique(nets_prop_PermaNet$Country)
# all_net_countries = c("Kenya", "Malawi", "Mozambique", "Senegal", "Zambia")
# all_net_decay = NULL
# for(country in all_net_countries) {
#     print(country)
#     net_decay = get_net_decay(net_type = "PermaNet 2.0", country = country, insecticide_type = "PermaNet 2.0", n_ips = 100, 3)
#     net_decay_melted = melt(net_decay, id.vars = "time")
#     net_decay_melted$country = country
#     all_net_decay = rbind.data.frame(all_net_decay, net_decay_melted)
# }

# net_decay = get_net_decay(net_type = "PermaNet 2.0", country = "Zambia", insecticide_type = "PermaNet 2.0", n_ips = 100, 3)
# p_net_decay = ggplot(net_decay_melted, aes(x = time, y = value)) +
#     geom_line() +
#     facet_wrap(~ variable, scales = "free") +
#     theme_bw(base_size = 16) +
#     labs( x = "Time point", y = "Value") +
#     theme(strip.background = element_rect(colour="white", fill="white")) +
#     theme(legend.text.align = 0)

#### Plot net properties for several net brands and one country
nets_prop = interventions_param$LLINs_params$durability_estim
nets_prop_Kenya = nets_prop %>% filter(Country == "Kenya")
nets_prop_Kenya = nets_prop_Kenya %>% filter(Parameter != "SD of log transformed holed area")
nets_prop_Kenya = nets_prop_Kenya %>% filter(Net_Type != "PermaNet 3.0")
nets_prop_Kenya$L95 = pmax(nets_prop_Kenya$L95, 0)
nets_prop_Kenya = nets_prop_Kenya %>% filter(Semester < 7)
title_area = paste0("Mean LLIN log holed area (cm" %p% supsc("2"), ")")
nets_prop_Kenya[which(nets_prop_Kenya$Parameter == "Mean of log transformed holed area"), "Title"] = title_area
nets_prop_Kenya[which(nets_prop_Kenya$Parameter == "Survival"), "Title"] = "LLIN survival (% remaining nets)"

p_net_decay_country = ggplot(nets_prop_Kenya, aes(x = Semester, y = Value, fill = Net_Type)) +
    geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
    geom_errorbar(aes(ymin = L95, ymax = U95), width=.2, position = position_dodge(width=0.9)) +
    facet_wrap(~ Title, scales = "free") +
    scale_fill_manual(values = c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5")) +
    theme_bw(base_size = 16) +
    scale_x_continuous(breaks = c(1:6)) +
    labs( x = "Semester", y = "Value") +
    theme(strip.background = element_rect(colour="white", fill="white")) +
    theme(legend.text.align = 0) +
    labs(fill = "LLIN type")

p_fig2 = ggarrange(plotlist = list(p_bio, p_act, p_net_decay_country), ncol = 1, nrow = 3, labels = c("A", "B", "D"))

# To modify ouptut folder accordingly
# ggsave("~/paper_AnophelesModel/Figures/Fig2.pdf", width = 11, height = 9)
