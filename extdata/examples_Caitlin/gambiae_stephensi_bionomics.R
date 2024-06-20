#########################################
#### GAMBIAE AND STEPHENSI BIONOMICS ####
#########################################


#### SETUP ####


## Clear working space.
rm(list = ls())

## Load packages.
library(ggplot2)
library(AnophelesModel)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)
library(ggpubr)
library(reporter)

# Build dataframe of bionomics.
df_bionomics <- vec_ent_param[, c("species_name", "M", "A0", "Chi", "endophagy")]
df_bionomics <- df_bionomics %>% filter(grepl(pattern = "Anopheles", x = species_name))
df_bionomics <- melt(df_bionomics, id.vars = c("species_name"))
df_bionomics$variable <- factor(df_bionomics$variable)
df_bionomics$variable <- recode_factor(df_bionomics$variable, M = "Parous Rate",
                                       A0 = "Sac Rate", Chi = "Human Blood\nIndex",
                                       endophagy = "Endophagy")


#### MOSQUITO BIONOMICS ####


# Filter for Anopheles gambiae and Anopheles stephensi bionomics.
sf_selected_bio <- df_bionomics %>% filter(species_name %in% c("Anopheles gambiae", "Anopheles stephensi"))
sf_selected_bio$species_name <- factor(sf_selected_bio$species_name,
                                       levels = c("Anopheles gambiae", "Anopheles stephensi"))


# Histograms of parous rates, human blood index, sac rate, and endophagy.
p_bio <- ggplot(df_bionomics, aes(x = value)) + geom_histogram(bins = 20) +
    geom_segment(data = sf_selected_bio, aes(x = value, y = 30, xend = value,
                                             yend = 20, color = species_name),
                 arrow = arrow(length = unit(0.15, "cm")), linewidth = 1) +
    facet_wrap(~ variable, ncol = 4) +
    theme_bw(base_size = 16) +
    labs( x = "Value", y = "Number of \nAnopheles Species", color = "Anopheles Species") +
    scale_color_manual(values = c("Anopheles stephensi" = "hotpink", "Anopheles gambiae" = "royalblue"),
                       labels = c(expression(italic("An. gambiae")), expression(italic("An. stephensi")))) +
    theme(strip.background = element_rect(colour = "white", fill = "white")) +
    theme(legend.text.align = 0)
p_bio


#### MOSQUITO BIONOMICS WITH STANDARD DEVIATIONS ####


# Create a dataframe with Anopheles gambiae and Anopheles stephensi bionomics.
# Include their confidence intervals (± 2 standard deviations).
df_bionomics_sd <- data.frame(species_name = c("Anopheles gambiae", "Anopheles gambiae", "Anopheles gambiae",
                                               "Anopheles stephensi", "Anopheles stephensi", "Anopheles stephensi"),
                              variable = c("Parous Rate", "Sac Rate", "Endophagy",
                                           "Parous Rate", "Sac Rate", "Endophagy"),
                              value = c(0.6134499, 0.6419328, 0.5604133,
                                        0.5577161, 0.4951913, 0.399699),
                              min = c(0.605811646, 0.49632422, 0.55030012,
                                      0.4932297, 0.39127398, 0.31509142),
                              max = c(0.621088154, 0.78754138, 0.57052648,
                                      0.6222025, 0.59910862, 0.48430658))

# Plot the mosquito bionomics with their corresponding standard deviations.
p_variance <- ggplot(df_bionomics_sd, aes(x = variable, y = value, color = species_name)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = min, ymax = max), width = 0.2, position = position_dodge(width = 0.5)) +
    theme_bw(base_size = 16) +
    labs(x = "Bionomics Variable", y = "Value", color = "Anopheles Species") +
    scale_color_manual(values = c("Anopheles stephensi" = "hotpink", "Anopheles gambiae" = "royalblue"),
                       labels = c(expression(italic("An. gambiae")), expression(italic("An. stephensi")))) +
    theme(strip.background = element_rect(colour = "white", fill = "white")) +
    theme(legend.text.align = 0) +
    facet_wrap(~ variable, scales = "free_y") +
    theme(axis.text.x = element_blank())
p_variance

