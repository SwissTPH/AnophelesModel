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
df_bionomics = vec_ent_param[, c("species_name", "M", "A0", "Chi", "endophagy")]
df_bionomics = df_bionomics %>% filter(grepl(pattern = "Anopheles", x = species_name))
df_bionomics = melt(df_bionomics, id.vars = c("species_name"))

df_bionomics$variable = factor(df_bionomics$variable)
df_bionomics$variable = recode_factor(df_bionomics$variable, M = "Parous Rate",
                                      A0 = "Sac Rate",
                                      Chi = "Human Blood\nIndex",
                                      endophagy = "Endophagy")


#### MOSQUITO BIONOMICS ####


# Filter for Anopheles gambiae and Anopheles stephensi bionomics.
sf_selected_bio = df_bionomics %>% filter(species_name %in% c("Anopheles gambiae", "Anopheles stephensi"))
sf_selected_bio$species_name = factor(sf_selected_bio$species_name,
                                      levels = c("Anopheles farauti", "Anopheles stephensi"))

# Histograms of parous rates, human blood index, sac rate, and endophagy.
p_bio = ggplot(df_bionomics, aes(x = value)) + geom_histogram(bins = 20) +
    geom_segment(data = sf_selected_bio, aes(x = value, y = 30, xend = value,
                                             yend = 20, color = species_name),
                 arrow = arrow(length = unit(0.15, "cm")), size = 1) +
    facet_wrap(~ variable, ncol = 4) +
    theme_bw(base_size = 16) +
    labs( x = "Value", y = "Number of \nAnopheles Species") +
    scale_color_discrete("Mosquito\nSpecies",
                         labels = c(expression(italic("An. stephensi")), expression(italic("An. gambiae")))) +
    theme(strip.background = element_rect(colour = "white", fill = "white")) +
    theme(legend.text.align = 0)
p_bio


