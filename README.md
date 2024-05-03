# [AnophelesModel](https://swisstph.github.io/AnophelesModel/) 
AnophelesModel is an R package to quantify the impact of vector control interventions according to mosquito species characteristics. It can be used to parameterize a [dynamical model of the mosquito feeding cycle](https://www.tandfonline.com/doi/full/10.1080/17513750701769857) using data about mosquito bionomics (entomological characteristics) and biting patterns, as well as human activity and intervention effects. The different types of data have been extracted from field studies and are included in the package. The model infers the species-specific impact of various vector control interventions on the vectorial capacity. The package can be used to compare the impact of interventions for different mosquito species and to generate geographic-specific parameterizations for the entomological and vector control components of more complex models of malaria transmission dynamics.

This package is associated with the following manuscript:
M. Golumbeanu, O. Briët, C. Champagne, J. Lemant, M. Winkel, B. Zogo, M. Gerhards, M. Sinka, N. Chitnis, M. Penny, E. Pothin, T. Smith. "**AnophelesModel: An R package to interface mosquito bionomics, human exposure and intervention effects with models of malaria intervention impact**" available as a preprint at [https://www.biorxiv.org/content/10.1101/2023.10.17.562838v1](https://www.biorxiv.org/content/10.1101/2023.10.17.562838v1).

The scripts and data used for generating figures 2-4 in the manuscript are provided at https://github.com/SwissTPH/AnophelesModel/tree/main/extdata (scripts example_fig2.R, etc.).

To install the package:
```{r}
devtools::install_github("SwissTPH/AnophelesModel", build_vignettes = TRUE)
```
IMPORTANT: To be able to build the vignette during package installation, you need to have the following packages installed: DiagrammeR, ggpubr, and Hmisc.

A documentation describing the package use cases and functions is available [here](https://swisstph.github.io/AnophelesModel/articles/AnophelesModel.html) and can also be accessed using the following R command:
```{r}
browseVignettes(AnophelesModel)
```
