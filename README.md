# [AnophelesModel](https://swisstph.github.io/AnophelesModel/) 
AnophelesModel is an R package to quantify the impact of vector control interventions according to mosquito species characteristics. It can be used to parameterize a [dynamical model of the mosquito feeding cycle](https://www.tandfonline.com/doi/full/10.1080/17513750701769857) using data about mosquito bionomics (entomological characteristics) and biting patterns, as well as human activity and intervention effects. The different types of data have been extracted from field studies and are included in the package. The model infers the species-specific impact of various vector control interventions on the vectorial capacity. The package can be used to compare the impact of interventions for different mosquito species and to generate geographic-specific parameterizations for the entomological and vector control components of more complex models of malaria transmission dynamics.

This package is associated with the following manuscript:
M. Golumbeanu, O. Briët, C. Champagne, J. Lemant, M. Winkel, B. Zogo, M. Gerhards, M. Sinka, N. Chitnis, M. Penny, E. Pothin, T. Smith. "**AnophelesModel: An R package to quantify the effect of vector bionomics on the impact of vector control interventions against malaria transmitted by *Anopheles* mosquitoes**".

The scripts and data used for generating figures 2-4 in the manuscript are provided at https://github.com/SwissTPH/AnophelesModel/tree/main/extdata (scripts example_fig2.R, etc.).

To install the package:
```{r}
devtools::install_github("SwissTPH/AnophelesModel", build_vignettes = TRUE)
```
A documentation describing the package use cases and functions is available [here](https://swisstph.github.io/AnophelesModel/articles/AnophelesModel.html) and can also be accessed using the following R command:
```{r}
browseVignettes(AnophelesModel)
```
