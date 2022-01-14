# [AnophelesModel](https://swisstph.github.io/AnophelesModel/) 
An R package to quantify the impact of vector control interventions according to mosquito species characteristics 

The AnophelesModel package can be used to parameterize a [dynamical model of the mosquito feeding cycle](https://www.tandfonline.com/doi/full/10.1080/17513750701769857) using data about mosquito bionomics (entomological characteristics) and biting patterns, as well as human activity and intervention effects. The different types of data have been extracted from field studies and are included in the package. The model infers the species-specific impact of various vector control interventions on the vectorial capacity. The package can be used to compare the impact of interventions for different mosquito species and to generate parameterizations for the entomological and vector control components of more complex models of malaria transmission dynamics.

This package is associated with the following manuscript in preparation:
M. Golumbeanu, O. Briët, C. Champagne, J. Lemant, B. Zogo, M. Gerhards, M. Sinka, N. Chitnis, M. Penny, E. Pothin, T. Smith. "**AnophelesModel: An R package to quantify the effect of vector bionomics on the impact of vector control interventions against malaria transmitted by *Anopheles* mosquitoes**".

To install the package:
```{r}
devtools::install_github("SwissTPH/AnophelesModel", build_vignettes = TRUE)
```
A documentation describing the package use cases and functions is available [here](https://swisstph.github.io/AnophelesModel/) and can also be accessed using the following R command:
```{r}
browseVignettes(AnophelesModel)
```
