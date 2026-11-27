# [AnophelesModel](https://swisstph.github.io/AnophelesModel/)

AnophelesModel is an R package to quantify the impact of vector control
interventions according to mosquito species characteristics. It can be
used to parameterize a [dynamical model of the mosquito feeding
cycle](https://www.tandfonline.com/doi/full/10.1080/17513750701769857)
using data about mosquito bionomics (entomological characteristics) and
biting patterns, as well as human activity and intervention effects. The
different types of data have been extracted from field studies and are
included in the package. The model infers the species-specific impact of
various vector control interventions on the vectorial capacity. The
package can be used to compare the impact of interventions for different
mosquito species and to generate geographic-specific parameterizations
for the entomological and vector control components of more complex
models of malaria transmission dynamics.

Version 2 is a refactoring of version 1 and is not backward compatible
with it: see
[NEWS.md](https://swisstph.github.io/AnophelesModel/NEWS.md) for the
differences. Version 1 is associated with the following manuscript: M.
Golumbeanu, O. Briët, C. Champagne, J. Lemant, M. Winkel, B. Zogo, M.
Gerhards, M. Sinka, N. Chitnis, M. Penny, E. Pothin, T. Smith.
“**AnophelesModel: An R package to interface mosquito bionomics, human
exposure and intervention effects with models of malaria intervention
impact**”
[2024](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1011609).

The scripts and data used for generating figures 2-4 in the manuscript
are provided in
[extdata](https://swisstph.github.io/AnophelesModel/extdata) (scripts
`example_fig2.R`, etc.). They use version 1 of the package (release
v1.1.0).

To install the package:
`{r} devtools::install_github("SwissTPH/AnophelesModel")`

To install version 1:
`{r} devtools::install_github("SwissTPH/AnophelesModel@v1.1.0")`
