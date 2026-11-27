# The main usages of the package

A framework for assessing the impact of malaria vector control
interventions, taking into account species- and/or
geographically-specific mosquito bionomics. Version 2 is a refactoring
of version 1 (see NEWS.md for the differences).

## Overview

The package combines a database of vector bionomics (from the
AnophelesBionomics package,
[`vec_ent_param`](https://swisstph.github.io/AnophelesModel/reference/vec_ent_param.md)),
host parameters, activity patterns and ITN efficacies fitted to
experimental hut trials
([`ITN_parameterisations_Champagne2025`](https://swisstph.github.io/AnophelesModel/reference/ITN_parameterisations_Champagne2025.md))
to parameterise an entomological model of the mosquito oviposition
cycle. The model computes, for a given *Anopheles* taxon, the reduction
in vectorial capacity due to vector control interventions
([`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md),
[`calculate_impact`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md),
[`run_interventions`](https://swisstph.github.io/AnophelesModel/reference/run_interventions.md),
[`calculate_combined_impact`](https://swisstph.github.io/AnophelesModel/reference/calculate_combined_impact.md)),
and can be used to compare interventions across species and settings.
The same parameterisations can be exported to OpenMalaria scenarios
(entomology and vector control; see
[`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md)
and
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md)).

## See also

Useful links:

- <https://github.com/SwissTPH/AnophelesModel>

- <https://swisstph.github.io/AnophelesModel/>

- Report bugs at <https://github.com/SwissTPH/AnophelesModel/issues>

## Author

Clara Champagne, <clara.champagne@swisstph.ch>

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Olivier Briët, <olivier.briet@swisstph.ch>

Nakul Chitnis, <nakul.chitnis@swisstph.ch>

Tom Smith, <thomas.smith@swisstph.ch>
