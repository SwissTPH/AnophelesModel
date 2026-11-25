# Calculate indoor and outdoor human exposure to biting

Calculates indoor and outdoor proportions of human exposure to mosquito
biting, based on activity patterns and mosquito endophagy / endophily
parameters.

The relevant proportion is used as the `exposure` argument of
[`calculate_impact()`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md)
/
[`run_interventions()`](https://swisstph.github.io/AnophelesModel/reference/run_interventions.md),
which reduces the effective coverage (`coverage * exposure`); it does
not modify the intervention efficacies (unlike AnophelesModel version
1). Typically `Exposure_Indoor_whileinbed` for ITNs; for IRS, see below.

For IRS, two choices are defensible: the mosquitoes resting indoors
(`indoor_resting`, the endophily) or the bites received indoors
(`Exposure_Indoor_total`). They describe different pathways and agree
only when both shares are equal; neither is wrong in itself. The
OpenMalaria export offers the same choice
(`get_OM_ento_snippet(irs_exposure = )`, default `"indoor_resting"`):
use the same one in both routes.

## Usage

``` r
calculate_exposure(activity_cycles, vec_p)
```

## Arguments

- activity_cycles:

  List with time-series attributes: `HBI`, `HBO`, `humans_indoors`,
  `humans_in_bed`.

- vec_p:

  List of vector-specific entomological parameters (see
  `def_vector_params`).

## Value

List with proportions of overall exposure:

- `Exposure_Indoor_total`

- `Exposure_Outdoor_total`

- `Exposure_Indoor_whileinbed`

- `Exposure_Outdoor_whileinbed`

- `indoor_resting`

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Olivier Briet

Nakul Chitnis

Tom Smith
