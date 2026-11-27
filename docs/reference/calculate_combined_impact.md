# Calculate impact for a combination of two interventions

Computes the mean reduction in vectorial capacity when two interventions
are deployed simultaneously at specified coverages. Effects are combined
multiplicatively (relative to the unintervened baseline).

Dummy-host (4-host) interventions are supported, also combined with
standard ones. Host-seeking killing is **additive**: each product acts
as its own dummy host, so for people covered by both interventions the
dummy-host availabilities add up.

The two interventions are evaluated over the shorter of their durations.
As in
[`calculate_impact()`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md),
their daily effects are rebuilt from their parameters (`$param_set`:
efficacies, attrition, insecticide decay, with their duration and
`insecticide_decay` setting) with the host and vector parameters of
`model_p`: the model used to build them does not matter.

Unlike
[`calculate_impact()`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md),
there is no `exposure` argument: the protected exposure of two products
overlaps (e.g. in-bed exposure for ITNs is part of indoor exposure for
IRS), so supply exposure-adjusted coverages (e.g.
`coverage * Exposure_Indoor_whileinbed`) directly.

## Usage

``` r
calculate_combined_impact(
  combination_name = "combination",
  intervention1,
  intervention2,
  cov_intervention1,
  cov_intervention2,
  cov_combination = NULL,
  model_p
)
```

## Arguments

- combination_name:

  String label for the combination.

- intervention1:

  Intervention object from
  [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)
  (an element, e.g. `interv[[1]]`).

- intervention2:

  Intervention object from
  [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)
  (an element, e.g. `interv[[1]]`).

- cov_intervention1:

  Coverage of intervention 1 (scalar in `[0,1]`).

- cov_intervention2:

  Coverage of intervention 2 (scalar in `[0,1]`).

- cov_combination:

  Coverage of the overlap (default `NULL` sets it to
  `cov_intervention1 * cov_intervention2`).

- model_p:

  Model object from
  [`build_model_obj()`](https://swisstph.github.io/AnophelesModel/reference/build_model_obj.md).

## Value

Mean reduction in vectorial capacity (scalar).

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Tom Smith

Clara Champagne
