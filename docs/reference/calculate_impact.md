# Calculate vectorial capacity reduction for a list of interventions

For each intervention object in `interventions_vec`, evaluates the
entomological model on every day of the intervention and returns the
average vectorial capacity and impact (reduction in vectorial capacity)
at each coverage. This is
[`run_interventions`](https://swisstph.github.io/AnophelesModel/reference/run_interventions.md)
applied to the parameters stored in each intervention (`$param_set`), so
both functions give the same results and the same output format.

The model parameters are those of `model_p` (the `model_p` used to build
the interventions does not matter).

## Usage

``` r
calculate_impact(
  interventions_vec,
  coverage_vec,
  model_p,
  exposure = 1,
  keep_time = FALSE
)
```

## Arguments

- interventions_vec:

  Intervention objects created with
  [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md),
  combined with [`c()`](https://rdrr.io/r/base/c.html) (e.g.
  `c(itn1, itn2)`); a single
  [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)
  output works as is.

- coverage_vec:

  Numeric vector of deployment coverages (values in `[0, 1]`). The
  impact is always computed relative to coverage 0 (no intervention),
  which is evaluated internally when not supplied.

- model_p:

  Model object created with
  [`build_model_obj()`](https://swisstph.github.io/AnophelesModel/reference/build_model_obj.md).

- exposure:

  Proportion of human exposure to bites that occurs where the
  intervention protects, in `[0, 1]` (default 1). The intervention acts
  on an effective coverage `coverage_vec * exposure`. Typically
  `calculate_exposure(...)$Exposure_Indoor_whileinbed` for ITNs, and
  `$indoor_resting` or `$Exposure_Indoor_total` for IRS (see
  [`calculate_exposure`](https://swisstph.github.io/AnophelesModel/reference/calculate_exposure.md)).
  Reported coverages remain the deployment coverages.

- keep_time:

  Logical (default `FALSE`): also return the vectorial capacity and
  impact on each day.

## Value

A `data.table` with one row per intervention and coverage:
`intervention` (position in `interventions_vec`), `name`, the
intervention parameters (as in
[`run_interventions()`](https://swisstph.github.io/AnophelesModel/reference/run_interventions.md)),
`coverage`, `exposure`, `avg_vc` and `avg_impact`. With
`keep_time = TRUE`, a list with this table as `impact` and a long table
`time` with `intervention`, `name`, `coverage`, `day` (0 = deployment),
`vc` and `impact`. Plot it with
[`plot_impact`](https://swisstph.github.io/AnophelesModel/reference/plot_impact.md).

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Olivier Briet

Nakul Chitnis

Tom Smith
