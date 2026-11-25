# Run the entomological model across multiple parameter sets

Fully vectorised runner: evaluates the entomological model for every
combination of intervention parameter set, coverage value, and
(optionally) vector parameter draw. All `param_sets × coverage`
combinations are evaluated in a single matrix call to
`eval_entomological_model()` per timepoint per draw, avoiding per-row R
loop overhead and scaling efficiently to tens of thousands of parameter
sets.

Two model structures are supported and are **mutually exclusive**. The
model is auto-detected from `param_sets`:

- **Standard (3-host)**: no `killing_during_host_seeking_absolute`
  column (or all values `NA`). Uses `deterrency`, `preprandial`,
  `postprandial`.

- **Dummy-host (4-host)**: `killing_during_host_seeking_absolute` column
  present with at least one non-`NA` value. Uses `deterrency`,
  `killing_during_host_seeking_absolute`, `postprandial`. `preprandial`
  must be `0` for all rows.

## Usage

``` r
run_interventions(
  param_sets,
  model_p,
  coverage_vec,
  vec_param_sets = NULL,
  interv_param_sets = NULL,
  exposure_sets = NULL,
  coverage_sets = NULL,
  duration_years = 3,
  insecticide_decay = TRUE,
  exposure = 1,
  keep_time = FALSE
)
```

## Arguments

- param_sets:

  Data frame where each row defines one parameter set. Required columns:

  - `deterrency`: initial deterrency efficacy in `[0,1]`

  - `preprandial`: initial pre-prandial killing efficacy in `[0,1]` (set
    to `0` for the dummy-host model)

  - `postprandial`: initial post-prandial killing efficacy in `[0,1]`

  - `halflife_attrition`: attrition decay parameter `L` (days):
    half-life for Weibull / exponential / Hill, time to zero for linear
    / step / smooth-compact (the column can be omitted when every row
    has `decay_attrition = "none"`)

  - `shape_attrition`: attrition decay shape parameter (required for
    Weibull / Hill / smooth-compact; `NA` or omitted otherwise)

  - `decay_attrition`: decay shape string (see
    [`decay_function()`](https://swisstph.github.io/AnophelesModel/reference/decay_function.md))

  Optional columns:

  - `name`: label of the parameter set (used by
    [`plot_impact()`](https://swisstph.github.io/AnophelesModel/reference/plot_impact.md));
    any other column is also carried to the output.

  - `killing_during_host_seeking_absolute`: activates the dummy-host
    (4-host) model; absolute availability of the dummy host (see
    `KillingDuringHostSeekingAbsolute` in
    [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)).
    Non-missing values must be finite and non-negative; `NA` means
    inactive.

  - `halflife_det`, `halflife_pre`, `halflife_post`, `halflife_hsk`:
    per-component linear insecticide decay (years); despite the name,
    the time at which the effect reaches zero (see
    `halflife_insecticide` in
    [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)).
    `halflife_hsk` applies to `killing_during_host_seeking_absolute` in
    dummy-host mode.

  - `decay_det`, `L_det`, `kappa_det` (and equivalently `_pre`, `_post`,
    `_hsk`): general per-component decay function, `L` (days; meaning
    depends on the shape, see
    [`decay_function()`](https://swisstph.github.io/AnophelesModel/reference/decay_function.md)),
    and shape. `_hsk` controls the decay of
    `killing_during_host_seeking_absolute`. Overrides the corresponding
    `halflife_*` column whenever a shape is specified. Constant shapes
    need no `L_*` or `kappa_*`; other shapes require their parameters
    (incomplete specifications raise an error).

- model_p:

  Model object from
  [`build_model_obj()`](https://swisstph.github.io/AnophelesModel/reference/build_model_obj.md).

- coverage_vec:

  Non-empty numeric vector of finite deployment coverages in `[0,1]`.
  Coverage `0` is used internally as the no-intervention baseline for
  computing `avg_impact`; it is added automatically if not present, and
  the corresponding row is included in the output only when explicitly
  supplied.

- vec_param_sets:

  Optional data frame of vector parameter draws (same columns as
  `vec_ent_param`). Each row is one draw. Adds a `draw` column to the
  output.

- interv_param_sets:

  Optional data frame with `n_draw` rows and the same columns as
  `param_sets`. Row `d` replaces `param_sets` for draw `d`. A list of
  `n_draw` single-row data frames is also accepted for backward
  compatibility.

- exposure_sets:

  Optional numeric vector (or list) of `n_draw` exposure factors; each
  replaces `exposure` for that draw, e.g. to propagate uncertainty in
  activity patterns via
  [`calculate_exposure()`](https://swisstph.github.io/AnophelesModel/reference/calculate_exposure.md).

- coverage_sets:

  Optional list of `n_draw` coverage vectors; each replaces
  `coverage_vec` for that draw.

- duration_years:

  Single finite positive duration in years (one time point per day, with
  at least one point). Default 3 years.

- insecticide_decay:

  Logical (default `TRUE`). If `FALSE`, the per-component insecticide
  decay columns (`halflife_*`, `decay_*`, `L_*`, `kappa_*`) are ignored:
  efficacies keep their initial (e.g. unwashed) values and only
  attrition decays.

- exposure:

  Proportion of human exposure to bites that occurs where the
  intervention protects, in `[0, 1]` (default 1). The intervention acts
  on an effective coverage `coverage * exposure`. Typically
  `calculate_exposure(...)$Exposure_Indoor_whileinbed` for ITNs, and
  `$indoor_resting` or `$Exposure_Indoor_total` for IRS (see
  [`calculate_exposure`](https://swisstph.github.io/AnophelesModel/reference/calculate_exposure.md)).

- keep_time:

  Logical (default `FALSE`): also return the vectorial capacity and
  impact on each day (memory-heavy for large sweeps).

## Value

A `data.table` with one row per `(parameter set × coverage [× draw])`.
All columns of `param_sets` are included; when `vec_param_sets` is
supplied the draw-specific vector parameter columns are appended. Output
columns `coverage` (deployment coverage), `exposure`, `avg_vc`,
`avg_impact`, and (when any uncertainty source is supplied) `draw`
follow.

With `keep_time = TRUE`, a list with this table as `impact` and a long
`data.table` `time` with columns `param_set` (row of `param_sets`),
`name` (if `param_sets` has one), `coverage`, `day` (0 = deployment),
`vc`, `impact` and, with uncertainty draws, `draw`.
