# Build an intervention object from efficacy parameters

Constructs an intervention effects object from user-supplied initial
efficacy values (deterrency, pre-prandial killing, post-prandial
killing) and a parametric attrition decay function. The three columns of
each effects matrix correspond to: (1) protected humans, (2) unprotected
humans, (3) animals.

Two model structures are supported and are **mutually exclusive**:

- **Standard (3-host)**: uses `Deterrency`, `PrePrandial`, and
  `PostPrandial`.

- **Dummy-host (4-host)**: uses `Deterrency`,
  `KillingDuringHostSeekingAbsolute`, and `PostPrandial`. A fourth
  dummy-host column is added to all effects matrices. Do not supply
  `PrePrandial` together with `KillingDuringHostSeekingAbsolute`.

## Usage

``` r
build_intervention(
  efficacy,
  halflife_attrition = NULL,
  shape_attrition = NULL,
  decay_attrition = "weibull",
  duration_years = 3,
  model_p = NULL,
  name = NULL,
  mode = NULL,
  halflife_insecticide = NULL,
  component_decay = NULL,
  insecticide_decay = TRUE
)
```

## Arguments

- efficacy:

  Non-empty numeric vector with unique component names and finite,
  non-missing values, using any subset of the following efficacy
  parameters:

  - `"Deterrency"` in `[0,1]` — reduces host availability (`alphai`) for
    protected humans; deterred mosquitoes keep searching.

  - `"PrePrandial"` in `[0,1]` — pre-prandial killing: reduces the
    probability that a mosquito that has found a protected human bites
    and survives (`PBi`); mosquitoes failing this stage die. Standard
    model only.

  - `"PostPrandial"` in `[0,1]` — post-prandial killing: reduces the
    probability of surviving after feeding on a protected human to find
    a resting place (`PCi`).

  - `"KillingDuringHostSeekingAbsolute"` \\\geq 0\\ — killing during
    host-seeking via a dummy-host mechanism. Dummy-host model only. An
    **absolute** rate, not a probability: it is used directly as the
    availability of the dummy host (one per protected person), in the
    same units as the host availabilities `alphai`. Values from
    experimental hut fits that are relative to the control availability
    `alpha_0` (`KillingDuringHostSeekingRelative`) must first be
    multiplied by `alpha_0`
    ([`get_ITN_params()`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md)
    does this).

  Note on time units: the experimental hut model has no time scale (its
  rates are per hut-night), whereas the rates of this package are per
  day, with `td` the duration of host seeking. By convention, a
  hut-night is treated as a day: `KillingDuringHostSeekingAbsolute` is
  used directly as a per-day rate, without dividing by `td`. This is
  provisional, until the experimental hut fitting model is improved.
  With this convention, the field impacts of the dummy-host
  parameterisations are of the same order as those of the standard
  parameterisations of the same trials, whereas dividing by `td` gives
  systematically higher impacts (the standard model is not a validated
  benchmark either).

- halflife_attrition:

  Attrition decay parameter `L` (days): the half-life for Weibull /
  exponential / Hill, the time to zero for linear / step /
  smooth-compact (see
  [`decay_function()`](https://swisstph.github.io/AnophelesModel/reference/decay_function.md)).

- shape_attrition:

  Shape parameter `kappa` of the attrition decay function (for Weibull /
  Hill / smooth-compact; set to `NULL` otherwise).

- decay_attrition:

  Attrition decay function: `"weibull"`, `"exponential"`, `"hill"`,
  `"linear"`, `"step"`, `"smooth-compact"` or `"none"` (see
  [`decay_function()`](https://swisstph.github.io/AnophelesModel/reference/decay_function.md)).
  Default `"weibull"`.

- duration_years:

  Single finite positive duration in years. One interpolation point per
  day is used, with at least one point:
  `npoints = max(round(365 * duration_years), 1)`. Default 3 years.

- model_p:

  Model object from
  [`build_model_obj()`](https://swisstph.github.io/AnophelesModel/reference/build_model_obj.md).

- name:

  Optional label for the intervention.

- mode:

  Optional character; `"standard"` or `"dummy_host"`. When supplied,
  overrides auto-detection from the names of `efficacy`. The object
  returned by
  [`get_ITN_params()`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md)
  includes a `mode` field that can be passed here directly.

- halflife_insecticide:

  Optional named numeric vector (years), using the same names as
  `efficacy`, triggering an additional per-component **linear**
  insecticide decay on top of the main attrition: the efficacy of a
  component is multiplied by \\1 - t / (365 \cdot hl)\\. Despite its
  name, \\hl\\ is the time at which the effect reaches zero (the effect
  halves at \\hl / 2\\). This is the convention of the Champagne et
  al. (2025) parameterisations returned by
  [`get_ITN_params()`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md),
  where \\hl = a / (1 - Washed / Unwashed)\\, with \\a\\ the aging time
  of the trial's washed arm (3 years, 2 for Sovegnon), so that efficacy
  after \\a\\ years equals the washed (20x) value. Negative values
  (washed efficacy higher than unwashed) make the effect increase
  linearly over time; `Inf` means no insecticide decay. Zero and `NaN`
  values are invalid; `NA` means unspecified. For other decay shapes,
  use `component_decay`.

- component_decay:

  Optional named list specifying an independent decay function for each
  efficacy component, using the same names as `efficacy`. Each element
  is a list with fields `decay` (string), `L` (days; meaning depends on
  the shape, see
  [`decay_function()`](https://swisstph.github.io/AnophelesModel/reference/decay_function.md)),
  and optionally `kappa`. An explicit shape always overrides
  `halflife_insecticide` for the same component. Constant shapes
  (`"none"`, `"constant"`) need no `L` or `kappa`; other shapes require
  a positive `L` and, where applicable, a positive `kappa`. Incomplete
  specifications raise an error instead of falling back to linear or
  constant decay.

- insecticide_decay:

  Logical (default `TRUE`). If `FALSE`, `halflife_insecticide` and
  `component_decay` are ignored: the efficacies keep their initial (e.g.
  unwashed) values throughout and only attrition decays. If `TRUE`, the
  per-component insecticide decay is applied and, in addition, a Weibull
  decay is fitted for each efficacy component to the product attrition
  \\\times\\ insecticide decay (see Value).

## Value

A list of length 1 containing the intervention specification, ready to
pass to
[`calculate_impact()`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md).
Standard model: 3-column effects matrices. Dummy-host model: 4-column
effects matrices. When `insecticide_decay = TRUE`, the element
`fitted_decay` is a data frame with one row per efficacy component and
columns `component`, `L` (Weibull half-life, days), `kappa` and `rmse`:
the single Weibull per component (starting at 1) that best approximates
attrition \\\times\\ insecticide decay over the intervention duration
(as used for OpenMalaria GVI parameterisations). The model itself always
uses the exact (unfitted) decay. The element `settings` keeps the inputs
(efficacy, mode, attrition and per-component decays,
`insecticide_decay`), used by
[`get_OM_GVI_snippet()`](https://swisstph.github.io/AnophelesModel/reference/get_OM_GVI_snippet.md).
The element `param_set` is the same intervention as a one-row
`param_sets` table of
[`run_interventions()`](https://swisstph.github.io/AnophelesModel/reference/run_interventions.md),
which
[`calculate_impact()`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md)
evaluates. Several interventions are combined with
[`c()`](https://rdrr.io/r/base/c.html).
