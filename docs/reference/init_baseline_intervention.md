# Initialise a baseline intervention effects object

Builds a no-intervention baseline effects object from the host
parameters in `model_p`. All three host types start with identical
parameters — no intervention applied. Used internally by
`build_intervention`.

When `dummy_host = TRUE` a fourth host type ("dummy host") is appended
with all parameters set to zero. Its availability is subsequently set by
[`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)
when `"KillingDuringHostSeekingAbsolute"` is supplied in the `efficacy`
vector.

## Usage

``` r
init_baseline_intervention(model_p, npoints, dummy_host = FALSE)
```

## Arguments

- model_p:

  Model object from
  [`build_model_obj()`](https://swisstph.github.io/AnophelesModel/reference/build_model_obj.md).

- npoints:

  Number of interpolation time points.

- dummy_host:

  Logical. If `TRUE`, add a fourth dummy-host column (all zeros) to all
  effects matrices. Default `FALSE`.

## Value

A list with fields `id`, `description`, `parameterisation`, and
`effects`.
