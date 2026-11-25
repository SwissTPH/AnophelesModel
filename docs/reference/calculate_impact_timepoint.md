# Evaluate the entomological model at a single time point

Thin wrapper around `eval_entomological_model` for evaluating the model
at one interpolation point given an explicit host-count matrix.

## Usage

``` r
calculate_impact_timepoint(intervention_obj, model_obj, n_vec, Ni)
```

## Arguments

- intervention_obj:

  Intervention effects object (a single time-point slice:
  scalars/vectors for `alphai`, `PBi`, etc.).

- model_obj:

  Model object from
  [`build_model_obj()`](https://swisstph.github.io/AnophelesModel/reference/build_model_obj.md).

- n_vec:

  Number of daily emerging mosquitoes.

- Ni:

  Matrix of host fractions (rows = coverages, cols = host types),
  consistent with the unit-population convention used by
  [`build_model_obj()`](https://swisstph.github.io/AnophelesModel/reference/build_model_obj.md):
  protected humans in \[0,1\], unprotected humans in \[0,1\] (summing to
  1), animals = 1.

## Value

List of entomological outputs from `eval_entomological_model`: `Pf_all`,
`or_all`, `sr_all`, `sigmai_all`, `eiri_all`, `vc_all`, `f_all`,
`PAmu_all`.

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>
