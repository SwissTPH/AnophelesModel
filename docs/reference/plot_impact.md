# Plot the reduction in vectorial capacity against coverage

Plots the average reduction in vectorial capacity (`avg_impact`) against
coverage, one line per intervention, from the output of
[`calculate_impact`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md)
or
[`run_interventions`](https://swisstph.github.io/AnophelesModel/reference/run_interventions.md).
With uncertainty draws (a `draw` column), the line is the median over
the draws and the band their 95% interval.

## Usage

``` r
plot_impact(impacts, title = NULL)
```

## Arguments

- impacts:

  Output of
  [`calculate_impact()`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md)
  or
  [`run_interventions()`](https://swisstph.github.io/AnophelesModel/reference/run_interventions.md)
  (the table, or the list returned with `keep_time = TRUE`). Lines are
  labelled by the `name` column (give each intervention a distinct
  name); without it, by the `intervention` column.

- title:

  Optional plot title, e.g. the mosquito species.

## Value

A ggplot object.

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Olivier Briët, <olivier.briet@swisstph.ch>

Nakul Chitnis, <nakul.chitnis@swisstph.ch>

Tom Smith, <thomas.smith@swisstph.ch>

## Examples

``` r
model_p = build_model_obj(def_vector_params(), def_host_params())
itn = build_intervention(
    efficacy = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
    halflife_attrition = 2 * 365, shape_attrition = 2,
    duration_years = 1, model_p = model_p, name = "ITN")
#> build_intervention: using standard (3-host) model with PrePrandial.
plot_impact(calculate_impact(itn, seq(0, 1, by = 0.2), model_p))
#> Ignoring unknown labels:
#> • fill : ""

```
