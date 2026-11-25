# Add vector control interventions to an OpenMalaria scenario

Writes the generic vector interventions from
[`get_OM_GVI_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_GVI_snippet.md)
into an openMalariaUtilities `baseList`. When the effects of an
intervention share the same decay (e.g. no insecticide decay, or the
same insecticide decay for all effects), they are written as **one**
component named after the intervention, as in ITNcascades; otherwise as
one component per effect (`<name>-deterrency`,
`<name>-preprandialKillingEffect`, `<name>-postprandialKillingEffect`),
as
[`openMalariaUtilities::defineVectorControl()`](https://swisstph.github.io/r-openMalariaUtilities/reference/defineVectorControl.html)
does. The component ids are printed: when there are several per
intervention, they must be deployed together, to the same people
([`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md)).
Dummy-host interventions have no pre-prandial component (its value is
written as 0).

Requires the openMalariaUtilities package (uses its internal functions
`.defineInterventionsHeader()` and `.xmlAddList()`).

## Usage

``` r
add_OM_GVI(baseList, gvi, merge = TRUE, append = TRUE)
```

## Arguments

- baseList:

  openMalariaUtilities `baseList` (the human interventions section is
  created if needed).

- gvi:

  Output of
  [`get_OM_GVI_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_GVI_snippet.md)
  (several can be combined with [`c()`](https://rdrr.io/r/base/c.html)).

- merge:

  Logical (default `TRUE`): write one component when the three decays
  are identical.

- append:

  Logical (default `TRUE`): append to the existing interventions.

## Value

The updated `baseList`.

## See also

[`get_OM_GVI_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_GVI_snippet.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gvi = c(get_OM_GVI_snippet(itn, om_ento, "ITN"),
        get_OM_GVI_snippet(irs, om_ento, "IRS"))
baseList = add_OM_GVI(baseList, gvi)
} # }
```
