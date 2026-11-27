# Define the entomology of an OpenMalaria scenario

Writes the entomology of
[`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md)
into an openMalariaUtilities `baseList`, with
[`openMalariaUtilities::defineEntomology()`](https://swisstph.github.io/r-openMalariaUtilities/reference/defineEntomology.html),
and keeps it in the `baseList`, so that
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)
uses the same mosquito populations and parameters.

The `annualEIR` of each population is its relative contribution to the
EIR (see `get_OM_ento_snippet(species_shares = )`); the total EIR is
`scaledAnnualEIR`. The seasonality gives the shape of the EIR over the
year: numbers, or placeholders (by default `"@m1@"`, ..., `"@m12@"`,
filled per scenario by columns `m1`, ..., `m12`). Placeholders need a
recent openMalariaUtilities: older versions of `defineEntomology()`
reject them with R \>= 4.3.

The animal host parameters of each population are those of `om_ento`
(its `hosts_p`), as in
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md):
older versions of `defineEntomology()` wrote fixed values (0.95, 0.95,
0.99), which are replaced. The schema version is that of the `baseList`
(`OMVersion`, 44 if absent); from schema 49, `propInfected` and
`propInfectious` (removed from the schema) are not written.

Requires the openMalariaUtilities package.

## Usage

``` r
define_OM_entomology(
  baseList,
  om_ento,
  seasonality = paste0("@m", 1:12, "@"),
  scaledAnnualEIR = "@EIR@",
  ...
)
```

## Arguments

- baseList:

  openMalariaUtilities `baseList`.

- om_ento:

  Output of
  [`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md).

- seasonality:

  12 monthly (or 365 daily) values for all populations: non-negative
  numbers, or placeholders (default `paste0("@m", 1:12, "@")`); or a
  named list with one such vector per population (names as in
  `om_ento$shares$mosquito`).

- scaledAnnualEIR:

  Total annual EIR: a number or a placeholder (default `"@EIR@"`).

- ...:

  Passed to `defineEntomology()` (e.g. `name`, `mode`).

## Value

The updated `baseList`.

## See also

[`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md),
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)
