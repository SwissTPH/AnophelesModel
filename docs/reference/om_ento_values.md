# Values of the species contribution placeholders of an OpenMalaria entomology

Fills the `annualEIR` placeholders `@contrib_<population>@` of
[`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md)
(used when `species_shares` is not given there): the relative
contribution of each mosquito population to the EIR, i.e. the share of
its species \\\times\\ its share within the species
(`om_ento$shares$share`), summing to 1 over all populations. One row per
species composition, to merge into an OpenMalaria scenario table; the
total EIR is set separately, with the `scaledAnnualEIR` attribute of
`<entomology>`.

## Usage

``` r
om_ento_values(om_ento, species_shares)
```

## Arguments

- om_ento:

  Output of
  [`get_OM_ento_snippet()`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md)
  without `species_shares`.

- species_shares:

  Species composition(s): a named vector (one composition) or a data
  frame / matrix with one row per composition and one column per taxon
  (canonical names or synonyms). Each composition must sum to 1 (see
  [`eir_shares`](https://swisstph.github.io/AnophelesModel/reference/eir_shares.md)).
  Row names, if any, are kept in a `composition` column.

## Value

Data frame with `composition` (with row names), the species shares (one
column per taxon) and one column per placeholder, named without the `@`
(`contrib_<population>`). A taxon with a share of 0 in a composition
gives its populations a contribution of 0 there (a warning: a
placeholder cannot remove a population).

## Examples

``` r
om = get_OM_ento_snippet(c("Anopheles funestus", "Anopheles gambiae ss/coluzzii"),
                         activity = def_activity_patterns())
#> get_OM_ento_snippet: annualEIR = a placeholder @contrib_<population>@ for the relative contribution of each population (values: om_ento_values()); set the total EIR with scaledAnnualEIR (e.g. defineEntomology(..., scaledAnnualEIR = "@EIR@")).
om_ento_values(om, rbind(
    funestus_dominant = c("Anopheles funestus" = 0.9,
                          "Anopheles gambiae ss/coluzzii" = 0.1),
    equal             = c("Anopheles funestus" = 0.5,
                          "Anopheles gambiae ss/coluzzii" = 0.5)))
#>         composition Anopheles funestus Anopheles gambiae ss/coluzzii
#> 1 funestus_dominant                0.9                           0.1
#> 2             equal                0.5                           0.5
#>   contrib_Anopheles_funestus_indoor1 contrib_Anopheles_funestus_indoor2
#> 1                          0.6095729                          0.2164939
#> 2                          0.3386516                          0.1202744
#>   contrib_Anopheles_funestus_outdoor
#> 1                         0.07393319
#> 2                         0.04107400
#>   contrib_Anopheles_gambiae_ss_coluzzii_indoor1
#> 1                                    0.07258244
#> 2                                    0.36291220
#>   contrib_Anopheles_gambiae_ss_coluzzii_indoor2
#> 1                                    0.01980605
#> 2                                    0.09903027
#>   contrib_Anopheles_gambiae_ss_coluzzii_outdoor
#> 1                                   0.007611507
#> 2                                   0.038057535
```
