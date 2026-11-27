# Share of each mosquito species in the EIR

Share of each species in the entomological inoculation rate (EIR), from
the relative abundance of the species (e.g. proportions in collections)
and their sporozoite rates: `abundance * sporozoite_rate`, normalised to
sum to 1. The result can be passed to
`get_OM_ento_snippet(species_shares = )` or
[`om_ento_values`](https://swisstph.github.io/AnophelesModel/reference/om_ento_values.md).

## Usage

``` r
eir_shares(abundance, sporozoite_rate)
```

## Arguments

- abundance:

  Named vector of relative abundances, summing to 1.

- sporozoite_rate:

  Named vector of sporozoite rates, with the same names (in any order).
  There is no default: sporozoite rates are setting-specific (e.g.
  Kweyamba et al. 2025 for south-eastern Tanzania).

## Value

Named vector of EIR shares (names and order of `abundance`).

## Examples

``` r
eir_shares(c("Anopheles funestus" = 0.95, "Anopheles gambiae ss/coluzzii" = 0.05),
           c("Anopheles funestus" = 0.235, "Anopheles gambiae ss/coluzzii" = 0.114))
#>            Anopheles funestus Anopheles gambiae ss/coluzzii 
#>                    0.97510373                    0.02489627 
```
