# List available ITN parameterisations

Lists all net type / EHT combinations available in the built-in
`ITN_parameterisations_Champagne2025` dataset.

Every pair has both parameterisations (standard and dummy-host model),
chosen with `mode` in
[`get_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md).

## Usage

``` r
list_ITN_params()
```

## Value

Data frame with columns `netType`, `EHT` and `reference`; `netType` and
`EHT` are the `net_type` and `EHT` arguments of
[`get_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md).

## Author

Clara Champagne, <clara.champagne@swisstph.ch>

## Examples

``` r
list_ITN_params()
#>            netType         EHT                     reference
#> 1   Interceptor G2     Assenga      Assenga et al., Tanzania
#> 2      Olyset Plus     Assenga      Assenga et al., Tanzania
#> 3  Pyrethroid-only     Assenga      Assenga et al., Tanzania
#> 4   Interceptor G2 Assenga, CI Assenga et al., Côte d'Ivoire
#> 5      Olyset Plus Assenga, CI Assenga et al., Côte d'Ivoire
#> 6  Pyrethroid-only Assenga, CI Assenga et al., Côte d'Ivoire
#> 7      Olyset Plus      BIT055                        BIT055
#> 8  Pyrethroid-only      BIT055                        BIT055
#> 9   Interceptor G2      BIT080                        BIT080
#> 10 Pyrethroid-only      BIT080                        BIT080
#> 11  Interceptor G2     Kibondo                Kibondo et al.
#> 12 Pyrethroid-only     Kibondo                Kibondo et al.
#> 13  Interceptor G2      Martin    Martin et al., An. gambiae
#> 14     Olyset Plus      Martin    Martin et al., An. gambiae
#> 15 Pyrethroid-only      Martin    Martin et al., An. gambiae
#> 16  Interceptor G2   Martin, f   Martin et al., An. funestus
#> 17     Olyset Plus   Martin, f   Martin et al., An. funestus
#> 18 Pyrethroid-only   Martin, f   Martin et al., An. funestus
#> 19  Interceptor G2    Nguessan               Nguessan et al.
#> 20 Pyrethroid-only    Nguessan               Nguessan et al.
#> 21     Olyset Plus     Odufuwa                Odufuwa et al.
#> 22 Pyrethroid-only     Odufuwa                Odufuwa et al.
#> 23  Interceptor G2    Sovegnon               Sovegnon et al.
#> 24 Pyrethroid-only    Sovegnon               Sovegnon et al.
```
