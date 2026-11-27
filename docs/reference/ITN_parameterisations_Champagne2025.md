# ITN efficacy parameterisations from experimental hut trials

Efficacy estimates and per-component insecticide half-lives for
pyrethroid-only and next-generation ITNs, fitted to experimental hut
trial (EHT) data from multiple sites (Champagne et al. 2025).

Efficacy components are provided for the standard three-host model
(`Repellent`, `Preprandialkilling`, `Postprandialkilling`) and the
dummy-host model (`KillingDuringHostSeekingRelative`).

Use
[`get_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md)
to load a specific net type / EHT combination into a format ready for
[`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md).

## Usage

``` r
ITN_parameterisations_Champagne2025
```

## Format

A data frame with columns:

- `netType`: ITN product name (e.g. `"Interceptor G2"`,
  `"Pyrethroid-only"`).

- `EHT`: experimental hut trial site identifier.

- `reference`: citation for the underlying EHT data.

- `parameter`: efficacy component (`Repellent`, `Preprandialkilling`,
  `Postprandialkilling`, `KillingDuringHostSeekingRelative`, `alpha_0`,
  `mu_0`). `KillingDuringHostSeekingRelative` is relative to the
  control-arm availability `alpha_0` (rows with `netType == "control"`);
  [`get_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md)
  rescales it to the absolute value used by the model,
  `KillingDuringHostSeekingAbsolute`. (Named `KillingDuringHostSeeking`
  in the source table.)

- `Unwashed`: fitted efficacy for a new (unwashed) net.

- `Washed`: fitted efficacy after washing / ageing.

- `aged_time`: years represented by the washed arm (3, or 2 for
  Sovegnon).

- `halflife_insecticide`: per-component linear insecticide decay
  (years), computed as `aged_time / (1 - Washed / Unwashed)`. Despite
  the name, it is the time at which the effect reaches zero under linear
  decay, calibrated so that efficacy after `aged_time` years equals
  `Washed`. Negative values indicate the component efficacy increased
  with washing; they are kept as fitted (the effect then increases
  linearly over time).

- `fit`: fit of the trial the row belongs to (1, or 1 and 2 when a trial
  was fitted in several parts, each with its own control, e.g.
  Sovegnon); host-seeking killing is rescaled by the control `alpha_0`
  of the same fit.

## Value

ITN efficacy parameters from experimental hut trials.

## References

Champagne et al. (2025) — in preparation.

## Author

Clara Champagne, <clara.champagne@swisstph.ch>

## Examples

``` r
head(ITN_parameterisations_Champagne2025)
#>           netType     EHT      reference           parameter aged_time Unwashed
#> 1 Pyrethroid-only Kibondo Kibondo et al. Postprandialkilling         3     0.21
#> 2  Interceptor G2 Kibondo Kibondo et al. Postprandialkilling         3     0.52
#> 3 Pyrethroid-only Kibondo Kibondo et al.  Preprandialkilling         3     0.26
#> 4  Interceptor G2 Kibondo Kibondo et al.  Preprandialkilling         3     0.44
#> 5 Pyrethroid-only Kibondo Kibondo et al.           Repellent         3     0.82
#> 6  Interceptor G2 Kibondo Kibondo et al.           Repellent         3     0.71
#>   Washed halflife_insecticide fit
#> 1   0.18                 21.0   1
#> 2   0.47                 31.2   1
#> 3   0.25                 78.0   1
#> 4   0.42                 66.0   1
#> 5   0.77                 49.2   1
#> 6   0.74                -71.0   1
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
