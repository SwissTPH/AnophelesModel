# Load ITN efficacy parameters from the built-in database

Returns efficacy parameters and per-component insecticide half-lives for
a named net type and experimental hut trial (EHT), ready to be passed
directly to
[`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md).

Two model modes are supported:

- `"standard"` (default): three-host model with `Deterrency`,
  `PrePrandial`, and `PostPrandial`.

- `"dummy_host"`: four-host model with `Deterrency`,
  `KillingDuringHostSeekingAbsolute`, and `PostPrandial`. `Deterrency`
  and `PostPrandial` are required in both modes.

The returned `halflife_insecticide` vector encodes per-component
insecticide decay as a **linear** decay (in years). Despite the name, it
is the time at which the effect reaches zero, calibrated so that
efficacy at the aging time of the trial's washed arm (`aged_time`: 3
years, 2 for Sovegnon) equals the washed value. Negative values
(efficacy increased with washing) are returned as fitted. To use the
unwashed efficacy throughout, pass `insecticide_decay = FALSE` to
[`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md).

In the experimental hut model, host-seeking killing is fitted
**relative** to the availability `alpha_0` of the trial's control arm
(extra host-seeking mortality
`KillingDuringHostSeekingRelative * alpha_0`). Since `alpha_0` varies
widely between trials, relative values are not comparable across trials.
With `rescale_hsk = TRUE`, the returned
`KillingDuringHostSeekingAbsolute` is the **absolute** value
`KillingDuringHostSeekingRelative * alpha_0`, using the control
`alpha_0` of the same arm (unwashed or washed fit), and its insecticide
decay is recomputed from the absolute unwashed and washed values, with
the trial's `aged_time`. The model uses the absolute value only.

## Usage

``` r
get_ITN_params(
  net_type,
  EHT,
  mode = "standard",
  washed = FALSE,
  rescale_hsk = TRUE,
  verbose = TRUE
)
```

## Arguments

- net_type:

  Character string; net type label as in the `netType` column of
  `ITN_parameterisations_Champagne2025`. Use
  [`list_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/list_ITN_params.md)
  to see available values.

- EHT:

  Character string; experimental hut trial identifier as in the `EHT`
  column.

- mode:

  Character; `"standard"` (default) for the three-host model or
  `"dummy_host"` for the four-host model.

- washed:

  Logical (default `FALSE`). If `FALSE`, returns *Unwashed* (new net)
  efficacy values; if `TRUE`, returns *Washed* values.

- rescale_hsk:

  Logical (default `TRUE`); dummy-host mode only. If `TRUE`, multiply
  host-seeking killing by the control `alpha_0` of its arm (see
  Details). If `FALSE`, return the fitted relative value as
  `KillingDuringHostSeekingRelative` (with its fitted decay), e.g. to
  rescale it with another `alpha_0`;
  [`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)
  does not accept it (convert it to `KillingDuringHostSeekingAbsolute`
  first).

- verbose:

  Logical (default `TRUE`). Print a summary of the loaded
  parameterisation.

## Value

Named list with:

- `efficacy`: named numeric vector for the `efficacy` argument of
  [`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md).

- `halflife_insecticide`: named numeric vector (years) encoding a
  **linear** per-component decay (time to zero). Pass directly to the
  `halflife_insecticide` argument of
  [`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md).

- `mode`: the model structure (`"standard"` or `"dummy_host"`), for
  `build_intervention(mode = )`.

- `name`: suggested intervention label (`"net_type (EHT)"`).

- `reference`: citation string from the dataset.

- `alpha_0`: control availabilities `c(Unwashed, Washed)` used to
  rescale host-seeking killing, or `NULL` if no rescaling was done.

## See also

[`list_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/list_ITN_params.md),
[`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)

## Author

Clara Champagne, <clara.champagne@swisstph.ch>

## Examples

``` r
# Standard three-host model
itn_p = get_ITN_params("Interceptor G2", "Kibondo")
#> ITN parameterisation loaded: Interceptor G2 (Kibondo) 
#> Mode: standard 
#> Reference: Kibondo et al. 
#> Efficacy (unwashed):
#> PostPrandial  PrePrandial   Deterrency 
#>         0.52         0.44         0.71 
#> Insecticide decay, linear time to zero (years; omitted = no decay):
#> PostPrandial  PrePrandial   Deterrency 
#>         31.2         66.0        -71.0 
itn_p$efficacy
#> PostPrandial  PrePrandial   Deterrency 
#>         0.52         0.44         0.71 
itn_p$halflife_insecticide
#> PostPrandial  PrePrandial   Deterrency 
#>         31.2         66.0        -71.0 

# Dummy-host (four-host) model
itn_dh = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host")
#> ITN parameterisation loaded: Interceptor G2 (Kibondo) 
#> Mode: dummy_host 
#> Reference: Kibondo et al. 
#> Efficacy (unwashed):
#>                     PostPrandial                       Deterrency 
#>                            0.520                            0.710 
#> KillingDuringHostSeekingAbsolute 
#>                            0.609 
#> Host-seeking killing rescaled by the control alpha_0 (unwashed 0.17, washed 0.16)
#> Insecticide decay, linear time to zero (years; omitted = no decay):
#>                     PostPrandial                       Deterrency 
#>                           31.200                          -71.000 
#> KillingDuringHostSeekingAbsolute 
#>                           28.981 
itn_dh$efficacy
#>                     PostPrandial                       Deterrency 
#>                           0.5200                           0.7100 
#> KillingDuringHostSeekingAbsolute 
#>                           0.6086 
```
