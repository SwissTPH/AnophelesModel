# defines the activity patterns of vectors and hosts

`def_activity_patterns` This function creates an object that contains
information about mosquito and human activity: the biting rhythms of the
mosquitoes and the times when humans are indoors or in bed. Pass the
result to
[`calculate_exposure()`](https://swisstph.github.io/AnophelesModel/reference/calculate_exposure.md)
to derive the exposure factor applied to coverage.

## Usage

``` r
def_activity_patterns(activity = "default_Anopheles_gambiae")
```

## Arguments

- activity:

  can be either a string corresponding to default entries:
  "default_Anopheles_gambiae", "default_Anopheles_albimanus" or a list
  object with the following attributes:

  - `HBI`: ID or proportion of human biting indoors

  - `HBO`: ID or propotion human biting outdoors

  - `humans_indoors`: ID or proportion of humans indoors

  - `humans_in_bed`: ID or proportion of humans in bed

  These attribues can be either vectors with the corresponding values at
  each time point, or an ID corresponding to an entry in the package
  database object `activity_patterns`. The function list_activity() can
  be used to retrieve the available entries for all or specific
  geographical locations.

## Value

list object with the time series attributes:

- `HBI`: proportion of human biting indoors

- `HBO`: propotion human biting outdoors

- `humans_indoors`: proportion of humans indoors

- `humans_in_bed`:proportion of humans in bed

## Author

Monica Golumbeanu, <monica.golumbeanu@unibas.ch>

Olivier Briët

Nakul Chitnis, <nakul.chitnis@unibas.ch>

Tom Smith, <thomas.smith@unibas.ch>

## Examples

``` r
# Retrieve default human and mosquito activity patterns
default_rhythms = def_activity_patterns("default_Anopheles_gambiae")
print(default_rhythms)
#> $HBI
#>  [1] 0.000000000 0.000000000 0.000491400 0.000491400 0.005896806 0.007862408
#>  [7] 0.028009828 0.048157248 0.068304668 0.066339066 0.062899263 0.062899263
#> [13] 0.070270270 0.077641278 0.000000000 0.000000000
#> 
#> $HBO
#>  [1] 0.000000000 0.000000000 0.000982801 0.000982801 0.003931204 0.025061425
#>  [7] 0.042260442 0.056019656 0.073710074 0.077149877 0.077149877 0.073710074
#> [13] 0.038329238 0.031449631 0.000000000 0.000000000
#> 
#> $humans_indoors
#>  [1] 0.00000000 0.28380952 0.61904762 0.84761905 0.96190476 1.00000000
#>  [7] 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000 1.00000000
#> [13] 0.96190476 0.00952381 0.00000000 0.00000000
#> 
#> $humans_in_bed
#>  [1] 0.0000000 0.0000000 0.0000000 0.2271333 0.6666333 1.0000000 1.0000000
#>  [8] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.8833167 0.2006667
#> [15] 0.0000000 0.0000000
#> 
```
