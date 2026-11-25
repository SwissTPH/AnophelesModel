# Extract activity patterns of humans or mosquitoes

Returns activity patterns from the package database matching the
provided criteria.

## Usage

``` r
list_activity(
  species = "Anopheles gambiae",
  sampling = "HBI",
  country = NULL,
  site = NULL
)
```

## Arguments

- species:

  Name of the species. Use "Homo sapiens" for human patterns. Default is
  "Anopheles gambiae".

- sampling:

  Sampling type: "IND", "BED", "HBI", "HBO", "ABO", or "HB". Default is
  "HBI".

- country:

  Country name. Default NULL returns all countries.

- site:

  Site name. Default NULL returns all sites.

## Value

Data frame with columns: id, species, sampling, country, site, variable
(time point), value.

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

## Examples

``` r
humans_indoors = list_activity(species = "Homo sapiens", sampling = "IND",
site = "Rachuonyo")
print(humans_indoors)
#>    id      species sampling country      site        hour      value
#> 17  2 Homo sapiens      IND   Kenya Rachuonyo 16.00_17.00 0.00000000
#> 18  2 Homo sapiens      IND   Kenya Rachuonyo 17.00_18.00 0.15566667
#> 19  2 Homo sapiens      IND   Kenya Rachuonyo 18.00_19.00 0.39518333
#> 20  2 Homo sapiens      IND   Kenya Rachuonyo 19.00_20.00 1.00000000
#> 21  2 Homo sapiens      IND   Kenya Rachuonyo 20.00_21.00 1.00000000
#> 22  2 Homo sapiens      IND   Kenya Rachuonyo 21.00_22.00 1.00000000
#> 23  2 Homo sapiens      IND   Kenya Rachuonyo 22.00_23.00 1.00000000
#> 24  2 Homo sapiens      IND   Kenya Rachuonyo 23.00_00.00 1.00000000
#> 25  2 Homo sapiens      IND   Kenya Rachuonyo 00.00_01.00 1.00000000
#> 26  2 Homo sapiens      IND   Kenya Rachuonyo 01.00_02.00 1.00000000
#> 27  2 Homo sapiens      IND   Kenya Rachuonyo 02.00_03.00 1.00000000
#> 28  2 Homo sapiens      IND   Kenya Rachuonyo 03.00_04.00 1.00000000
#> 29  2 Homo sapiens      IND   Kenya Rachuonyo 04.00_05.00 1.00000000
#> 30  2 Homo sapiens      IND   Kenya Rachuonyo 05.00_06.00 0.53900000
#> 31  2 Homo sapiens      IND   Kenya Rachuonyo 06.00_07.00 0.03533333
#> 32  2 Homo sapiens      IND   Kenya Rachuonyo 07.00_08.00 0.00000000
```
