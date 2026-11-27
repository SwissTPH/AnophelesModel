# Human and mosquito behavioural patterns

This data object contains time series of measurements of mosquito biting
behavior, as well as of human activity.

## Usage

``` r
activity_patterns
```

## Format

A `data frame` with 7 columns consisting of:

- `id:` database record ID (a unique value per time series)

- `species:` for mosquito activity entries: name of the mosquito
  species; for human activity entries: "Homo sapiens"

- `sampling:` for mosquito activity entries can be one of: HBI (human
  biting indoors), HBO (human biting outdoors), ABO (animal biting
  outdoors), or HB (human biting); for human activity entries can be one
  of BED (humans in bed) or IND (humans indoors)

- `country:` the country where the measurements have been taken

- `site:` the name of the location where the measurements have been
  taken

- `hour:` hour when the measurement was done

- `value:` for mosquito activity entries: proportion of overall mosquito
  bites at a given time in the corresponding location; for human
  activity entries: proportion of overall humans in the corresponding
  location at a given time.

## Value

activity cycle data

## References

TO DO

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Olivier Briët, <olivier.briet@swisstph.ch>

Nakul Chitnis, <nakul.chitnis@swisstph.ch>

Tom Smith, <thomas.smith@swisstph.ch>

## Examples

``` r
# Print the first lines of the data frame
head(activity_patterns)
#>   id      species sampling            country         site        hour   value
#> 1  1 Homo sapiens      IND Equatorial Guinea  Bioko Island 16.00_17.00 0.00000
#> 2  1 Homo sapiens      IND Equatorial Guinea  Bioko Island 17.00_18.00 0.00000
#> 3  1 Homo sapiens      IND Equatorial Guinea  Bioko Island 18.00_19.00 0.00000
#> 4  1 Homo sapiens      IND Equatorial Guinea  Bioko Island 19.00_20.00 0.62375
#> 5  1 Homo sapiens      IND Equatorial Guinea  Bioko Island 20.00_21.00 0.73125
#> 6  1 Homo sapiens      IND Equatorial Guinea  Bioko Island 21.00_22.00 0.83500
```
