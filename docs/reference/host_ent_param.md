# Host-specific estimated parameters for the entomological model of malaria transmission

This data object contains, for humans (without intervention) and
animals, estimated parameters for the entomological model of malaria
transmission. The values do not depend on the mosquito species;
[`def_host_params`](https://swisstph.github.io/AnophelesModel/reference/def_host_params.md)
uses the first human and animal rows.

## Usage

``` r
host_ent_param
```

## Format

A `data frame` with one row per mosquito species and host type, and the
columns:

- `species_name:` name of the mosquito species

- `host:` host type, `"human"` or `"animal"`

- `PBi`, `PCi`, `PDi`, `PEi`: probabilities that a mosquito completes
  the next stage of the oviposition cycle, conditional on having reached
  that stage: PBi (a mosquito that has encountered a host of type i
  bites it), PCi (it finds a resting place after biting), PDi (it
  survives the resting phase), PEi (it lays eggs and returns to host
  seeking)

- `Kvi:` proportion of susceptible mosquitoes that become infected after
  biting a host of type i

## Value

host-specific entomological parameters

## References

TO DO

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Olivier Briët, <olivier.briet@swisstph.ch>

Nakul Chitnis, <nakul.chitnis@swisstph.ch>

Tom Smith, <thomas.smith@swisstph.ch>

## Examples

``` r
# Print the first lines of the data frame object
head(host_ent_param)
#>           species_name   host  PBi  PCi  PDi  PEi  Kvi
#> 1    Anopheles gambiae  human 0.95 0.95 0.99 0.88 0.03
#> 2    Anopheles gambiae animal 0.95 0.95 0.99 0.88 0.00
#> 3   Anopheles funestus  human 0.95 0.95 0.99 0.88 0.03
#> 4   Anopheles funestus animal 0.95 0.95 0.99 0.88 0.00
#> 5 Anopheles arabiensis  human 0.95 0.95 0.99 0.88 0.03
#> 6 Anopheles arabiensis animal 0.95 0.95 0.99 0.88 0.00
```
