# define host-specific entomological parameters

`def_host_params` This function can be used to load host-specific
entomological parameters available in the package or to set the
entomological parameters to new custom values provided by the user. Host
parameters (biting, survival, and infection probabilities for humans and
animals) are species-independent: the same values apply regardless of
mosquito species.

## Usage

``` r
def_host_params(host_table = NULL, verbose = TRUE)
```

## Arguments

- host_table:

  data frame with custom host-specific values. Must be provided if
  custom parameter values should be used instead of the ones in the
  package database. The provided data frame should have the same
  structure (column names) as the database object `host_ent_param`.
  Default value is NULL.

- verbose:

  boolean specifying whether any messages should be displayed during the
  execution of the function. Default value is TRUE.

## Value

list object with the following vector attributes:

- `species_name`: name of the mosquito species of the table row (if
  present; the values do not depend on it)

- `host`: `c("human", "animal")`

- `PBi`: probability that a mosquito that has encountered host i bites
  it (and survives to do so); mosquitoes failing a stage die

- `PCi`: probability that a mosquito finds a resting place after biting
  a host of type i

- `PDi`: probability that a mosquito survives the resting phase after
  biting a host of type i

- `PEi`: probability that a mosquito lays eggs and returns to host
  seeking after resting, having bitten a host of type i

- `Kvi`: proportion of susceptible mosquitoes that become infected after
  biting any host of type i

Each vector has two elements: (1) humans (without intervention) and (2)
animals, in this order whatever the row order of `host_table`. Protected
humans are added by the intervention functions.

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Olivier Briët

Tom Smith

Nakul Chitnis

## Examples

``` r
# Retrieve the host-specific entomological parameters
hosts_p = def_host_params()

# Print all the parameters
print(hosts_p)
#> $species_name
#> [1] "Anopheles gambiae" "Anopheles gambiae"
#> 
#> $host
#> [1] "human"  "animal"
#> 
#> $PBi
#> [1] 0.95 0.95
#> 
#> $PCi
#> [1] 0.95 0.95
#> 
#> $PDi
#> [1] 0.99 0.99
#> 
#> $PEi
#> [1] 0.88 0.88
#> 
#> $Kvi
#> [1] 0.03 0.00
#> 
```
