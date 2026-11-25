# List the taxa with entomological parameters in the package database

Lists the mosquito taxa (species, complexes and the genus) whose
entomological parameters are available in the package.

## Usage

``` r
list_all_species(level = NULL)
```

## Arguments

- level:

  Optional; `"species"`, `"complex"` or `"genus"` to list only taxa of
  that level. Default `NULL` lists all taxa.

## Value

Data frame with columns `species_name`, `level`, `complex` (parent
complex of a species, `NA` otherwise) and `imputed_traits` (parameters
without data for this taxon, whose values come from its complex or the
genus).

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

## Examples

``` r
species = list_all_species(level = "species")
head(species)
#>             species_name   level    complex
#> 1     Anopheles aconitus species   Funestus
#> 2    Anopheles albimanus species       <NA>
#> 3 Anopheles albitarsis A species Albitarsis
#> 4 Anopheles albitarsis B species Albitarsis
#> 5 Anopheles albitarsis E species Albitarsis
#> 6  Anopheles annularis A species  Annularis
#>                                        imputed_traits
#> 1      endophagy;endophily;HBI_indoor;HBI_outdoor;tau
#> 2                                    HBI_indoor;M;tau
#> 3 endophagy;endophily;HBI_indoor;HBI_outdoor;M;tau;A0
#> 4 endophagy;endophily;HBI_indoor;HBI_outdoor;M;tau;A0
#> 5 endophagy;endophily;HBI_indoor;HBI_outdoor;M;tau;A0
#> 6 endophagy;endophily;HBI_indoor;HBI_outdoor;M;tau;A0
```
