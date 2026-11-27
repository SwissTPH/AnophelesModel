# Read a vector bionomics table from a CSV file

Reads and validates a table of vector bionomics with the same structure
as `vec_ent_param`, for example a newer export from the
AnophelesBionomics package, so that it can be used with
`def_vector_params(bionomics_table = ...)` without reinstalling
AnophelesModel.

## Usage

``` r
read_bionomics_table(file)
```

## Arguments

- file:

  Path to a CSV file. Required columns: `species_name`, `level`
  (`"species"`, `"complex"` or `"genus"`), `M`, `Chi`, `A0`, `zeta.3`,
  `td`, `tau`, `ts`, `to`, `endophily`, `endophagy`. Optional:
  `complex`, `imputed_traits`, standard deviations (`*.sd`) and
  `HBI_indoor`, `HBI_outdoor`.

## Value

The validated table as a data frame.

## See also

[`def_vector_params`](https://swisstph.github.io/AnophelesModel/reference/def_vector_params.md),
[`vec_ent_param`](https://swisstph.github.io/AnophelesModel/reference/vec_ent_param.md)
