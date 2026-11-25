# define the vector-specific entomological parameters

`def_vector_params` This function can be used to load entomological
parameters for a mosquito taxon (species, complex or genus) from the
package database (or another bionomics table), or to set the
entomological parameters to new custom values provided by the user.

Taxa are identified by their name **and** level: the same name can
denote a species and a complex. *An. gambiae s.s.* and *An. coluzzii*
are merged in the data as `"Anopheles gambiae ss/coluzzii"`; the names
`"Anopheles gambiae"`, `"Anopheles gambiae s.s."` and
`"Anopheles coluzzii"` are accepted for it with a warning. Complexes are
stored under their label (e.g. `"Gambiae"`);
`"Anopheles gambiae complex"` is also accepted with `level = "complex"`.

## Usage

``` r
def_vector_params(
  mosquito_species = "Anopheles gambiae ss/coluzzii",
  level = "species",
  hbi = "indoor",
  vector_table = NULL,
  bionomics_table = NULL,
  verbose = TRUE
)
```

## Arguments

- mosquito_species:

  string, name of the taxon. Default `"Anopheles gambiae ss/coluzzii"`.
  Use
  [`list_all_species()`](https://swisstph.github.io/AnophelesModel/reference/list_all_species.md)
  to see the available taxa.

- level:

  taxonomic level of `mosquito_species`: `"species"` (default),
  `"complex"` or `"genus"`.

- hbi:

  human blood index used as `Chi`: `"indoor"` (default, indoor HBI),
  `"outdoor"` (outdoor HBI) or `"weighted"`
  (`HBI_indoor * endophagy + HBI_outdoor * (1 - endophagy)`; its
  standard deviation is approximated assuming independent estimates).
  For a custom `vector_table`, `Chi` is used as supplied unless `hbi` is
  `"outdoor"` or `"weighted"`.

- vector_table:

  data frame with custom vector entomological values. Must be provided
  if custom parameter values should be used instead of the ones in the
  package database. The data frame should use (a subset of) the column
  names of the database object `vec_ent_param`. Default value is NULL.

- bionomics_table:

  optional bionomics table with the same structure as `vec_ent_param`
  (e.g. from
  [`read_bionomics_table()`](https://swisstph.github.io/AnophelesModel/reference/read_bionomics_table.md)),
  used instead of the package database to look up `mosquito_species`.

- verbose:

  boolean specifying whether any messages should be displayed during the
  execution of the function. Default value is TRUE.

## Value

list object with the columns of `vec_ent_param` (with a custom
`vector_table`: its columns only), plus `hbi_source`:

- `species_name`, `level`, `complex`: taxon

- `imputed_traits`: parameters without data for this taxon

- `M`: parous rate (proportion of host seeking mosquitoes that have laid
  eggs at least once)

- `M.sd`: standard deviation of the parous rate

- `Chi`: human blood index (proportion of mosquito blood meals derived
  from humans), chosen with `hbi`

- `HBI_indoor`, `HBI_outdoor`: indoor and outdoor human blood indices

- `A0`: sac rate (proportion of mosquitoes who laid eggs in a day)

- `A0.sd`: standard deviation of the sac rate

- `zeta.3`: relative availability of different non-human hosts

- `td`: proportion of a day that a mosquito actively seeks a host

- `tau`: resting duration, time required for a mosquito that has
  encountered a host to return to host seeking

- `ts`: duration of the extrinsic incubation period (time required for
  sporozoites to develop in mosquitoes)

- `endophily:` proportion of indoor resting mosquitoes

- `endophily.sd:` standard deviation of the endophily

- `endophagy:` proportion of indoor feeding mosquitoes

- `endophagy.sd:` standard deviation of the endophagy

- `hbi_source`: which human blood index is used as `Chi` (absent for a
  custom `vector_table` with `hbi = "indoor"`, whose `Chi` is used as
  supplied)

Standard deviations (`.sd` columns) are included where available.

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

## Examples

``` r
# Retrieve the vector entomological parameters for An. gambiae s.s./coluzzii
gambiae_vec_p = def_vector_params()

# The whole Gambiae complex, using the outdoor human blood index
gambiae_complex = def_vector_params("Gambiae", level = "complex",
                                    hbi = "outdoor")
```
