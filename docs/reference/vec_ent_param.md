# Vector-specific estimated entomological parameters

Bionomic parameterisations for 57 *Anopheles* species, 17 complexes and
the genus, from the AnophelesBionomics package
(<https://github.com/SwissTPH/AnophelesBionomics>; "Estimating mosquito
bionomics parameters with a hierarchical Bayesian model", Current
Research in Parasitology & Vector-Borne Diseases, 2026). Each parameter
is estimated with a hierarchical Bayesian model (genus, complex,
species) from published field data; the values are posterior means and
standard deviations.

Taxa are identified by `species_name` **and** `level`. Complexes are
named by their label (e.g. `"Gambiae"`; `"Funestus"` is a group). A
species alone in its complex is listed once, as a species (no complex
row). *An. gambiae s.s.* and *An. coluzzii* are merged as
`"Anopheles gambiae ss/coluzzii"`. Taxa without data for a parameter
carry the value imputed from their complex or the genus, flagged in
`imputed_traits`. *An. freeborni* and *An. nivipes* have no parent
complex in the AnophelesBionomics taxonomy.

The dataset is regenerated with
`data-raw/vec_ent_param_from_AnophelesBionomics.R` (from the
AnophelesBionomics posterior draws) and `data-raw/vec_ent_param.R`; its
provenance is stored in `attr(vec_ent_param, "source")`.

## Usage

``` r
vec_ent_param
```

## Format

A `data frame` with the following columns:

- `species_name:` name of the taxon

- `level:` `"species"`, `"complex"` or `"genus"`

- `complex:` parent complex of a species; `NA` for complexes, the genus,
  species alone in their complex and species without a complex

- `imputed_traits:` parameters without data for this taxon, separated by
  `";"` (empty if all are estimated from its own data)

- `M`, `M.sd:` parous rate (proportion of host-seeking mosquitoes that
  have laid eggs at least once)

- `Chi`, `Chi.sd:` human blood index used by the model (proportion of
  blood meals taken on humans); by default the indoor human blood index
  (see `hbi` in
  [`def_vector_params`](https://swisstph.github.io/AnophelesModel/reference/def_vector_params.md))

- `HBI_indoor`, `HBI_indoor.sd`, `HBI_outdoor`, `HBI_outdoor.sd:` indoor
  and outdoor human blood indices

- `A0`, `A0.sd:` sac rate (proportion of mosquitoes who laid eggs the
  same day)

- `zeta.3:` relative availability of different non-human hosts (fixed to
  1)

- `td:` proportion of a day that a mosquito actively seeks a host (fixed
  to 0.33)

- `tau`, `tau.sd:` resting duration (days), time required for a mosquito
  that has encountered a host to return to host-seeking. Estimated as
  \\1/q\\, with \\q = fed / (fed + gravid)\\ the proportion of fed
  females among fed and gravid females in field resting collections
  (i.e. assuming a one-day fed stage). This may underestimate the
  resting duration if gravid females are under-sampled indoors or if
  digestion lasts more than a day. Rounded to whole days in the survival
  over the extrinsic incubation period.

- `ts:` duration of the extrinsic incubation period (fixed to 10)

- `to:` oocyst development time (fixed to 5)

- `endophily`, `endophily.sd:` proportion of indoor resting mosquitoes

- `endophagy`, `endophagy.sd:` proportion of indoor feeding mosquitoes

## Value

vector-specific entomological parameters

## See also

[`def_vector_params`](https://swisstph.github.io/AnophelesModel/reference/def_vector_params.md),
[`list_all_species`](https://swisstph.github.io/AnophelesModel/reference/list_all_species.md),
[`read_bionomics_table`](https://swisstph.github.io/AnophelesModel/reference/read_bionomics_table.md)

## Author

Monica Golumbeanu, <monica.golumbeanu@swisstph.ch>

Clara Champagne, <clara.champagne@swisstph.ch>

## Examples

``` r
# Print the first lines of the data frame
head(vec_ent_param)
#>             species_name   level    complex
#> 1             Albitarsis complex       <NA>
#> 2              Annularis complex       <NA>
#> 3              Anopheles   genus       <NA>
#> 4     Anopheles aconitus species   Funestus
#> 5    Anopheles albimanus species       <NA>
#> 6 Anopheles albitarsis A species Albitarsis
#>                                        imputed_traits         M       M.sd
#> 1             endophily;HBI_indoor;HBI_outdoor;tau;A0 0.4992852 0.05449998
#> 2          endophagy;endophily;HBI_indoor;HBI_outdoor 0.5665722 0.05162611
#> 3                                                     0.5621623 0.11195962
#> 4      endophagy;endophily;HBI_indoor;HBI_outdoor;tau 0.7417245 0.01627495
#> 5                                    HBI_indoor;M;tau 0.5621623 0.11195962
#> 6 endophagy;endophily;HBI_indoor;HBI_outdoor;M;tau;A0 0.4992852 0.05449998
#>         Chi     Chi.sd HBI_indoor HBI_indoor.sd HBI_outdoor HBI_outdoor.sd
#> 1 0.7862996 0.19935096  0.7862996    0.19935096   0.3210316     0.13119505
#> 2 0.7862996 0.19935096  0.7862996    0.19935096   0.3210316     0.13119505
#> 3 0.7862996 0.19935096  0.7862996    0.19935096   0.3210316     0.13119505
#> 4 0.8888701 0.05024776  0.8888701    0.05024776   0.2789333     0.09035337
#> 5 0.7862996 0.19935096  0.7862996    0.19935096   0.3204423     0.07581368
#> 6 0.7862996 0.19935096  0.7862996    0.19935096   0.3210316     0.13119505
#>          A0      A0.sd zeta.3   td      tau    tau.sd ts to endophily
#> 1 0.4616783 0.13642676      1 0.33 2.032249 1.0160269 10  5 0.3741708
#> 2 0.5577209 0.10093231      1 0.33 1.928723 0.5250293 10  5 0.3741708
#> 3 0.4616783 0.13642676      1 0.33 2.032249 1.0160269 10  5 0.3741708
#> 4 0.4827996 0.06225184      1 0.33 1.905176 0.4840903 10  5 0.6773032
#> 5 0.4051511 0.01293158      1 0.33 2.032249 1.0160269 10  5 0.0891974
#> 6 0.4616783 0.13642676      1 0.33 2.032249 1.0160269 10  5 0.3741708
#>   endophily.sd endophagy endophagy.sd
#> 1   0.29730378 0.4554501   0.06381631
#> 2   0.29730378 0.4030836   0.13087295
#> 3   0.29730378 0.4030836   0.13087295
#> 4   0.14655722 0.4822035   0.05195917
#> 5   0.01946437 0.2274075   0.01333692
#> 6   0.29730378 0.4554501   0.06381631
```
