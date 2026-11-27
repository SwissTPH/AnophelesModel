# Mosquito parameters for an OpenMalaria scenario

Builds the mosquito (entomology) parameters of an OpenMalaria scenario
(schema 44) from the bionomics in `vec_ent_param`, as the input lists of
[`openMalariaUtilities::defineEntomology()`](https://swisstph.github.io/r-openMalariaUtilities/reference/defineEntomology.html).

With `activity`, each taxon is split into three mosquito populations
according to human exposure
([`calculate_exposure`](https://swisstph.github.io/AnophelesModel/reference/calculate_exposure.md)):
with `in_bed` the share of bites indoors while in bed (reached by ITNs)
and `irs` the share reached by IRS, chosen with `irs_exposure`: the
proportion resting indoors (`"indoor_resting"`, the endophily; default)
or the share of bites indoors (`"indoor_biting"`,
`Exposure_Indoor_total`),

- `<taxon>_indoor1`: share `min(in_bed, irs)`, reached by both ITNs and
  IRS;

- `<taxon>_indoor2`: share `max(in_bed, irs) - min(in_bed, irs)`,
  reached by ITNs only (if `in_bed > irs`) or by IRS only;

- `<taxon>_outdoor`: the rest, reached by neither.

With `"indoor_resting"`, the overlap between biting in bed and resting
indoors is an assumption (the largest possible,
[`min()`](https://rdrr.io/r/base/Extremes.html)). With
`"indoor_biting"`, bites in bed are a part of bites indoors, so
`indoor1` is exactly the in-bed share and `indoor2` the indoor bites
outside bed. The vectorial capacity calculation has the same choice
through its `exposure` argument (see
[`calculate_exposure`](https://swisstph.github.io/AnophelesModel/reference/calculate_exposure.md)).
The populations share all parameters except the human blood index:
indoor populations use the indoor HBI, the outdoor population the
outdoor HBI. Populations with a share of 0 (e.g. `indoor2` when the two
shares are equal, or a species with a `species_shares` of 0) are not
written, with a message. Without `activity`, each taxon is a single
population whose human blood index is chosen with `hbi`.

Mapping to OpenMalaria: `mosqRestDuration` = resting duration `tau` in
whole days (at least 1), `mosqSeekingDuration` = `td`,
`extrinsicIncubationPeriod` = `ts`, `mosqLaidEggsSameDayProportion` =
`A0`, `mosqSurvivalFeedingCycleProbability` = `M` (parous rate), human
host probabilities from `hosts_p`. Each population also carries a
`nonHumanHosts` entry with the animal host probabilities of `hosts_p`,
written by
[`define_OM_entomology`](https://swisstph.github.io/AnophelesModel/reference/define_OM_entomology.md)
and
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md)
(`defineEntomology()` alone writes fixed animal values and ignores this
entry).

## Usage

``` r
get_OM_ento_snippet(
  taxa,
  level = "species",
  activity = NULL,
  hbi = "weighted",
  hosts_p = def_host_params(),
  propInfected = 0.078,
  propInfectious = 0.021,
  bionomics_table = NULL,
  species_shares = NULL,
  irs_exposure = c("indoor_resting", "indoor_biting")
)
```

## Arguments

- taxa:

  Character vector of taxon names (see
  [`list_all_species`](https://swisstph.github.io/AnophelesModel/reference/list_all_species.md));
  the usual synonyms of *An. gambiae ss/coluzzii* are accepted with a
  warning.

- level:

  Taxonomic level(s) of `taxa`: `"species"` (default), `"complex"` or
  `"genus"`; recycled.

- activity:

  Optional activity patterns (from
  [`def_activity_patterns`](https://swisstph.github.io/AnophelesModel/reference/def_activity_patterns.md)),
  one per taxon (a single pattern is used for all taxa). Triggers the
  indoor/outdoor split.

- hbi:

  Human blood index of unsplit populations: `"weighted"` (default,
  indoor and outdoor HBI weighted by endophagy), `"indoor"` or
  `"outdoor"`.

- hosts_p:

  Host parameters from
  [`def_host_params`](https://swisstph.github.io/AnophelesModel/reference/def_host_params.md).

- propInfected, propInfectious:

  Initial proportions of infected and infectious mosquitoes, written to
  `seasonalityParameters`.

- bionomics_table:

  Optional bionomics table used instead of `vec_ent_param` (see
  [`read_bionomics_table`](https://swisstph.github.io/AnophelesModel/reference/read_bionomics_table.md)).

- species_shares:

  Optional named vector: share of each taxon in the EIR, one per taxon
  (names as in `taxa`, canonical names or synonyms), summing to 1; see
  [`eir_shares`](https://swisstph.github.io/AnophelesModel/reference/eir_shares.md)
  to compute it from relative abundances and sporozoite rates.

  The `annualEIR` of each population is its relative contribution to the
  EIR: share of its species \\\times\\ share within the species, summing
  to 1 over all populations. With `species_shares`, these are numbers;
  without (default), placeholders `"@contrib_<population>@"` (the
  population name with each run of characters other than letters and
  digits replaced by `_`, e.g.
  `@contrib_Anopheles_gambiae_ss_coluzzii_indoor1@`), whose values for
  any species composition are given by
  [`om_ento_values`](https://swisstph.github.io/AnophelesModel/reference/om_ento_values.md).
  In both cases, the total EIR is set with the `scaledAnnualEIR`
  attribute of `<entomology>` (a number or a placeholder, e.g.
  `defineEntomology(..., scaledAnnualEIR = "@EIR@")`).

- irs_exposure:

  With `activity`: the mosquitoes reached by IRS, `"indoor_resting"`
  (default: those resting indoors, the endophily) or `"indoor_biting"`
  (those biting indoors, `Exposure_Indoor_total`). It sets the split and
  the `indoor_resting` flag of `shares`, used by the IRS export.

## Value

List with

- `mosquitoParameters`: named list (one element per mosquito population)
  for `defineEntomology(mosquitoParameters = )`;

- `seasonalityParameters`: named list skeleton for
  `defineEntomology(seasonalityParameters = )`, with placeholders
  `"@contrib_<population>@"` for `annualEIR` (numbers with
  `species_shares`) and `"@m1@"`, ..., `"@m12@"` for the seasonality,
  filled per scenario (columns `m1`, ..., `m12`), or replaced by numbers
  with `define_OM_entomology(seasonality = )` (older
  openMalariaUtilities versions reject the placeholders with R \>= 4.3);

- `shares`: data frame with `mosquito`, `taxon`, `type` (`indoor1`,
  `indoor2`, `outdoor` or `all`), `share` within the taxon (with
  `species_shares`, also `species_share` and the overall `eir_share`),
  and the logical columns `in_bed` and `indoor_resting` (reached by
  interventions acting in bed, e.g. ITNs, or by IRS: resting indoors, or
  biting indoors with `irs_exposure = "indoor_biting"`), and `muvA`, the
  host-seeking death rate without interventions (per day, with the
  population's HBI);

- `model_params`: named list (one element per mosquito population) of
  [`build_model_obj`](https://swisstph.github.io/AnophelesModel/reference/build_model_obj.md)
  outputs with the population's HBI (`NULL` if the bionomics give no
  valid death rate); their human availability scales the dummy hosts of
  host-seeking killing
  ([`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md));

- `irs_exposure`: the choice above (`NULL` without `activity`).

## See also

[`get_OM_GVI_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_GVI_snippet.md),
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)

## Examples

``` r
om = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
                         activity = def_activity_patterns())
#> get_OM_ento_snippet: annualEIR = a placeholder @contrib_<population>@ for the relative contribution of each population (values: om_ento_values()); set the total EIR with scaledAnnualEIR (e.g. defineEntomology(..., scaledAnnualEIR = "@EIR@")).
names(om$mosquitoParameters)
#> [1] "Anopheles gambiae ss/coluzzii_indoor1"
#> [2] "Anopheles gambiae ss/coluzzii_indoor2"
#> [3] "Anopheles gambiae ss/coluzzii_outdoor"
om$shares
#>                                mosquito                         taxon    type
#> 1 Anopheles gambiae ss/coluzzii_indoor1 Anopheles gambiae ss/coluzzii indoor1
#> 2 Anopheles gambiae ss/coluzzii_indoor2 Anopheles gambiae ss/coluzzii indoor2
#> 3 Anopheles gambiae ss/coluzzii_outdoor Anopheles gambiae ss/coluzzii outdoor
#>        share in_bed indoor_resting      muvA
#> 1 0.72582440   TRUE           TRUE 0.6461614
#> 2 0.19806053   TRUE          FALSE 0.6461614
#> 3 0.07611507  FALSE          FALSE 0.6461614
if (FALSE) { # \dontrun{
baseList = openMalariaUtilities::defineEntomology(
    baseList, om$seasonalityParameters, om$mosquitoParameters)
} # }
```
