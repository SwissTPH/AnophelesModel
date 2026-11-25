# Fill the vector control placeholders of a scenario table

Adds to the user's OpenMalaria scenario table (one row per scenario,
built as the user wants, e.g. with
[`expand.grid()`](https://rdrr.io/r/base/expand.grid.html)) the values
of the placeholders of a vector control intervention, row by row:

- **efficacies** (`<name>_deterrency`, ...), when the intervention is a
  list of several nets: those of the net named in column `net`;

- **decay parameters** (`<name>_deterrency_L` in years,
  `<name>_deterrency_k`, ...), when the intervention was defined with
  `decay = "placeholder"`;

- **dummy-host availabilities** (`<name>_dummy_<k>_pop<j>`) of
  dummy-host nets, when the net or the coverages vary: coverage of
  campaign \\k\\ \\\times\\ `KillingDuringHostSeekingAbsolute` / human
  availability of population \\j\\, the coverages taken from the columns
  named like the coverage placeholders of the schedule (e.g. `cov2021`);
  the decay of the dummy hosts is that of host-seeking killing
  (`<name>_hsk_L`, `<name>_hsk_k`), filled as the other decays.

Only the placeholders written by
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)
/
[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md)
(or
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md))
are filled: the intervention's record (nets, placeholders, populations,
schedule) is read from `source`. The other columns and the row order are
kept; the coverages and the scenario design stay the user's.

**Decays.** The decays differ between effects through the insecticide
decay of each effect (from the net's parameterisation), while the
attrition is common to all effects. With `combine = TRUE` (default),
`halflife` and `kappa` name **one attrition column** each (e.g. a
usage-adjusted attrition): each effect's decay is this attrition
combined with the effect's own insecticide decay (fitted by a Weibull
when needed, over `fit_years` of
[`om_vector_control`](https://swisstph.github.io/AnophelesModel/reference/om_vector_control.md)),
host-seeking killing (the dummy hosts) included. With `combine = FALSE`,
the columns are **final** decays, written as they are: one column per
effect (named vector, e.g.
`c(Deterrency = "hl_det", PrePrandial = "hl_pre", PostPrandial = "hl_post", KillingDuringHostSeekingAbsolute = "hl_hsk")`)
or one column for all effects (host-seeking killing: the decay of the
dummy hosts). Without columns, each net's own decay is used. Decay
columns need an intervention defined with `decay = "placeholder"`.

Half-life and shape are replaced **independently**: a `kappa` column
alone changes the shape and keeps the half-life (and the other way
round), the rest coming from the net (its attrition with
`combine = TRUE`; its exported decay of the effect with
`combine = FALSE`). Values must be positive numbers (the shape only
where the decay function has one); errors name the rows and the effect.

Values are computed once per distinct net and decay, so large tables are
fast.

## Usage

``` r
om_scenario_values(
  scenarios,
  source,
  name = NULL,
  net = NULL,
  halflife = NULL,
  kappa = NULL,
  decay_unit = c("years", "days"),
  combine = TRUE
)
```

## Arguments

- scenarios:

  Data frame of scenarios (one row per scenario).

- source:

  The `baseList` after
  [`define_OM_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)
  and
  [`deploy_OM_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md),
  or the result of
  [`get_OM_xml()`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md).

- name:

  Name of the intervention (optional with
  [`get_OM_xml()`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md)).

- net:

  Column naming the net of each row (the names of the list given to
  [`define_OM_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md));
  needed when the list has several nets.

- halflife, kappa:

  Columns of decay parameters (see Decays): half-life (or time to zero,
  for linear / step decays) and shape, each optional. Interventions
  specified with
  [`om_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/om_vector_control.md)
  are needed for `combine = TRUE`.

- decay_unit:

  Unit of the `halflife` columns: `"years"` (default, as OpenMalaria) or
  `"days"`. Written values are in years.

- combine:

  Logical (default `TRUE`): `halflife` / `kappa` are an attrition
  combined with each effect's insecticide decay; `FALSE`: final decays.

## Value

`scenarios` with one column per placeholder, named without the `@`.

## See also

[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md),
[`om_campaigns`](https://swisstph.github.io/AnophelesModel/reference/om_campaigns.md)

## Examples

``` r
if (FALSE) { # \dontrun{
nets = list(martin  = om_vector_control(get_ITN_params("Interceptor G2", "Martin"),
                                        halflife_attrition = 2 * 365, shape_attrition = 2),
            assenga = om_vector_control(get_ITN_params("Interceptor G2", "Assenga"),
                                        halflife_attrition = 2 * 365, shape_attrition = 2))
baseList = define_OM_vector_control(baseList, nets, name = "futITN",
                                    decay = "placeholder")
baseList = deploy_OM_vector_control(baseList, "futITN",
    om_campaigns("2019-01-25", "2030-01-25", coverage = "@futITNcov@"))
scens = expand.grid(net = c("martin", "assenga"), halflife = c(1, 2, 3),
                    futITNcov = c(0.5, 0.8))
scens = om_scenario_values(scens, baseList, "futITN", net = "net",
                           halflife = "halflife")
} # }
```
