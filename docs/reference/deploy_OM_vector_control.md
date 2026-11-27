# Deploy a vector control intervention in an OpenMalaria scenario

Writes the deployment of an intervention defined with
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md):
**one** OpenMalaria `<deployment>` containing all its GVI components
(one per effect, or one merged component), so that all effects reach the
same people, with one `<deploy>` per campaign of `schedule`.

For dummy-host interventions, it also writes the host-seeking killing:
one dummy host per campaign (`addNonHumanHosts`, with its decays in
`nonHumanHostsModifications`; see
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)),
which needs OpenMalaria schema \>= 46. Such an intervention is deployed
**once**, with the whole schedule; `subpop`, `cumulative`, `minAge` and
`maxAge` are rejected (the dummy hosts assume campaigns covering the
whole population at the schedule coverages).

## Usage

``` r
deploy_OM_vector_control(
  baseList,
  name,
  schedule,
  end_date = NULL,
  subpop = NULL,
  cumulative = FALSE,
  minAge = NULL,
  maxAge = NULL
)
```

## Arguments

- baseList:

  openMalariaUtilities `baseList`, after
  [`define_OM_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md).

- name:

  Name of the intervention given to (or printed by)
  [`define_OM_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md).

- schedule:

  Data frame with one row per campaign: `date` (`"YYYY-MM-DD"`,
  increasing) and `coverage` (numbers in \[0, 1\] or placeholders such
  as `"@cov2021@"`), e.g. from
  [`om_campaigns`](https://swisstph.github.io/AnophelesModel/reference/om_campaigns.md);
  or a continuous schedule from
  [`om_continuous`](https://swisstph.github.io/AnophelesModel/reference/om_continuous.md)
  (deployment at target ages; standard model, without `cumulative`,
  `minAge`, `maxAge`).

- end_date:

  Optional end of the simulated period (`"YYYY-MM-DD"`): the dummy hosts
  of dummy-host interventions last until then (otherwise 100 years). It
  can also be kept in the schedule
  ([`om_campaigns`](https://swisstph.github.io/AnophelesModel/reference/om_campaigns.md)`(end_date = )`;
  if both are given, they must agree).

- subpop:

  Optional sub-population id suffix: the deployment is restricted to the
  sub-population `"<name>-<subpop>"` (`restrictToSubPop`). This and the
  next arguments: standard model only.

- cumulative:

  Logical (default `FALSE`): cumulative coverage (`cumulativeCoverage`,
  people already covered are not redeployed), measured on
  `"<name>-<subpop>"` or the first component.

- minAge, maxAge:

  Optional age limits of the deployment, in years (numbers or
  placeholders): **both or neither** (all ages), e.g.
  `minAge = 0, maxAge = 5` for under-fives. Several age groups: one
  deploy per group.

## Value

The updated `baseList`.

## See also

[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)
