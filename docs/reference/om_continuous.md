# Continuous deployment of a vector control intervention

Builds a continuous `schedule` for
[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md)
and
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md):
the intervention is given to people when they reach a target age (e.g.
nets through EPI or antenatal care, or at school entry), with a
coverage, while the deployment is active (OpenMalaria `<continuous>`
deployment). Unlike campaigns
([`om_campaigns`](https://swisstph.github.io/AnophelesModel/reference/om_campaigns.md)),
it cannot target an age range at given dates: use campaigns with
`minAge` / `maxAge` for that. A net deployed both continuously and by
campaigns is deployed twice under the same name (two deployments of the
same components).

Standard (non dummy-host) interventions only; `cumulative`, `minAge` and
`maxAge` of the deploy do not apply, `subpop` does.

## Usage

``` r
om_continuous(age, coverage, begin = NULL, end = NULL)
```

## Arguments

- age:

  Target ages in years (0 to 100), numbers or placeholders (e.g.
  `"@ageITN@"`); one deploy per age.

- coverage:

  Coverage at each target age: a number in \[0, 1\] or a placeholder
  (e.g. `"@contITNcov@"`), one value or one per age.

- begin, end:

  Optional dates (`"YYYY-MM-DD"`) between which the deployment is active
  (OpenMalaria's defaults: from the start of the intervention period,
  without end), one value or one per age.

## Value

A data frame of class `om_continuous`, one row per target age
(`targetAgeYrs`, `coverage`, `begin`, `end`).

## See also

[`om_campaigns`](https://swisstph.github.io/AnophelesModel/reference/om_campaigns.md),
[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md)

## Examples

``` r
# Nets to infants at 3 months (EPI), from 2021, coverage as a placeholder
om_continuous(age = 0.25, coverage = "@contITNcov@", begin = "2021-01-01")
#>   targetAgeYrs     coverage      begin  end
#> 1         0.25 @contITNcov@ 2021-01-01 <NA>
# Two channels with their own coverage
om_continuous(age = c(0.25, 6), coverage = c(0.8, 0.6),
              begin = "2021-01-01", end = "2030-01-01")
#>   targetAgeYrs coverage      begin        end
#> 1         0.25      0.8 2021-01-01 2030-01-01
#> 2            6      0.6 2021-01-01 2030-01-01
if (FALSE) { # \dontrun{
baseList = deploy_OM_vector_control(baseList, "IG2",
    om_continuous(age = 0.25, coverage = 0.8, begin = "2021-01-01"))
} # }
```
