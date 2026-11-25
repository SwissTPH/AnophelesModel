# Schedule of vector control campaigns

Builds the `schedule` of
[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md)
and
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md)
(which keep it for
[`om_scenario_values`](https://swisstph.github.io/AnophelesModel/reference/om_scenario_values.md)):
one row per campaign, with its date and coverage, from regular dates
(`from`, `to`, `every`) or given `dates`.

## Usage

``` r
om_campaigns(
  from = NULL,
  to = NULL,
  every = "1 year",
  coverage,
  by_year = FALSE,
  dates = NULL,
  end_date = NULL
)
```

## Arguments

- from, to:

  First and last possible dates (`"YYYY-MM-DD"`); the campaigns are
  `seq(from, to, by = every)`.

- every:

  Interval between campaigns, as `by` in
  [`seq.Date`](https://rdrr.io/r/base/seq.Date.html): `"1 year"`
  (default), `"6 months"`, `"3 weeks"`, ...

- coverage:

  Coverage: a number in \[0, 1\] or a placeholder (e.g.
  `"@contITNcov@"`), used for every campaign, or one value per campaign.

- by_year:

  Logical (default `FALSE`): with a placeholder coverage, one
  placeholder per year (`"@histITNcov@"` becomes `"@histITNcov2015@"`,
  `"@histITNcov2016@"`, ...), as `byyear` of openMalariaUtilities'
  `deploy_it_compat()`.

- dates:

  Dates of the campaigns, instead of `from`, `to` and `every`.

- end_date:

  Optional end of the simulated period (`"YYYY-MM-DD"`, after the last
  campaign): the dummy hosts of dummy-host interventions last until then
  (otherwise 100 years). Kept with the schedule, so that
  [`deploy_OM_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md)
  and
  [`get_OM_xml()`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md)
  need no `end_date` argument.

## Value

Data frame with `date` and `coverage`, and the `end_date` as an
attribute when given (subsetting the data frame drops it).

## Examples

``` r
# Annual campaigns, one coverage placeholder per year
om_campaigns("2015-01-25", "2018-01-25", coverage = "@histITNcov@", by_year = TRUE)
#>         date         coverage
#> 1 2015-01-25 @histITNcov2015@
#> 2 2016-01-25 @histITNcov2016@
#> 3 2017-01-25 @histITNcov2017@
#> 4 2018-01-25 @histITNcov2018@
# Same coverage every year, period ending in 2031
om_campaigns("2019-01-25", "2030-01-25", coverage = "@contITNcov@",
             end_date = "2031-01-01")
#>          date     coverage
#> 1  2019-01-25 @contITNcov@
#> 2  2020-01-25 @contITNcov@
#> 3  2021-01-25 @contITNcov@
#> 4  2022-01-25 @contITNcov@
#> 5  2023-01-25 @contITNcov@
#> 6  2024-01-25 @contITNcov@
#> 7  2025-01-25 @contITNcov@
#> 8  2026-01-25 @contITNcov@
#> 9  2027-01-25 @contITNcov@
#> 10 2028-01-25 @contITNcov@
#> 11 2029-01-25 @contITNcov@
#> 12 2030-01-25 @contITNcov@
# Given dates and coverages
om_campaigns(dates = c("2021-01-01", "2024-01-01"), coverage = c(0.8, 0.6))
#>         date coverage
#> 1 2021-01-01      0.8
#> 2 2024-01-01      0.6
```
