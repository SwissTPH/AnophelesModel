# Specify a vector control intervention for OpenMalaria

Specifies an ITN or IRS intervention for the OpenMalaria export
([`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md),
[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md),
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md),
[`om_scenario_values`](https://swisstph.github.io/AnophelesModel/reference/om_scenario_values.md))
from its efficacies and decays, without a model or a duration: the
export builds the intervention with the entomology of the scenario, over
the right period. Efficacies and decays are as in
[`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md).

## Usage

``` r
om_vector_control(
  efficacy,
  halflife_attrition = NULL,
  shape_attrition = NULL,
  decay_attrition = "weibull",
  halflife_insecticide = NULL,
  component_decay = NULL,
  insecticide_decay = TRUE,
  mode = NULL,
  name = NULL,
  fit_years = NULL
)
```

## Arguments

- efficacy:

  Named efficacies (`Deterrency`, `PrePrandial`, `PostPrandial`,
  `KillingDuringHostSeekingAbsolute`), or the output of
  [`get_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md),
  whose efficacy, insecticide decay, model structure and name are used
  (unless given here).

- halflife_attrition, shape_attrition, decay_attrition:

  Attrition decay, as in
  [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)
  (`halflife_attrition` in days).

- halflife_insecticide, component_decay, insecticide_decay:

  Per-component insecticide decay, as in
  [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md).

- mode:

  `"standard"` or `"dummy_host"` (default: from the efficacies).

- name:

  Optional name (used for the OpenMalaria ids if no other name is
  given).

- fit_years:

  Window (years) over which a Weibull is fitted to each effect's decay
  when attrition and insecticide decay cannot be exported as a single
  OpenMalaria decay (default: 3 years for ITNs, 1 year for IRS, chosen
  by `intervention_type` at the export).

## Value

An object of class `om_vector_control`.

## See also

[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md),
[`get_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md)

## Examples

``` r
# Built-in parameterisation
ig2 = om_vector_control(get_ITN_params("Interceptor G2", "Kibondo",
                                       mode = "dummy_host", verbose = FALSE),
                        halflife_attrition = 2 * 365, shape_attrition = 2)
# Custom net
custom = om_vector_control(
    c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.5,
      PostPrandial = 0.4),
    halflife_attrition = 2 * 365, shape_attrition = 2, name = "Custom ITN")
```
