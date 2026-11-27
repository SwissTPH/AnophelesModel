# Vector control intervention for an OpenMalaria scenario

Builds the generic vector intervention (GVI) parameters of an
OpenMalaria scenario (schema 44) from an intervention object, as the
input list of
[`openMalariaUtilities::defineVectorControl()`](https://swisstph.github.io/r-openMalariaUtilities/reference/defineVectorControl.html).
Each effect (deterrency, pre-prandial and post-prandial killing) becomes
an OpenMalaria component with its own decay.

Values are the initial efficacies. The decay of each effect is the
product of attrition and its insecticide decay, exported exactly when it
is a single OpenMalaria decay shape and approximated otherwise:

- no insecticide decay for the effect (or `insecticide_decay = FALSE`):
  the attrition decay;

- no attrition (`decay_attrition = "none"`, e.g. IRS): the effect's
  insecticide decay (`component_decay`, or linear from
  `halflife_insecticide`);

- both (e.g. nets with insecticide decay): the Weibull fitted to their
  product (`fitted_decay` of
  [`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md),
  starting at 1 as OpenMalaria's decays).

A negative `halflife_insecticide` (efficacy increasing with washing) has
no OpenMalaria shape: it is only exported through the fitted Weibull
(with attrition), and is an error without attrition. Decay parameters
`L` are converted from days to years. As OpenMalaria has no separate
attrition for GVIs, net attrition is folded into the decay of the
effects.

The effects act on the mosquito populations exposed to the intervention
(see
[`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md)):
in bed for ITNs, resting indoors for IRS (or biting indoors, with
`get_OM_ento_snippet(irs_exposure = "indoor_biting")`); other
populations get `propActive = 0`. Use the same choice for the `exposure`
of
[`calculate_impact()`](https://swisstph.github.io/AnophelesModel/reference/calculate_impact.md)
(see
[`calculate_exposure`](https://swisstph.github.io/AnophelesModel/reference/calculate_exposure.md)).
Without the split
([`get_OM_ento_snippet()`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md)
without `activity`), the intervention acts on all the bites of every
population, outdoor bites included, with a warning (here, in
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)
and in
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md)).

Dummy-host interventions (`KillingDuringHostSeekingAbsolute`) have no
pre-prandial killing: their GVI has deterrency and post-prandial killing
only, and the host-seeking killing, which has no GVI equivalent, is not
part of this snippet (nor of
[`add_OM_GVI`](https://swisstph.github.io/AnophelesModel/reference/add_OM_GVI.md)),
and a warning says so: it is written by
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md) +
[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md),
or
[`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md).

## Usage

``` r
get_OM_GVI_snippet(
  interv,
  om_ento,
  intervention_type = c("ITN", "IRS"),
  name = NULL
)
```

## Arguments

- interv:

  Intervention from
  [`om_vector_control`](https://swisstph.github.io/AnophelesModel/reference/om_vector_control.md)
  (or an object from
  [`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)).

- om_ento:

  Output of
  [`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md).

- intervention_type:

  `"ITN"` (acts in bed) or `"IRS"` (acts on indoor resting mosquitoes,
  or indoor biting ones with
  `get_OM_ento_snippet(irs_exposure = "indoor_biting")`).

- name:

  Name of the intervention (default: the `name` of an
  [`om_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/om_vector_control.md)
  specification, else the description of a
  [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)
  object unless `"custom"`, else `"GVI"`).

## Value

Named list (one element, `name`) with `deterrency`,
`preprandialKillingEffect` (standard model only) and
`postprandialKillingEffect`, each with `decay` and per-mosquito
`anophelesParams` (`propActive`, `value`), for
`defineVectorControl(vectorInterventionParameters = )`.

## See also

[`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md),
[`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)

## Examples

``` r
itn = om_vector_control(
    c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
    halflife_attrition = 2 * 365, shape_attrition = 2, name = "ITN")
om  = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
                          activity = def_activity_patterns())
#> get_OM_ento_snippet: annualEIR = a placeholder @contrib_<population>@ for the relative contribution of each population (values: om_ento_values()); set the total EIR with scaledAnnualEIR (e.g. defineEntomology(..., scaledAnnualEIR = "@EIR@")).
gvi = get_OM_GVI_snippet(itn, om, intervention_type = "ITN")
str(gvi$ITN$deterrency$decay)
#> List of 3
#>  $ L       : chr "2"
#>  $ function: chr "weibull"
#>  $ k       : chr "2"
if (FALSE) { # \dontrun{
baseList = openMalariaUtilities::defineVectorControl(baseList, gvi)
} # }
```
