# Define a vector control intervention in an OpenMalaria scenario

Writes the definition of a vector control intervention (ITN or IRS) into
an openMalariaUtilities `baseList`, for both model structures: the
components of the generic vector intervention (GVI) of
[`add_OM_GVI`](https://swisstph.github.io/AnophelesModel/reference/add_OM_GVI.md)
(deterrency, pre-prandial killing in the standard model, post-prandial
killing). Deploy it with
[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md)
and the same `name`.

For dummy-host interventions, the host-seeking killing is written by
[`deploy_OM_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md),
one dummy host per campaign; `define_OM_vector_control()` keeps what it
needs in `baseList$anophelesmodel` (not written to the XML) and says so
in a message.

**What has been checked in OpenMalaria.** Run through
openMalariaUtilities and checked on the entomological outputs:

- OpenMalaria 44: **standard ITNs deployed by campaigns**: each effect
  (deterrency, pre- and post-prandial killing) changes its own
  parameter, in the in-bed mosquito populations only; Weibull decays
  (exact and fitted); refresh on redeployment; baseline EIR and
  population contributions.

- OpenMalaria 49: standard ITNs (the same effects as in 44);
  **dummy-host nets** (below: timing, coverage, replacement by later
  campaigns, populations reached and lifespan as designed; the decay of
  the added hosts follows the written decay within 0.3% of the initial
  effect); a named list of nets with placeholder decays (identical to
  the nets defined separately); **IRS**, with both `irs_exposure`
  definitions of
  [`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md);
  deployments restricted by **age** and with **cumulative** coverage;
  **continuous** deployment
  ([`om_continuous`](https://swisstph.github.io/AnophelesModel/reference/om_continuous.md)).

The plain-text route
([`get_OM_xml`](https://swisstph.github.io/AnophelesModel/reference/get_OM_xml.md))
writes the same XML as the openMalariaUtilities route (package tests,
and for schema 49). **Not run in OpenMalaria:** deployments restricted
to a sub-population (`subpop`).

**Host-seeking killing (OpenMalaria schema \>= 46).** OpenMalaria's GVI
has no host-seeking killing. In this package it is a dummy host per
covered person, of availability `KillingDuringHostSeekingAbsolute`
\\\times\\ insecticide decay (relative to the human availability
\\\alpha_h\\), on which every mosquito that finds it dies. It is written
as OpenMalaria non-human hosts: for each campaign \\k\\ (date \\t_k\\,
coverage \\c_k\\), an added non-human host `<name>_dummy_<k>`
(`addNonHumanHosts`, deployed at \\t_k\\), of availability relative to
an adult human \\c_k \times\\ `KillingDuringHostSeekingAbsolute` \\/
\alpha_h\\ (\\\alpha_h\\ of the population's model) and biting
probability 0, whose availability decays as the GVI effects do:
attrition \\\times\\ insecticide decay of host-seeking killing, exact
when it is a single OpenMalaria decay, otherwise the Weibull fitted to
their product (one `nonHumanHostsModifications`
`<name>_dummy_<k>_decay`, omitted for a constant decay: an availability
reduction of 1 with an `increasing` decay multiplies the availability by
the decay). As for the GVI, whose components are refreshed (not stacked)
in people who receive them again, a campaign **replaces** the nets of
its recipients: each earlier dummy host is reduced by the coverage
\\c_k\\ of every later campaign \\k\\ (`<name>_dummy_<j>_replaced_<k>`,
constant availability reduction \\c_k\\, the same number or placeholder
as the schedule), which gives the expected effect of campaigns reaching
random, independent shares of the population. Increasing decays need
OpenMalaria schema 46 or later (checked with 49): the `OMVersion` of the
`baseList`. As for the GVI effects, a rising insecticide decay (negative
`halflife_insecticide`) is only exported through the fitted Weibull
(with attrition). Host-seeking killing acts on the same populations as
the GVI (in bed for ITNs).

**Reference human.** OpenMalaria multiplies the relative availability of
an added host by the availability of an average **adult**, the person
sleeping in the experimental hut where host-seeking killing was
measured. Relative to the average human of the population, which is the
human of this package's model, the host-seeking killing is therefore
stronger in OpenMalaria than in this package, by the ratio of adult to
mean human availability (1.35 in the scenario checked, Tanzanian
demography; it depends on the age structure). This choice may be
revisited.

**Placeholders, only where something varies.**

- **Efficacies:** written for one intervention; placeholders
  (`"@<name>_deterrency@"`, `"@<name>_preprandial@"`,
  `"@<name>_postprandial@"`) for a **named list** of several
  interventions (e.g. nets or parameterisations to choose between in
  scenarios). A list defines **one** intervention, deployed under
  `name`; each scenario uses the values of one of its interventions.

- **Decays:** with `decay = "fixed"` (default), written from the
  interventions (which must then have the same decays); with
  `decay = "placeholder"`, `"@<name>_deterrency_L@"` (and `_k` when the
  decay function has a shape parameter), ... Each effect keeps its decay
  function when it is the same for all the interventions (e.g. a `step`
  for historical nets), and is otherwise a Weibull (exact, or fitted).

- **Dummy hosts:** availabilities written when one intervention and
  numeric coverages fix them; otherwise `"@<name>_dummy_<k>_pop<j>@"`
  (campaign \\k\\, mosquito population \\j\\: row of `om_ento$shares`,
  printed by the message). Their decay follows `decay` as an effect of
  its own: with `"placeholder"`, `"@<name>_hsk_L@"` (and `_k`).

All interventions of a list must use the same model structure. Fill the
placeholders of a scenario table with
[`om_scenario_values`](https://swisstph.github.io/AnophelesModel/reference/om_scenario_values.md).

Requires the openMalariaUtilities package (see
[`add_OM_GVI`](https://swisstph.github.io/AnophelesModel/reference/add_OM_GVI.md)).

## Usage

``` r
define_OM_vector_control(
  baseList,
  interv,
  intervention_type = c("ITN", "IRS"),
  name = NULL,
  decay = c("fixed", "placeholder")
)
```

## Arguments

- baseList:

  openMalariaUtilities `baseList`, with the entomology written by
  [`define_OM_entomology`](https://swisstph.github.io/AnophelesModel/reference/define_OM_entomology.md)
  (whose mosquito populations and parameters are used).

- interv:

  Intervention from
  [`om_vector_control`](https://swisstph.github.io/AnophelesModel/reference/om_vector_control.md)
  (or an object from
  [`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)),
  or a named list of them (template).

- intervention_type:

  `"ITN"` (acts in bed) or `"IRS"` (acts on indoor resting mosquitoes,
  or indoor biting ones with
  `get_OM_ento_snippet(irs_exposure = "indoor_biting")`).

- name:

  Name of the intervention, used for component ids and placeholders.
  Default: for a named list, `intervention_type`; for one intervention,
  the `name` of an
  [`om_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/om_vector_control.md)
  specification, else the description of a
  [`build_intervention()`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)
  object unless `"custom"`, else `intervention_type`. In placeholders,
  `<name>` is the name with each run of characters other than letters
  and digits replaced by `_` (e.g. `"Interceptor G2"` gives
  `"@Interceptor_G2_deterrency@"`).

- decay:

  `"fixed"` (default: decays written from the interventions) or
  `"placeholder"` (decay parameters as placeholders, e.g. when the
  attrition varies between scenarios).

## Value

The updated `baseList`.

## See also

[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md),
[`om_scenario_values`](https://swisstph.github.io/AnophelesModel/reference/om_scenario_values.md),
[`get_ITN_params`](https://swisstph.github.io/AnophelesModel/reference/get_ITN_params.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Built-in and custom dummy-host nets (other nets: list_ITN_params())
ig2 = om_vector_control(get_ITN_params("Interceptor G2", "Kibondo",
                                       mode = "dummy_host"),
                        halflife_attrition = 2 * 365, shape_attrition = 2)
custom = om_vector_control(
    c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.5,
      PostPrandial = 0.4),
    halflife_attrition = 2 * 365, shape_attrition = 2)
nets = list(IG2 = ig2, custom = custom)
om_ento  = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
                               activity = def_activity_patterns())
baseList$OMVersion = 49L     # dummy hosts need schema >= 46
baseList = define_OM_entomology(baseList, om_ento, seasonality = rep(1, 12))
baseList = define_OM_vector_control(baseList, nets, name = "ITN",
                                    decay = "placeholder")
schedule = om_campaigns("2021-01-01", "2027-01-01", every = "3 years",
                        coverage = "@cov@", by_year = TRUE,
                        end_date = "2030-01-01")
baseList = deploy_OM_vector_control(baseList, "ITN", schedule)
# The user's scenario table, filled with the values of the placeholders
scens = expand.grid(net = c("IG2", "custom"), attrition = c(2, 3),
                    cov2021 = c(0.5, 0.8), cov2024 = 0.6, cov2027 = 0.6)
scens = om_scenario_values(scens, baseList, "ITN", net = "net",
                           halflife = "attrition")
} # }
```
