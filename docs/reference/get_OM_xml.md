# OpenMalaria XML snippets, as text

Writes the entomology and the vector control of an OpenMalaria scenario
(schema 44, or `schema`) as XML text, to paste into an existing
scenario, without openMalariaUtilities. The vector control snippets are
those that
[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)
and
[`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md)
write into a `baseList` (same lists and checks):

- `entomology`: the `<vector>` element, with one `<anopheles>` per
  mosquito population of `om_ento` and the declaration of the non-human
  hosts, to paste inside `<entomology>` (replacing its `<vector>`). Same
  structure as
  [`openMalariaUtilities::defineEntomology()`](https://swisstph.github.io/r-openMalariaUtilities/reference/defineEntomology.html),
  but with the animal host parameters of `om_ento` (`defineEntomology()`
  writes fixed values); the placeholders of
  `om_ento$seasonalityParameters` are kept (contributions
  `"@contrib_<population>@"` and monthly `"@m1@"`, ..., `"@m12@"`). The
  `annualEIR` values are relative contributions: add `scaledAnnualEIR`
  (the total EIR) to `<entomology>`;

- `define`: the `<component>` elements of the intervention, to paste
  inside `<interventions><human>`;

- `deploy`: its `<deployment>` (all components, so that all effects
  reach the same people, and one `<deploy>` per campaign), to paste
  inside `<interventions><human>` after all `<component>` elements
  (OpenMalaria requires the components first);

- `add_hosts` and `modify_hosts`: dummy-host interventions only (schema
  \>= 46), the host-seeking killing as one dummy host per campaign
  (`<nonHumanHosts>` elements, with their deployment dates, to paste
  inside `<interventions><addNonHumanHosts>`) and the decays of their
  availability (`<intervention>` elements, to paste inside
  `<interventions><nonHumanHostsModifications>`); see
  [`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md).

Placeholders (coverages, and the values that vary, as in
[`define_OM_vector_control()`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md))
are kept in the text; `om_scenario_values(scenarios, xml)` fills them in
a scenario table (the result keeps the record of the intervention and
its schedule). Printing the result shows each snippet with a comment
saying where to paste it. For several interventions, call the function
once per intervention (with `entomology = FALSE` after the first).

## Usage

``` r
get_OM_xml(
  om_ento,
  interv = NULL,
  schedule = NULL,
  end_date = NULL,
  intervention_type = c("ITN", "IRS"),
  name = NULL,
  subpop = NULL,
  cumulative = FALSE,
  minAge = NULL,
  maxAge = NULL,
  entomology = TRUE,
  decay = c("fixed", "placeholder"),
  schema = 44
)

# S3 method for class 'om_xml'
print(x, ...)
```

## Arguments

- om_ento:

  Output of
  [`get_OM_ento_snippet`](https://swisstph.github.io/AnophelesModel/reference/get_OM_ento_snippet.md):
  written as the entomology snippet, and used for the mosquito
  populations of the interventions.

- interv:

  Intervention from
  [`om_vector_control`](https://swisstph.github.io/AnophelesModel/reference/om_vector_control.md)
  (or an object from
  [`build_intervention`](https://swisstph.github.io/AnophelesModel/reference/build_intervention.md)),
  or a named list of them (template, see
  [`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md)).
  `NULL` (default): entomology only.

- schedule, end_date:

  As in
  [`deploy_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/deploy_OM_vector_control.md):
  `schedule` is needed with `interv`.

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

- entomology:

  Logical (default `TRUE`): include the entomology snippet.

- decay:

  `"fixed"` (default: decays written from the interventions) or
  `"placeholder"` (decay parameters as placeholders, e.g. when the
  attrition varies between scenarios).

- schema:

  OpenMalaria schema version of the scenario (default 44). Dummy-host
  interventions need 46 or later (checked with 49); from 49, the
  entomology has no `propInfected` / `propInfectious`.

- x:

  Object of class `om_xml`.

- ...:

  Not used.

## Value

Object of class `om_xml`: named list with `entomology`, `define`,
`deploy`, `add_hosts` and `modify_hosts`, each a single string (XML
text) or `NULL`.

## See also

[`define_OM_vector_control`](https://swisstph.github.io/AnophelesModel/reference/define_OM_vector_control.md),
[`om_scenario_values`](https://swisstph.github.io/AnophelesModel/reference/om_scenario_values.md)

## Examples

``` r
itn = om_vector_control(
    c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
    halflife_attrition = 2 * 365, shape_attrition = 2, name = "ITN")
om_ento = get_OM_ento_snippet("Anopheles gambiae ss/coluzzii",
                              activity = def_activity_patterns())
#> get_OM_ento_snippet: annualEIR = a placeholder @contrib_<population>@ for the relative contribution of each population (values: om_ento_values()); set the total EIR with scaledAnnualEIR (e.g. defineEntomology(..., scaledAnnualEIR = "@EIR@")).
schedule = data.frame(date = c("2021-01-01", "2024-01-01"),
                      coverage = c("@cov2021@", "@cov2024@"))
xml = get_OM_xml(om_ento, itn, schedule)
xml
#> <!-- entomology: paste inside <entomology>, replacing its <vector> element; annualEIR are relative contributions: set the total EIR with <entomology ... scaledAnnualEIR="@EIR@"> -->
#> <vector>
#>   <anopheles mosquito="Anopheles gambiae ss/coluzzii_indoor1" propInfected="0.078" propInfectious="0.021">
#>     <seasonality annualEIR="@contrib_Anopheles_gambiae_ss_coluzzii_indoor1@" input="EIR">
#>       <monthlyValues smoothing="fourier">
#>         <value>@m1@</value>
#>         <value>@m2@</value>
#>         <value>@m3@</value>
#>         <value>@m4@</value>
#>         <value>@m5@</value>
#>         <value>@m6@</value>
#>         <value>@m7@</value>
#>         <value>@m8@</value>
#>         <value>@m9@</value>
#>         <value>@m10@</value>
#>         <value>@m11@</value>
#>         <value>@m12@</value>
#>       </monthlyValues>
#>     </seasonality>
#>     <mosq minInfectedThreshold="0.001">
#>       <mosqRestDuration value="1"/>
#>       <extrinsicIncubationPeriod value="10"/>
#>       <mosqLaidEggsSameDayProportion value="0.620567"/>
#>       <mosqSeekingDuration value="0.33"/>
#>       <mosqSurvivalFeedingCycleProbability value="0.613252"/>
#>       <availability distr="const"/>
#>       <mosqProbBiting mean="0.95" variance="0"/>
#>       <mosqProbFindRestSite mean="0.95" variance="0"/>
#>       <mosqProbResting mean="0.99" variance="0"/>
#>       <mosqProbOvipositing value="0.88"/>
#>       <mosqHumanBloodIndex value="0.864221"/>
#>     </mosq>
#>     <nonHumanHosts name="unprotectedAnimals">
#>       <mosqRelativeEntoAvailability value="1"/>
#>       <mosqProbBiting value="0.95"/>
#>       <mosqProbFindRestSite value="0.95"/>
#>       <mosqProbResting value="0.99"/>
#>     </nonHumanHosts>
#>   </anopheles>
#>   <anopheles mosquito="Anopheles gambiae ss/coluzzii_indoor2" propInfected="0.078" propInfectious="0.021">
#>     <seasonality annualEIR="@contrib_Anopheles_gambiae_ss_coluzzii_indoor2@" input="EIR">
#>       <monthlyValues smoothing="fourier">
#>         <value>@m1@</value>
#>         <value>@m2@</value>
#>         <value>@m3@</value>
#>         <value>@m4@</value>
#>         <value>@m5@</value>
#>         <value>@m6@</value>
#>         <value>@m7@</value>
#>         <value>@m8@</value>
#>         <value>@m9@</value>
#>         <value>@m10@</value>
#>         <value>@m11@</value>
#>         <value>@m12@</value>
#>       </monthlyValues>
#>     </seasonality>
#>     <mosq minInfectedThreshold="0.001">
#>       <mosqRestDuration value="1"/>
#>       <extrinsicIncubationPeriod value="10"/>
#>       <mosqLaidEggsSameDayProportion value="0.620567"/>
#>       <mosqSeekingDuration value="0.33"/>
#>       <mosqSurvivalFeedingCycleProbability value="0.613252"/>
#>       <availability distr="const"/>
#>       <mosqProbBiting mean="0.95" variance="0"/>
#>       <mosqProbFindRestSite mean="0.95" variance="0"/>
#>       <mosqProbResting mean="0.99" variance="0"/>
#>       <mosqProbOvipositing value="0.88"/>
#>       <mosqHumanBloodIndex value="0.864221"/>
#>     </mosq>
#>     <nonHumanHosts name="unprotectedAnimals">
#>       <mosqRelativeEntoAvailability value="1"/>
#>       <mosqProbBiting value="0.95"/>
#>       <mosqProbFindRestSite value="0.95"/>
#>       <mosqProbResting value="0.99"/>
#>     </nonHumanHosts>
#>   </anopheles>
#>   <anopheles mosquito="Anopheles gambiae ss/coluzzii_outdoor" propInfected="0.078" propInfectious="0.021">
#>     <seasonality annualEIR="@contrib_Anopheles_gambiae_ss_coluzzii_outdoor@" input="EIR">
#>       <monthlyValues smoothing="fourier">
#>         <value>@m1@</value>
#>         <value>@m2@</value>
#>         <value>@m3@</value>
#>         <value>@m4@</value>
#>         <value>@m5@</value>
#>         <value>@m6@</value>
#>         <value>@m7@</value>
#>         <value>@m8@</value>
#>         <value>@m9@</value>
#>         <value>@m10@</value>
#>         <value>@m11@</value>
#>         <value>@m12@</value>
#>       </monthlyValues>
#>     </seasonality>
#>     <mosq minInfectedThreshold="0.001">
#>       <mosqRestDuration value="1"/>
#>       <extrinsicIncubationPeriod value="10"/>
#>       <mosqLaidEggsSameDayProportion value="0.620567"/>
#>       <mosqSeekingDuration value="0.33"/>
#>       <mosqSurvivalFeedingCycleProbability value="0.613252"/>
#>       <availability distr="const"/>
#>       <mosqProbBiting mean="0.95" variance="0"/>
#>       <mosqProbFindRestSite mean="0.95" variance="0"/>
#>       <mosqProbResting mean="0.99" variance="0"/>
#>       <mosqProbOvipositing value="0.88"/>
#>       <mosqHumanBloodIndex value="0.700805"/>
#>     </mosq>
#>     <nonHumanHosts name="unprotectedAnimals">
#>       <mosqRelativeEntoAvailability value="1"/>
#>       <mosqProbBiting value="0.95"/>
#>       <mosqProbFindRestSite value="0.95"/>
#>       <mosqProbResting value="0.99"/>
#>     </nonHumanHosts>
#>   </anopheles>
#>   <nonHumanHosts name="unprotectedAnimals" number="1"/>
#> </vector>
#> 
#> <!-- define: paste inside <interventions><human>, with the other <component> elements -->
#> <component id="ITN" name="ITN">
#>   <GVI>
#>     <decay L="2" function="weibull" k="2"/>
#>     <anophelesParams mosquito="Anopheles gambiae ss/coluzzii_indoor1" propActive="1">
#>       <deterrency value="0.5"/>
#>       <preprandialKillingEffect value="0.4"/>
#>       <postprandialKillingEffect value="0.3"/>
#>     </anophelesParams>
#>     <anophelesParams mosquito="Anopheles gambiae ss/coluzzii_indoor2" propActive="1">
#>       <deterrency value="0.5"/>
#>       <preprandialKillingEffect value="0.4"/>
#>       <postprandialKillingEffect value="0.3"/>
#>     </anophelesParams>
#>     <anophelesParams mosquito="Anopheles gambiae ss/coluzzii_outdoor" propActive="0">
#>       <deterrency value="0"/>
#>       <preprandialKillingEffect value="0"/>
#>       <postprandialKillingEffect value="0"/>
#>     </anophelesParams>
#>   </GVI>
#> </component>
#> 
#> <!-- deploy: paste inside <interventions><human>, after all <component> elements (deploys the components above together) -->
#> <deployment name="ITN">
#>   <component id="ITN"/>
#>   <timed>
#>     <deploy coverage="@cov2021@" time="2021-01-01"/>
#>     <deploy coverage="@cov2024@" time="2024-01-01"/>
#>   </timed>
#> </deployment>
#> 
if (FALSE) { # \dontrun{
writeLines(unlist(xml), "ITN_snippets.xml")
} # }
```
