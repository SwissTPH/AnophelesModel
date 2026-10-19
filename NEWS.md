# AnophelesModel2 1.0.0

AnophelesModel2 is a refactoring of
[AnophelesModel](https://github.com/SwissTPH/AnophelesModel). It keeps the
entomological model of the mosquito feeding cycle (Chitnis et al.) and the
host / activity databases, but the intervention framework and API were
rewritten and are **not backward compatible**.

## Numerical correctness

* Explicit per-component insecticide decay shapes always take priority over
  linear half-lives. Constant shapes need no decay parameters; incomplete
  non-constant specifications and zero or NaN linear half-lives now raise
  errors identifying the component and parameter-set row.
* Intervention interfaces reject unnamed, duplicate, missing or invalid
  efficacies, out-of-range coverages (including uncertainty draws), and
  non-positive or non-finite durations. Positive subday durations use one
  time point consistently. Host-seeking killing remains a non-negative rate
  that can exceed one; optional batch `NA` values still mean inactive.
* `def_host_params()` orders custom host tables as human then animal, so
  reversing input rows no longer changes host parameters or model results.
  The `species_name` column of custom host tables is optional (host
  parameters are species-independent).
* Incubation-survival sums now include the term at index zero when it is
  their only term. This corrects custom parameterisations with longer
  resting durations or shorter incubation periods; bundled parameterisations
  retain their existing results.
* `calculate_combined_impact()` handles complete deterrency and killing
  without dividing by zero when no mosquitoes reach a feeding stage.
* Impact calculations now raise an error for a zero baseline vectorial
  capacity (relative reduction is undefined) or non-finite results, instead
  of silently reporting zero impact. The message names the time point and the
  parameter sets concerned (and the draw in `run_interventions()`, or the
  intervention in `calculate_impact()`).

## Vector bionomics from AnophelesBionomics

* `vec_ent_param` now comes from
  [AnophelesBionomics](https://github.com/SwissTPH/AnophelesBionomics)
  (posterior means and sds of its hierarchical Bayesian model), replacing the
  older parameterisation. **Results differ from AnophelesModel.**
* Taxa have an explicit `level` (`species`, `complex`, `genus`), a parent
  `complex` and `imputed_traits` (parameters without data of their own).
  `def_vector_params()` gains `level`; `list_all_species()` returns a data
  frame of taxa.
* *An. gambiae s.s.* and *An. coluzzii* are merged as
  `"Anopheles gambiae ss/coluzzii"`, the new default species; the old names
  are accepted with a warning. Complexes are named by their label
  (e.g. `"Gambiae"`).
* `tau` (resting duration) is estimated per taxon from the fed / gravid ratio
  (1.07 days for *An. gambiae ss/coluzzii*, about 2 days for the genus)
  instead of the fixed value 3. In the survival over the extrinsic incubation
  period it is rounded to whole days (at least one); the former silent
  replacement of `tau < 2` by 2 is removed.
* `Chi` is the **indoor** human blood index by default; indoor and outdoor HBI
  are both stored and `def_vector_params(hbi = "outdoor" | "weighted")`
  selects another one.
* `read_bionomics_table()` and `def_vector_params(bionomics_table = )` use a
  newer bionomics table without reinstalling. The dataset is regenerated with
  the scripts in `data-raw/`.

## Export to OpenMalaria

* `get_OM_ento_snippet()` is rewritten: it returns the input lists of
  `openMalariaUtilities::defineEntomology()` (no XML nodes; the XML dependency
  is removed). It fixes the mapping of the original version, which wrote the
  resting duration `tau` as `mosqSeekingDuration` and hardcoded
  `mosqRestDuration = 2`: now `mosqRestDuration` = `tau` in whole days and
  `mosqSeekingDuration` = `td`. With activity patterns, each species is split
  into `_indoor1` / `_indoor2` / `_outdoor` populations by human exposure,
  with indoor / outdoor human blood indices.
* New `get_OM_GVI_snippet()`: generic vector intervention for
  `openMalariaUtilities::defineVectorControl()`, one component per effect with
  its own decay (exact when possible, fitted Weibull for attrition x
  insecticide decay), acting on populations exposed in bed (ITN) or resting
  indoors (IRS). For dummy-host interventions it has deterrency and
  post-prandial killing only (no pre-prandial killing).
* New `define_OM_vector_control()` / `deploy_OM_vector_control()`: write a
  whole ITN / IRS intervention into an openMalariaUtilities `baseList`, with
  one `<deployment>` holding all its components (same people) and one deploy
  per campaign of a `schedule` (dates, numeric or placeholder coverages).
  The define writes the components, the deploy the deployment (the schedule
  is given only there). Host-seeking killing of dummy-host nets is exported
  exactly as the model's dummy host: per campaign, an OpenMalaria added
  non-human host (`addNonHumanHosts`, availability relative to an adult human
  = coverage × `HostSeekingKilling` / human availability, biting probability
  0) whose availability decays with the attrition and the insecticide decay
  of host-seeking killing (`nonHumanHostsModifications` with increasing
  decays; a rising insecticide decay as a negative reduction). Written by the
  deploy (once, with the whole schedule); it needs OpenMalaria schema >= 46
  (developed for 49), from the `baseList`'s `OMVersion`. A named list of
  interventions defines one intervention whose values are chosen per
  scenario, e.g. to loop scenarios over net types.
* Schema version: 44 by default; `get_OM_xml(schema = )` or the
  `baseList`'s `OMVersion`. From 49, the entomology has no `propInfected` /
  `propInfectious` (removed in 49; also dropped by `define_OM_entomology()`).
  Standard interventions are the same in 44 and 49.
* Placeholders only where something varies: efficacies with several nets,
  decays with `define_OM_vector_control(decay = "placeholder")` (otherwise
  written from the nets), dummy-host availabilities unless the net and the
  coverages are fixed.
* New `om_scenario_values()`: fills these placeholders in the user's own
  scenario table, row by row (net from a column, coverages from the columns
  named like the schedule's placeholders), from the `baseList` or the
  `get_OM_xml()` result, which keep what was written. Decays: by default one
  attrition column (years, or `decay_unit = "days"`) combined with each
  effect's insecticide decay (the dummy hosts take the attrition as it is);
  `combine = FALSE` for final decays per effect. Computed once per distinct
  net and decay.
* New `om_vector_control()`: specifies an intervention for the OpenMalaria
  export from its efficacies (or a `get_ITN_params()` parameterisation) and
  decays, without a model or a duration; the export builds it with the
  entomology of the scenario and fits Weibull decays over 3 years (ITN) or
  1 year (IRS) by default (`fit_years`). Intervention objects of
  `build_intervention()` remain accepted.
* New `om_campaigns()`: campaign schedules from regular dates (`from`, `to`,
  `every`, as `seq.Date()`) or given dates, with one coverage (number or
  placeholder), one per campaign, or one placeholder per year
  (`by_year = TRUE`, as `byyear` of `deploy_it_compat()`), and optionally
  the `end_date` of the period (life of the dummy hosts), kept with the
  schedule so that deploy and `get_OM_xml()` need no `end_date` argument.
* In a list of interventions (template), each effect keeps its decay
  function when all the interventions share it (e.g. a step for historical
  nets), with its parameters as placeholders; it is a Weibull only when the
  decay functions differ.
* New `define_OM_entomology()`: writes the entomology with
  `defineEntomology()` (numeric seasonality, `scaledAnnualEIR`) and keeps it
  in the `baseList`; `define_OM_vector_control()` takes the mosquito
  populations and parameters from there (no `om_ento` argument), so the
  interventions are consistent with the entomology.
* `get_OM_ento_snippet()` also returns `muvA` (in `shares`) and the model of
  each mosquito population (`model_params`, whose human availability scales
  the dummy hosts).
* The OpenMalaria export follows the model's priorities for insecticide
  decay: an explicit shape, then a linear half-life (`NA`: unspecified).
* Relative contribution of the species to the EIR: each population's
  `annualEIR` is its relative contribution (species share × share within
  the species), the total EIR being set with `scaledAnnualEIR` on
  `<entomology>`. `eir_shares()` computes species shares from relative
  abundances and sporozoite rates; `get_OM_ento_snippet(species_shares = )`
  writes the contributions as numbers, and without it as placeholders
  `@contrib_<population>@`, filled by `om_ento_values()` for one or several
  species compositions. Shares that do not sum to 1, or do not match the
  taxa, are errors.
* New `get_OM_xml()`: the entomology and vector control as XML text,
  without openMalariaUtilities, to paste into an existing scenario: one
  snippet for the entomology (`<vector>`, same structure as
  `defineEntomology()` but with the animal host parameters of `hosts_p`), and
  per intervention one for the components (define), one for the deployment
  (deploy) and, for dummy-host interventions, the dummy hosts and their
  decays. It shares the list builders of `define_OM_vector_control()` /
  `deploy_OM_vector_control()`, so both routes write the same XML.
* New `add_OM_GVI()`: writes these interventions into an openMalariaUtilities
  `baseList`, as a single component when the three decays are identical and
  one component per effect otherwise (openMalariaUtilities in Suggests).
* `build_intervention()` keeps its inputs in `$settings`.

## New

* `build_intervention()` builds an intervention from user-supplied efficacies
  (`Deterrency`, `PrePrandial`, `PostPrandial`, `HostSeekingKilling`), an
  attrition decay and optional per-component insecticide decay.
* `run_interventions()` evaluates many parameter sets × coverages in one
  vectorised call, with zipped uncertainty draws (`vec_param_sets`,
  `interv_param_sets`, `exposure_sets`, `coverage_sets`).
* `get_ITN_params()` / `list_ITN_params()` load ITN efficacies and insecticide
  decay fitted to experimental hut trials (`ITN_parameterisations_Champagne2025`).
  The dataset has a `fit` column: a trial fitted in several parts (Sovegnon)
  has one control per fit, and host-seeking killing is rescaled by the
  control of the net's own fit.
* **Dummy-host model**: host-seeking killing (`HostSeekingKilling`) adds a
  fourth host type whose availability is the **absolute** killing rate (in
  the units of the host availabilities, not a probability). It is mutually
  exclusive with pre-prandial killing. `get_ITN_params(mode = "dummy_host")`
  converts the hut-trial estimates, which are relative to the control-arm
  availability `alpha_0`, to absolute values by multiplying by `alpha_0` of
  the same arm (`rescale_hsk = FALSE` to skip). The hut model's rates are per
  hut-night while the package uses per-day rates; by provisional convention a
  hut-night is treated as a day (no conversion by the host-seeking duration
  `td`), pending an improved experimental hut fitting model.
* **Insecticide decay**: per-component linear decay (`halflife_insecticide`,
  years, a time-to-zero calibrated so that efficacy after 3 years equals the
  20x-washed value) or any decay shape (`component_decay`). The
  `insecticide_decay = FALSE` switch keeps unwashed efficacies throughout.
  With `TRUE`, `build_intervention()` also returns `fitted_decay`, one Weibull
  per component approximating attrition × insecticide decay.
* `decay_function()` supports `weibull`, `exponential`, `hill`, `linear`,
  `step`, `smooth-compact` and `none`/`constant`, following OpenMalaria
  conventions (`L` is a half-life for weibull/exponential/hill and a time to
  zero for linear/step/smooth-compact); values always stay in [0, 1].
* `calculate_combined_impact()` supports dummy-host interventions
  (host-seeking killing is additive for people covered by both products).

## Changed

* `build_model_obj(vec_p, hosts_p)` no longer takes activity patterns or
  `total_pop`; `Nv0` is no longer an argument anywhere. Host populations are
  unit-sized and vectorial capacity reduction is scale-invariant.
* **Exposure acts on coverage, not on efficacies.** AnophelesModel adjusted
  the intervention effects by indoor/in-bed exposure (`adjustment_for_location`).
  Now `calculate_exposure()` (formerly `get_in_out_exp()`, same calculation)
  gives an exposure factor passed as `exposure` to `calculate_impact()` /
  `run_interventions()`, which use the effective coverage
  `coverage × exposure`.
* `calculate_impact()` always computes impact relative to coverage 0, even if
  `coverage_vec` does not start with 0 (previously relative to its first
  element). `calculate_impact()`, `run_interventions()` and
  `calculate_combined_impact()` share one vectorised engine.
* `calculate_impact()` is `run_interventions()` applied to the parameters
  stored in each intervention (`$param_set`, added by `build_intervention()`)
  and returns the same `data.table` (one row per intervention and coverage,
  with `intervention`, `name`, the parameters, `coverage`, `exposure`,
  `avg_vc`, `avg_impact`) instead of the nested list of AnophelesModel. The
  model parameters all come from `model_p`. Interventions are combined with
  `c()`; objects not made by `build_intervention()` are rejected.
* `keep_time = TRUE` in `calculate_impact()` and `run_interventions()` also
  returns the vectorial capacity and impact on each day, for all parameter
  sets.
* `plot_impact()` replaces `plot_impact_species()` and `plot_impact_var()`: it
  plots the output of both functions, with the median and 95% interval when
  there are uncertainty draws. ggpubr and Hmisc are no longer suggested.
* `calculate_combined_impact()` evaluates both interventions over the shorter
  duration (one point per day) and reports the assigned coverages via
  `message()`.

## Removed

* Intervention database and its functions: `def_interventions_effects()`,
  `get_net_types()`, `get_net_insecticides()`, `get_net_decay()`,
  `list_available_interventions()`, `list_intervention_models()`, and the
  datasets `interventions_param` and `intervention_obj_examples`. Use
  `build_intervention()` / `get_ITN_params()` instead.
* `AnophelesModel()`, `calculate_impact_var()`, `calculate_impact_ip()`,
  `calculate_combined_impact_var()`; use `run_interventions()` for
  variability and uncertainty.
* `get_exposure_multiplier()` (exposure now acts on coverage, see above).
* The XML-node outputs of `get_OM_ento_snippet()` / `get_OM_GVI_snippet()`
  (see "Export to OpenMalaria" above for their replacements).
