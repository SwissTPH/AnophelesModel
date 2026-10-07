# AnophelesModel2 0.1.0

AnophelesModel2 is a refactoring of
[AnophelesModel](https://github.com/SwissTPH/AnophelesModel). It keeps the
entomological model of the mosquito feeding cycle (Chitnis et al.) and the
bionomics / activity databases, but the intervention framework and API were
rewritten and are **not backward compatible**.

## New

* `build_intervention()` builds an intervention from user-supplied efficacies
  (`Deterrency`, `PrePrandial`, `PostPrandial`, `HostSeekingKilling`), an
  attrition decay and optional per-component insecticide decay.
* `run_interventions()` evaluates many parameter sets × coverages in one
  vectorised call, with zipped uncertainty draws (`vec_param_sets`,
  `interv_param_sets`, `exposure_sets`, `coverage_sets`).
* `get_ITN_params()` / `list_ITN_params()` load ITN efficacies and insecticide
  decay fitted to experimental hut trials (`ITN_parameterisations_Champagne2025`).
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
* `get_OM_GVI_snippet()`: the OpenMalaria intervention export is being
  redesigned for the new framework. `get_OM_ento_snippet()` is unchanged.
