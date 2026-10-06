## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(AnophelesModel2)

## -----------------------------------------------------------------------------
head(vec_ent_param)

## -----------------------------------------------------------------------------
gambiae_vec_p = def_vector_params(mosquito_species = "Anopheles gambiae")

## ----eval=FALSE---------------------------------------------------------------
# custom_vec_table = data.frame(
#     species_name  = "Anopheles gambiae",
#     M = 0.623, M.sd = 0.06,    # parous rate
#     Chi = 0.939,                # Human Blood Index (HBI)
#     A0 = 0.313, A0.sd = 0.03,  # sac rate
#     zeta.3 = 1, td = 0.33, tau = 3, ts = 10, to = 5,
#     endophily = 0.813, endophily.sd = 0.07,
#     endophagy = 0.813, endophagy.sd = 0.07
# )
# custom_vec_p = def_vector_params(mosquito_species = "Anopheles gambiae",
#                                  vector_table = custom_vec_table)

## -----------------------------------------------------------------------------
host_p = def_host_params()
print(host_p)

## ----eval=FALSE---------------------------------------------------------------
# custom_host_table = data.frame(
#     host = c("human", "animal"),
#     PBi = c(0.95, 0.70), PCi = c(0.90, 0.70),
#     PDi = c(0.90, 0.80), PEi = c(0.88, 0.80),
#     Kvi = c(0.52, 0.00)
# )
# custom_host_p = def_host_params(host_table = custom_host_table)

## -----------------------------------------------------------------------------
# Built-in default (An. gambiae, Kenya)
activity_gambiae = def_activity_patterns("default_Anopheles_gambiae")

# From database by entry ID
activity_arabiensis = def_activity_patterns(list(
    HBI = 34, HBO = 165, humans_indoors = 4, humans_in_bed = 24
))

## -----------------------------------------------------------------------------
exposure = calculate_exposure(activity_cycles = activity_gambiae,
                              vec_p           = gambiae_vec_p)
print(exposure)

## -----------------------------------------------------------------------------
model_p = build_model_obj(gambiae_vec_p, host_p)
print(model_p$host_params$muvA)    # mosquito death rate while host-seeking
print(model_p$host_params$alphai)  # per-individual-host availability rates

## ----echo=FALSE, fig.cap="Decay function shapes (L=365 days, kappa=2 where applicable)."----
library(ggplot2)
days = 0:(2*365)
decay_df = rbind(
    data.frame(day=days, value=decay_function(L=365,kappa=2,   datej=days,decay="weibull"),       shape="Weibull"),
    data.frame(day=days, value=decay_function(L=365,kappa=NULL,datej=days,decay="exponential"),    shape="Exponential"),
    data.frame(day=days, value=decay_function(L=365,kappa=2,   datej=days,decay="hill"),           shape="Hill"),
    data.frame(day=days, value=decay_function(L=365,kappa=NULL,datej=days,decay="linear"),         shape="Linear"),
    data.frame(day=days, value=decay_function(L=365,kappa=NULL,datej=days,decay="step"),           shape="Step"),
    data.frame(day=days, value=decay_function(L=365,kappa=2,   datej=days,decay="smooth-compact"), shape="Smooth-compact")
)
ggplot(decay_df, aes(x=day, y=value, colour=shape)) +
    geom_line() + theme_bw() +
    labs(x="Day", y="Remaining efficacy", colour="Shape")

## -----------------------------------------------------------------------------
itn_p = get_ITN_params("Interceptor G2", "Kibondo")

my_interv = build_intervention(
    efficacy             = itn_p$efficacy,
    halflife_insecticide = itn_p$halflife_insecticide,
    mode                 = itn_p$mode,
    halflife_attrition =  3*365, shape_attrition = 2, decay_attrition = "weibull",
    duration_years = 3, model_p = model_p, name = itn_p$name
)

## -----------------------------------------------------------------------------
my_interv[[1]]$fitted_decay

## ----eval=FALSE---------------------------------------------------------------
# my_interv_nodecay = build_intervention(
#     efficacy = itn_p$efficacy, halflife_insecticide = itn_p$halflife_insecticide,
#     mode = itn_p$mode, insecticide_decay = FALSE,
#     halflife_attrition = 3*365, shape_attrition = 2, decay_attrition = "weibull",
#     duration_years = 3, model_p = model_p, name = itn_p$name
# )

## ----eval=FALSE---------------------------------------------------------------
# my_interv_custom = build_intervention(
#     efficacy = c(Deterrency = 0.50, PrePrandial = 0.60, PostPrandial = 0.70),
#     halflife_attrition = 3*365, shape_attrition = 2, decay_attrition = "weibull",
#     duration_years = 3, model_p = model_p, name = "Custom ITN"
# )

## -----------------------------------------------------------------------------
itn_dh_p = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host")

my_interv_dh = build_intervention(
    efficacy             = itn_dh_p$efficacy,
    halflife_insecticide = itn_dh_p$halflife_insecticide,
    mode                 = itn_dh_p$mode,
    halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
    duration_years = 3, model_p = model_p, name = itn_dh_p$name
)
# effects matrices now have 4 columns (protected humans, unprotected, animals, dummy host)
ncol(my_interv_dh[[1]]$effects$alphai)

## -----------------------------------------------------------------------------
my_interv_compdecay = build_intervention(
    efficacy = c(Deterrency = 0.50, PrePrandial = 0.60, PostPrandial = 0.70),
    halflife_attrition = 365, shape_attrition = 2, decay_attrition = "weibull",
    duration_years = 3, model_p = model_p,
    name = "ITN (per-component decay)",
    component_decay = list(                           # optional per-component decay
        Deterrency  = list(decay = "hill",   L = 90,  kappa = 3),   # fast
        PrePrandial = list(decay = "linear", L = 730, kappa = NULL)  # slow
    )
)

## -----------------------------------------------------------------------------
# Effective in-bed coverage = deployment coverage × fraction of exposure while in bed
pop_coverage = 0.8
eff_coverage = pop_coverage * exposure$Exposure_Indoor_whileinbed
cat("Effective coverage:", round(eff_coverage, 3), "\n")

coverages = seq(0, 1, by = 0.1)

impacts = calculate_impact(
    interventions_vec = my_interv,
    coverage_vec      = coverages,
    model_p           = model_p
)

## ----dpi=150, fig.cap="Vectorial capacity reduction for the ITN scenario."----
plot_tab = data.frame(
    intervention_name     = "My ITN",
    intervention_coverage = coverages,
    intervention_impact   = impacts$interventions_vec[[1]]$effects$avg_impact
)
plot_impact_var("Anopheles gambiae", plot_tab)

## -----------------------------------------------------------------------------
interv_ITN = build_intervention(
    efficacy = c(Deterrency = 0.5, PrePrandial = 0.6, PostPrandial = 0.7),
    halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
    duration_years = 1, model_p = model_p, name = "ITN"
)
interv_IRS = build_intervention(
    efficacy = c(Deterrency = 0.0, PrePrandial = 0.3, PostPrandial = 0.8),
    halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
    duration_years = 1, model_p = model_p, name = "IRS"
)
combined_impact = calculate_combined_impact(
    combination_name  = "ITN + IRS",
    intervention1     = interv_ITN[[1]], intervention2 = interv_IRS[[1]],
    cov_intervention1 = 0.8,            cov_intervention2 = 0.8,
    model_p = model_p
)
cat("Combined VC reduction:", round(combined_impact * 100, 1), "%\n")

## -----------------------------------------------------------------------------
param_sets = data.frame(
    name         = c("Low efficacy", "Medium efficacy", "High efficacy"),
    deterrency   = c(0.20, 0.4, 0.5),
    preprandial  = c(0.25, 0.55, 0.80),
    postprandial = c(0.20, 0.50, 0.75),
    halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
    stringsAsFactors = FALSE
)

results = run_interventions(
    param_sets   = param_sets,
    model_p      = model_p,
    coverage_vec = seq(0, 1, by = 0.2),
    duration_years = 3
)
# All param_sets columns (deterrency, preprandial, …) are present alongside outputs
head(results)

## ----dpi=150, fig.cap="Vectorial capacity reduction for three efficacy scenarios."----
ggplot(results, aes(x=coverage, y=avg_impact*100, colour=name, group=name)) +
    geom_line(size=0.8) + theme_bw() +
    labs(x="Coverage", y="Mean reduction in VC (%)", colour="Scenario")

## -----------------------------------------------------------------------------
standard_ps = data.frame(
    name         = "Standard ITN",
    deterrency   = 0.55,
    preprandial  = 0.28,
    postprandial = 0.24,
    halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
    stringsAsFactors = FALSE
)
coverage_vec = seq(0, 1, by = 0.2)
res_std = run_interventions(standard_ps, model_p, coverage_vec, duration_years = 3)
res_std[, c("name", "coverage", "avg_impact")]

## -----------------------------------------------------------------------------
dummyhost_ps = data.frame(
    name                 = "Dummy-host ITN",
    deterrency           = 0.55,
    preprandial          = 0,      # required column; set to 0 in dummy-host mode
    postprandial         = 0.24,
    host_seeking_killing = 1.10,   # rate multiplier; values > 1 are valid
    halflife_attrition   = 180, shape_attrition = 2, decay_attrition = "weibull",
    stringsAsFactors     = FALSE
)
res_dh = run_interventions(dummyhost_ps, model_p, coverage_vec, duration_years = 3)
res_dh[, c("name", "coverage", "avg_impact")]

## -----------------------------------------------------------------------------
# Dummy-host model with Weibull decay on host-seeking killing (half-life 90 days)
dummyhost_decay_ps = data.frame(
    name                 = "Dummy-host ITN (with HSK decay)",
    deterrency           = 0.55,
    preprandial          = 0,
    postprandial         = 0.24,
    host_seeking_killing = 1.10,
    halflife_attrition   = 180, shape_attrition = 2, decay_attrition = "weibull",
    decay_hsk = "weibull", L_hsk = 90, kappa_hsk = 2,   # HSK decays faster than attrition
    stringsAsFactors     = FALSE
)
res_dh_decay = run_interventions(dummyhost_decay_ps, model_p, coverage_vec, duration_years = 3)
res_dh_decay[, c("name", "coverage", "avg_impact")]

## -----------------------------------------------------------------------------
# Three scenarios with identical initial efficacy but different component decays
comp_decay_ps = data.frame(
    name         = c("No comp. decay", "Fast deterrency", "Differential decay"),
    deterrency   = 0.55,
    preprandial  = 0.45,
    postprandial = 0.40,
    halflife_attrition = 365, shape_attrition = 2, decay_attrition = "weibull",
    # Scenario 2: deterrency decays fast (Weibull, half-life 60 d, shape 3)
    # Scenario 3: deterrency fast (Weibull), pre-prandial slow (linear, 2 yr), post-prandial medium (linear, 1 yr)
    decay_det  = c(NA,        "weibull", "weibull"),
    L_det      = c(NA,         60,        60),
    kappa_det  = c(NA,          3,         3),
    decay_pre  = c(NA,        NA,        "linear"),
    L_pre      = c(NA,        NA,         730),
    kappa_pre  = c(NA,        NA,         NA),
    decay_post = c(NA,        NA,        "linear"),
    L_post     = c(NA,        NA,         365),
    kappa_post = c(NA,        NA,         NA),
    stringsAsFactors = FALSE
)

comp_results = run_interventions(
    param_sets   = comp_decay_ps,
    model_p      = model_p,
    coverage_vec = seq(0, 1, by = 0.1),
    duration_years = 3
)
head(comp_results[, c("name", "coverage", "avg_impact")])

## ----dpi=150, fig.cap="Effect of per-component insecticide decay on average impact at 70% coverage."----
# Compare with a tolerance: seq() does not produce exactly 0.7
ggplot(comp_results[abs(comp_results$coverage - 0.7) < 1e-8, ],
       aes(x = name, y = avg_impact * 100, fill = name)) +
    geom_col(width = 0.5) + theme_bw() +
    labs(x = NULL, y = "Mean reduction in VC (%)", fill = NULL) +
    theme(legend.position = "none")

## ----dpi=150, fig.cap="Uncertainty band (median + 95% interval) across 200 draws of vector parameters."----
set.seed(42)
n_draws  = 200
base_vec = gambiae_vec_p

vec_draws = data.frame(
    species_name = base_vec$species_name,
    M    = pmax(pmin(rnorm(n_draws, base_vec$M,  0.06), 0.999), 0.001),  # parous rate
    M.sd = base_vec$M.sd,
    Chi  = base_vec$Chi,                                                   # HBI
    A0   = pmax(pmin(rnorm(n_draws, base_vec$A0, 0.03), 0.999), 0.001),  # sac rate
    A0.sd=base_vec$A0.sd, zeta.3=base_vec$zeta.3, td=base_vec$td,
    tau=base_vec$tau, ts=base_vec$ts, to=base_vec$to,
    endophily=base_vec$endophily, endophily.sd=base_vec$endophily.sd,
    endophagy=base_vec$endophagy, endophagy.sd=base_vec$endophagy.sd,
    stringsAsFactors=FALSE
)

single_scenario = data.frame(
    name="Medium efficacy ITN", deterrency=0.45, preprandial=0.55,
    postprandial=0.50, halflife_attrition=180, shape_attrition=2,
    decay_attrition="weibull", stringsAsFactors=FALSE
)
coverage_grid = seq(0, 1, by=0.1)

uncertainty_results = run_interventions(
    param_sets=single_scenario, model_p=model_p, coverage_vec=coverage_grid,
    vec_param_sets=vec_draws, duration_years=3
)

# Each row carries both the intervention inputs and the draw-specific vector
# parameters (M, Chi, A0, …) alongside avg_vc, avg_impact, and draw.
head(uncertainty_results)

library(dplyr)
unc_summary = uncertainty_results %>%
    group_by(coverage) %>%
    summarise(med=median(avg_impact), lo=quantile(avg_impact,0.025),
              hi=quantile(avg_impact,0.975), .groups="drop")

ggplot(unc_summary, aes(x=coverage)) +
    geom_ribbon(aes(ymin=lo*100, ymax=hi*100), alpha=0.3, fill="#3182bd") +
    geom_line(aes(y=med*100), colour="#3182bd", size=1) + theme_bw() +
    labs(x="Coverage", y="Mean reduction in VC (%)",
         subtitle="Median + 95% interval, 200 draws (M sd=0.06, A0 sd=0.03)")

## -----------------------------------------------------------------------------
set.seed(7)
n_draws_interv = 50
interv_draws = data.frame(
    name         = "ITN",
    deterrency   = pmin(pmax(rnorm(n_draws_interv, 0.45, 0.08), 0), 1),
    preprandial  = pmin(pmax(rnorm(n_draws_interv, 0.55, 0.10), 0), 1),
    postprandial = pmin(pmax(rnorm(n_draws_interv, 0.50, 0.09), 0), 1),
    halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull"
)

interv_unc_results = run_interventions(
    param_sets=single_scenario, model_p=model_p, coverage_vec=coverage_grid,
    interv_param_sets=interv_draws, duration_years=3
)
cat("Output rows:", nrow(interv_unc_results),
    "= 1 scenario x", length(coverage_grid), "coverage x", n_draws_interv, "draws\n")
# Each row carries the draw-specific input parameters (deterrency, preprandial, …)
# alongside avg_vc, avg_impact, and draw — no re-joining needed.
head(interv_unc_results)

## -----------------------------------------------------------------------------
set.seed(99)
n_draws_joint = 30
joint_results = run_interventions(
    param_sets=single_scenario, model_p=model_p, coverage_vec=coverage_grid,
    vec_param_sets    = vec_draws[seq_len(n_draws_joint), ],
    interv_param_sets = interv_draws[seq_len(n_draws_joint), ],
    duration_years=3
)
cat("Joint rows:", nrow(joint_results),
    "= 1 x", length(coverage_grid), "x", n_draws_joint, "\n")

## -----------------------------------------------------------------------------
xml_snippets = get_OM_ento_snippet(gambiae_vec_p, host_p)
print(xml_snippets)

