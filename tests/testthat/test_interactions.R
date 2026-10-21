library(AnophelesModel2)
library(testthat)

# Interaction tests: every combination of model structure x decay type is
# checked through every entry point (calculate_impact, run_interventions,
# calculate_combined_impact, OpenMalaria export) and options (exposure,
# uncertainty draws, keep_time, several parameter sets), against the
# independent reference loop (helper-reference.R) and invariants that must
# always hold (limit cases, direction, consistency between entry points).

gss     = "Anopheles gambiae ss/coluzzii"
model_p = build_model_obj(def_vector_params(gss, verbose = FALSE),
                          def_host_params(verbose = FALSE))
days    = 90                                  # short interventions: fast
cov     = c(0, 0.3, 0.7, 1)

efficacy_of = function(mode, scale = 1) scale * switch(mode,
    standard   = c(Deterrency = 0.3, PrePrandial = 0.4, PostPrandial = 0.2),
    dummy_host = c(Deterrency = 0.3, HostSeekingKilling = 0.5, PostPrandial = 0.2))

# Decay cases: attrition shapes, a general component decay, a rising
# (negative) linear insecticide decay. `ins` lists the effects with an
# insecticide decay (the others decay with the attrition only).
decay_cases = list(
    none        = list(args = list(decay_attrition = "none"), ins = character(0)),
    weibull     = list(args = list(decay_attrition = "weibull", halflife_attrition = 60,
                                   shape_attrition = 2), ins = character(0)),
    linear      = list(args = list(decay_attrition = "linear", halflife_attrition = 120),
                       ins = character(0)),
    # Time to zero between two days: the export writes L in years with 6
    # significant digits, which only matters exactly at a whole-day boundary
    step        = list(args = list(decay_attrition = "step", halflife_attrition = 45.5),
                       ins = character(0)),
    exponential = list(args = list(decay_attrition = "weibull", halflife_attrition = 60,
                                   shape_attrition = 2,
                                   component_decay = list(PostPrandial = list(
                                       decay = "exponential", L = 30))),
                       ins = "PostPrandial"),
    rising      = list(args = list(decay_attrition = "weibull", halflife_attrition = 60,
                                   shape_attrition = 2,
                                   halflife_insecticide = c(Deterrency = -2)),
                       ins = "Deterrency"))

grid = expand.grid(mode = c("standard", "dummy_host"), decay = names(decay_cases),
                   stringsAsFactors = FALSE)
label = function(i) paste(grid$mode[i], grid$decay[i])

build_case = function(mode, decay, scale = 1, duration = days)
    suppressMessages(do.call(build_intervention, c(
        list(efficacy = efficacy_of(mode, scale), duration_years = duration / 365,
             model_p = model_p, mode = mode, name = paste(mode, decay)),
        decay_cases[[decay]]$args)))

impact_of = function(iv, coverage = cov, ...)
    calculate_impact(iv, coverage, model_p, ...)$avg_impact

# ---------------------------------------------------------------------------
# 1. Entry points agree, for every model structure x decay
# ---------------------------------------------------------------------------
test_that("calculate_impact = reference loop = run_interventions, for every case", {
    for (i in seq_len(nrow(grid))) {
        iv  = build_case(grid$mode[i], grid$decay[i])
        new = calculate_impact(iv, cov, model_p, keep_time = TRUE)
        ref = reference_calculate_impact(iv, cov, model_p)$interventions_vec[[1]]$effects
        expect_equal(new$impact$avg_impact, unname(ref$avg_impact), tolerance = 1e-10,
                     info = label(i))
        expect_equal(new$impact$avg_vc, unname(ref$avg_vc), tolerance = 1e-10,
                     info = label(i))
        batch = suppressMessages(run_interventions(iv[[1]]$param_set, model_p, cov,
                                                   duration_years = days / 365))
        expect_equal(batch$avg_impact, new$impact$avg_impact, tolerance = 1e-12,
                     info = label(i))
        # keep_time: the average of the daily impacts is the average impact
        daily = tapply(new$time$impact, new$time$coverage, mean)
        expect_equal(as.vector(daily[as.character(cov)]), new$impact$avg_impact,
                     tolerance = 1e-12, info = label(i))
    }
})

# ---------------------------------------------------------------------------
# 2. Limit cases and direction, for every case
# ---------------------------------------------------------------------------
test_that("limit cases hold for every case", {
    for (i in seq_len(nrow(grid))) {
        imp = impact_of(build_case(grid$mode[i], grid$decay[i]))
        # Coverage 0: no impact
        expect_equal(imp[1], 0, tolerance = 1e-12, info = label(i))
        # Zero efficacy: no impact at any coverage
        expect_equal(impact_of(build_case(grid$mode[i], grid$decay[i], scale = 0)),
                     rep(0, length(cov)), tolerance = 1e-12, info = label(i))
    }
})

test_that("impact never decreases with coverage or efficacy, for every case", {
    for (i in seq_len(nrow(grid))) {
        full = impact_of(build_case(grid$mode[i], grid$decay[i]))
        half = impact_of(build_case(grid$mode[i], grid$decay[i], scale = 0.5))
        expect_true(all(diff(full) >= -1e-12), info = label(i))
        expect_true(all(half <= full + 1e-12), info = label(i))
        expect_gt(full[length(cov)], 0, label = label(i))
    }
})

test_that("faster attrition never gives a higher impact", {
    for (mode in c("standard", "dummy_host")) {
        slow = suppressMessages(build_intervention(
            efficacy_of(mode), halflife_attrition = 60, shape_attrition = 2,
            duration_years = days / 365, model_p = model_p, mode = mode))
        fast = suppressMessages(build_intervention(
            efficacy_of(mode), halflife_attrition = 20, shape_attrition = 2,
            duration_years = days / 365, model_p = model_p, mode = mode))
        expect_true(all(impact_of(fast) <= impact_of(slow) + 1e-12), info = mode)
    }
})

# ---------------------------------------------------------------------------
# 3. Options: exposure, uncertainty draws, several parameter sets
# ---------------------------------------------------------------------------
test_that("exposure equals scaling the coverage, for every case", {
    for (i in seq_len(nrow(grid))) {
        iv = build_case(grid$mode[i], grid$decay[i])
        expect_equal(impact_of(iv, exposure = 0.6), impact_of(iv, coverage = cov * 0.6),
                     tolerance = 1e-12, info = label(i))
    }
})

test_that("uncertainty draws equal separate runs, for every case", {
    for (i in seq_len(nrow(grid))) {
        ps = build_case(grid$mode[i], grid$decay[i])[[1]]$param_set
        drawn = suppressMessages(run_interventions(ps, model_p, cov,
            exposure_sets = c(1, 0.6), duration_years = days / 365))
        for (d in 1:2) {
            single = suppressMessages(run_interventions(ps, model_p, cov,
                exposure = c(1, 0.6)[d], duration_years = days / 365))
            expect_equal(drawn$avg_impact[drawn$draw == d], single$avg_impact,
                         tolerance = 1e-12, info = paste(label(i), "draw", d))
        }
    }
})

test_that("a batch of all decay cases equals the separate runs, per model structure", {
    for (mode in c("standard", "dummy_host")) {
        cases = names(decay_cases)
        sets  = lapply(cases, function(dc) build_case(mode, dc)[[1]]$param_set)
        batch = suppressMessages(run_interventions(
            as.data.frame(data.table::rbindlist(sets, fill = TRUE)), model_p, cov,
            duration_years = days / 365))
        for (k in seq_along(cases)) {
            rows = (k - 1) * length(cov) + seq_along(cov)
            single = suppressMessages(run_interventions(sets[[k]], model_p, cov,
                                                        duration_years = days / 365))
            expect_equal(batch$avg_impact[rows], single$avg_impact, tolerance = 1e-12,
                         info = paste(mode, cases[k]))
        }
    }
})

# ---------------------------------------------------------------------------
# 4. Combined interventions, for every case
# ---------------------------------------------------------------------------
test_that("combining with a null intervention gives the single one, for every case", {
    none = build_case("standard", "weibull", scale = 0)[[1]]
    for (i in seq_len(nrow(grid))) {
        iv = build_case(grid$mode[i], grid$decay[i])
        single = impact_of(iv, coverage = 0.7)
        combined = suppressMessages(calculate_combined_impact(
            intervention1 = iv[[1]], intervention2 = none,
            cov_intervention1 = 0.7, cov_intervention2 = 0.5, model_p = model_p))
        expect_equal(combined, single, tolerance = 1e-10, info = label(i))
        # Order does not matter
        swapped = suppressMessages(calculate_combined_impact(
            intervention1 = none, intervention2 = iv[[1]],
            cov_intervention1 = 0.5, cov_intervention2 = 0.7, model_p = model_p))
        expect_equal(swapped, combined, tolerance = 1e-10, info = label(i))
    }
})

test_that("any two cases combine symmetrically, within [0, 1], with coverage 0 = single", {
    # (Combined is not required to exceed each alone: deterrency of one
    # product can divert mosquitoes from the killing of the other.)
    ivs = lapply(seq_len(nrow(grid)), function(i) build_case(grid$mode[i], grid$decay[i])[[1]])
    comb = function(a, b, ca, cb) suppressMessages(calculate_combined_impact(
        intervention1 = a, intervention2 = b, cov_intervention1 = ca,
        cov_intervention2 = cb, model_p = model_p))
    for (i in seq_len(nrow(grid))) for (j in seq_len(nrow(grid))) {
        info = paste(label(i), "+", label(j))
        both = comb(ivs[[i]], ivs[[j]], 0.6, 0.4)
        expect_true(is.finite(both) && both >= 0 && both <= 1, info = info)
        expect_equal(comb(ivs[[j]], ivs[[i]], 0.4, 0.6), both, tolerance = 1e-10, info = info)
        # The second at coverage 0: the first alone
        expect_equal(comb(ivs[[i]], ivs[[j]], 0.6, 0),
                     impact_of(list(ivs[[i]]), coverage = 0.6), tolerance = 1e-10, info = info)
    }
})

# ---------------------------------------------------------------------------
# 5. OpenMalaria export reproduces the package's decays (standard model)
# ---------------------------------------------------------------------------
test_that("exported GVI decays reproduce the package's curves where exact", {
    om = suppressMessages(get_OM_ento_snippet(gss))
    t  = seq_len(days) - 1
    for (dc in names(decay_cases)) {
        iv  = build_case("standard", dc)
        gvi = suppressWarnings(suppressMessages(get_OM_GVI_snippet(iv, om, "ITN")))[[1]]
        effects = c(Deterrency = "deterrency", PrePrandial = "preprandialKillingEffect",
                    PostPrandial = "postprandialKillingEffect")
        # Effects without insecticide decay: the exported decay is the attrition
        for (comp in setdiff(names(effects), decay_cases[[dc]]$ins)) {
            d = gvi[[effects[[comp]]]]$decay
            curve = decay_function(L = if (!is.null(d$L)) 365 * as.numeric(d$L),
                                   kappa = if (!is.null(d$k)) as.numeric(d$k),
                                   datej = t, decay = d[["function"]])
            expect_equal(curve, iv[[1]]$effects$survival, tolerance = 1e-5,
                         info = paste(dc, comp))
        }
        # Initial values: the efficacies
        for (comp in names(effects))
            expect_equal(as.numeric(gvi[[effects[[comp]]]]$anophelesParams[[1]]$value),
                         unname(efficacy_of("standard")[[comp]]), info = paste(dc, comp))
    }
})
