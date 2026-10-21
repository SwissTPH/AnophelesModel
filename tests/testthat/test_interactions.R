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
    # Time to zero on a whole day: the export rounds L towards zero (it was
    # rounded up with 6 digits, keeping the protection at day 45)
    step        = list(args = list(decay_attrition = "step", halflife_attrition = 45),
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

# ---------------------------------------------------------------------------
# 6. Row and coverage ordering
# ---------------------------------------------------------------------------
test_that("reordered parameter rows and unsorted coverages give the same results", {
    for (mode in c("standard", "dummy_host")) {
        cases = c("weibull", "step", "rising")
        ps = as.data.frame(data.table::rbindlist(lapply(cases, function(dc)
            build_case(mode, dc)[[1]]$param_set), fill = TRUE))
        ps$name = cases
        run = function(p, coverage) suppressMessages(run_interventions(
            p, model_p, coverage, duration_years = days / 365, keep_time = TRUE))
        sorted   = run(ps, c(0, 0.3, 0.7, 1))
        shuffled = run(ps[c(3, 1, 2), ], c(1, 0.3, 0, 0.7))   # 0 not first
        key = c("name", "coverage")
        m = merge(as.data.frame(sorted$impact)[c(key, "avg_vc", "avg_impact")],
                  as.data.frame(shuffled$impact)[c(key, "avg_vc", "avg_impact")],
                  by = key, suffixes = c(".s", ".u"))
        expect_equal(nrow(m), length(cases) * 4L, info = mode)
        expect_equal(m$avg_impact.u, m$avg_impact.s, tolerance = 1e-12, info = mode)
        expect_equal(m$avg_vc.u, m$avg_vc.s, tolerance = 1e-12, info = mode)
        expect_equal(m$avg_impact.u[m$coverage == 0], rep(0, length(cases)), info = mode)
        # Daily outputs
        dkey = c("name", "coverage", "day")
        md = merge(as.data.frame(sorted$time)[c(dkey, "vc", "impact")],
                   as.data.frame(shuffled$time)[c(dkey, "vc", "impact")],
                   by = dkey, suffixes = c(".s", ".u"))
        expect_equal(nrow(md), length(cases) * 4L * days, info = mode)
        expect_equal(md$impact.u, md$impact.s, tolerance = 1e-12, info = mode)
        expect_equal(md$vc.u, md$vc.s, tolerance = 1e-12, info = mode)
        # calculate_impact, unsorted coverages
        iv = build_case(mode, "weibull")
        a = calculate_impact(iv, c(0, 0.3, 0.7, 1), model_p)
        b = calculate_impact(iv, c(1, 0.3, 0, 0.7), model_p)
        expect_equal(b$avg_impact[match(a$coverage, b$coverage)], a$avg_impact,
                     tolerance = 1e-12, info = mode)
    }
})

# ---------------------------------------------------------------------------
# 7. A combined decay with a known analytical answer
# ---------------------------------------------------------------------------
test_that("exponential attrition x exponential insecticide decay = 20-day half-life", {
    # Half-lives 60 and 30 days: 2^(-t/60) x 2^(-t/30) = 2^(-t/20)
    t  = 0:119
    expected = 0.4 * exp(-log(2) * t / 20)
    iv = suppressMessages(build_intervention(
        c(Deterrency = 0, PrePrandial = 0.4, PostPrandial = 0),
        decay_attrition = "exponential", halflife_attrition = 60,
        component_decay = list(PrePrandial = list(decay = "exponential", L = 30)),
        duration_years = length(t) / 365, model_p = model_p))[[1]]
    # The package: population-level protection = survival x efficacy reduction
    e = iv$effects
    protection = e$survival * (1 - e$PBi[, 1] / e$PBi[, 2])
    expect_equal(protection, expected, tolerance = 1e-12)
    # The export: a single Weibull (fitted to the product), ~ exponential, L = 20 days
    om  = suppressMessages(get_OM_ento_snippet(gss))
    gvi = suppressMessages(get_OM_GVI_snippet(list(iv), om, "ITN"))[[1]]
    pre = gvi$preprandialKillingEffect
    d   = pre$decay
    expect_equal(d[["function"]], "weibull")
    expect_equal(as.numeric(d$L) * 365, 20, tolerance = 1e-3)
    expect_equal(as.numeric(d$k), 1, tolerance = 1e-3)
    curve = decay_function(L = 365 * as.numeric(d$L), kappa = as.numeric(d$k),
                           datej = t, decay = "weibull")
    expect_equal(as.numeric(pre$anophelesParams[[1]]$value) * curve, expected,
                 tolerance = 1e-3)
})

test_that("a product that is not a Weibull is exported as a close fitted Weibull", {
    # Weibull attrition (60 days, k = 2) x exponential insecticide decay (30
    # days): no closed-form Weibull, the export fits one. The package keeps
    # the exact product.
    t  = 0:(3 * 365 - 1)
    exact = 0.4 * exp(-log(2) * ((t / 60)^2 + t / 30))
    iv = suppressMessages(build_intervention(
        c(Deterrency = 0, PrePrandial = 0.4, PostPrandial = 0),
        decay_attrition = "weibull", halflife_attrition = 60, shape_attrition = 2,
        component_decay = list(PrePrandial = list(decay = "exponential", L = 30)),
        duration_years = 3, model_p = model_p))[[1]]
    e = iv$effects
    expect_equal(e$survival * (1 - e$PBi[, 1] / e$PBi[, 2]), exact, tolerance = 1e-12)

    om  = suppressMessages(get_OM_ento_snippet(gss))
    gvi = suppressMessages(get_OM_GVI_snippet(list(iv), om, "ITN"))[[1]]
    d   = gvi$preprandialKillingEffect$decay
    fd  = iv$fitted_decay[iv$fitted_decay$component == "PrePrandial", ]
    # The export writes the package's fit (L in years)...
    expect_equal(d[["function"]], "weibull")
    expect_equal(as.numeric(d$L) * 365, fd$L, tolerance = 1e-5)
    expect_equal(as.numeric(d$k), fd$kappa, tolerance = 1e-5)
    # ... which stays close to the exact product: within 5% of the initial
    # effect at every day
    fitted = 0.4 * decay_function(L = 365 * as.numeric(d$L), kappa = as.numeric(d$k),
                                  datej = t, decay = "weibull")
    expect_lt(max(abs(fitted - exact)), 0.05 * 0.4)
    expect_lt(fd$rmse, 0.05)
})

# ---------------------------------------------------------------------------
# 8. Overlap boundaries of combined interventions
# ---------------------------------------------------------------------------
test_that("combined impact at the smallest and largest overlaps, and just outside", {
    itn = build_case("standard", "weibull")[[1]]
    irs = suppressMessages(build_intervention(
        c(Deterrency = 0, PrePrandial = 0.3, PostPrandial = 0.8), decay_attrition = "none",
        duration_years = days / 365, model_p = model_p))[[1]]
    comb = function(a, b, c1, c2, overlap) suppressMessages(calculate_combined_impact(
        intervention1 = a, intervention2 = b, cov_intervention1 = c1,
        cov_intervention2 = c2, cov_combination = overlap, model_p = model_p))
    # Coverages exact in binary, so that the bounds are exact
    for (cv in list(c(0.75, 0.5), c(0.25, 0.5))) {
        c1 = cv[1]; c2 = cv[2]
        lo = max(0, c1 + c2 - 1)
        hi = min(c1, c2)
        for (overlap in c(lo, hi)) {
            info = paste(c1, c2, overlap)
            res = comb(itn, irs, c1, c2, overlap)
            expect_equal(res, reference_combined_impact(itn, irs, c1, c2, overlap,
                                                        model_p = model_p),
                         tolerance = 1e-10, info = info)
            expect_equal(comb(irs, itn, c2, c1, overlap), res, tolerance = 1e-10, info = info)
        }
        # Just outside: the overlap rule (below only where the lower bound is
        # positive; below 0 is a negative coverage, a separate check)
        if (lo > 0)
            expect_error(comb(itn, irs, c1, c2, lo - 0.001),
                         "Coverage of combination must be between")
        else
            expect_error(comb(itn, irs, c1, c2, lo - 0.001), "cov_combination must be")
        expect_error(comb(itn, irs, c1, c2, hi + 0.001),
                     "Coverage of combination must be between")
    }
    # Bounds that are not exact in floating point: 0.8 + 0.8 - 1 = 0.6 gives a
    # group coverage of about -1e-16, which must be accepted
    at_bound = comb(itn, irs, 0.8, 0.8, 0.6)
    expect_true(is.finite(at_bound) && at_bound >= 0 && at_bound <= 1)
    expect_equal(at_bound, comb(itn, irs, 0.8, 0.8, 0.6 + 1e-9), tolerance = 1e-6)
    expect_equal(comb(irs, itn, 0.8, 0.8, 0.6), at_bound, tolerance = 1e-10)
    expect_error(comb(itn, irs, 0.8, 0.8, 0.6 - 1e-6), "Coverage of combination must be between")
})

test_that("an exported step decay ends on the same day as in the package", {
    # 45 days = 0.12328767... years: rounding L up (0.123288 = 45.0001 days)
    # would keep the protection at day 45 in OpenMalaria
    om = suppressMessages(get_OM_ento_snippet(gss))
    t  = 0:89
    for (L in c(45, 30, 200 / 3)) {
        iv  = suppressMessages(build_intervention(
            c(Deterrency = 0.3, PrePrandial = 0.4, PostPrandial = 0.2),
            decay_attrition = "step", halflife_attrition = L,
            duration_years = length(t) / 365, model_p = model_p, insecticide_decay = FALSE))
        d   = suppressMessages(get_OM_GVI_snippet(iv, om, "ITN"))[[1]]$deterrency$decay
        expect_lte(as.numeric(d$L), L / 365)                    # never beyond the end
        curve = decay_function(L = 365 * as.numeric(d$L), datej = t, decay = "step")
        expect_equal(curve, iv[[1]]$effects$survival, info = paste("L =", L))
    }
})

# ---------------------------------------------------------------------------
# 9. Simultaneous uncertainty inputs
# ---------------------------------------------------------------------------
test_that("intervention, vector and exposure draws together equal separate runs", {
    base = vec_ent_param[vec_ent_param$species_name == gss & vec_ent_param$level == "species", ]
    vec_draws = rbind(base, base)
    vec_draws$M[2]   = base$M * 0.85                  # deliberately different draws
    vec_draws$Chi[2] = base$Chi * 0.8
    vec_draws$A0[2]  = min(base$A0 * 1.1, 0.95)
    rownames(vec_draws) = NULL
    exposures = c(1, 0.5)

    for (mode in c("standard", "dummy_host")) {
        interv_draws = as.data.frame(data.table::rbindlist(list(
            build_case(mode, "weibull")[[1]]$param_set,
            build_case(mode, "weibull", scale = 0.6)[[1]]$param_set)))
        interv_draws$halflife_attrition[2] = 30

        drawn = suppressMessages(run_interventions(
            interv_draws[1, ], model_p, cov, vec_param_sets = vec_draws,
            interv_param_sets = interv_draws, exposure_sets = exposures,
            duration_years = days / 365))
        for (d in 1:2) {
            model_d = build_model_obj(def_vector_params(vector_table = vec_draws[d, ],
                                                        verbose = FALSE),
                                      def_host_params(verbose = FALSE))
            single = suppressMessages(run_interventions(
                interv_draws[d, ], model_d, cov, exposure = exposures[d],
                duration_years = days / 365))
            expect_equal(drawn$avg_impact[drawn$draw == d], single$avg_impact,
                         tolerance = 1e-12, info = paste(mode, "draw", d))
        }
        # The draws do differ
        expect_false(isTRUE(all.equal(drawn$avg_impact[drawn$draw == 1],
                                      drawn$avg_impact[drawn$draw == 2])), info = mode)
    }
})
