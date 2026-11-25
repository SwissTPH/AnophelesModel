library(AnophelesModel)
library(testthat)

# calculate_impact() and calculate_combined_impact() run on the shared
# simulate_vc() engine; they are checked against the pre-refactoring loops
# kept in helper-reference.R.

model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"),
                          def_host_params())

bi = function(efficacy, days = 60, ...) {
    suppressMessages(build_intervention(
        efficacy = efficacy,
        halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
        duration_years = days / 365, model_p = model_p, ...))
}
avg_impact = function(res, i = 1) res$avg_impact[res$intervention == i]
combined   = function(...) suppressMessages(
    calculate_combined_impact(..., model_p = model_p))

std   = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3)
dummy = c(Deterrency = 0.4, KillingDuringHostSeekingAbsolute = 2, PostPrandial = 0.3)
null  = c(Deterrency = 0, PrePrandial = 0, PostPrandial = 0)
cov   = c(0, 0.25, 0.5, 0.75, 1)

# Per-day values of intervention i as a [day x coverage] matrix
per_day = function(time, i, col)
    matrix(time[[col]][time$intervention == i], ncol = length(unique(time$coverage)))

expect_same_impact = function(interv_list, coverage_vec) {
    new = calculate_impact(interv_list, coverage_vec, model_p, keep_time = TRUE)
    ref = reference_calculate_impact(interv_list, coverage_vec, model_p)
    for (i in seq_along(interv_list)) {
        n = new$impact[new$impact$intervention == i, ]
        r = ref$interventions_vec[[i]]$effects
        expect_equal(n$coverage, coverage_vec)
        expect_equal(n$avg_vc,     unname(r$avg_vc),     tolerance = 1e-10)
        expect_equal(n$avg_impact, unname(r$avg_impact), tolerance = 1e-10)
        for (nm in c("vc", "impact"))
            expect_equal(per_day(new$time, i, nm), unname(r[[nm]]),
                         tolerance = 1e-10, info = nm)
    }
}

# ---------------------------------------------------------------------------
# 1. calculate_impact matches the reference loop
# ---------------------------------------------------------------------------
test_that("calculate_impact matches the reference: standard model", {
    expect_same_impact(bi(std), cov)
})

test_that("calculate_impact matches the reference: dummy-host model", {
    expect_same_impact(bi(dummy), cov)
})

test_that("calculate_impact matches the reference: several interventions, different durations", {
    expect_same_impact(list(bi(std, days = 30)[[1]], bi(dummy, days = 50)[[1]]), cov)
})

test_that("calculate_impact matches the reference with and without insecticide decay", {
    for (ins in c(TRUE, FALSE))
        expect_same_impact(bi(std, days = 400,
                              halflife_insecticide = c(Deterrency = 2, PostPrandial = 3),
                              insecticide_decay = ins), cov)
})

test_that("calculate_impact matches the reference with component decay and a dummy host", {
    expect_same_impact(c(
        bi(std, days = 200, name = "comp", component_decay = list(
            Deterrency = list(decay = "hill", L = 90, kappa = 3),
            PrePrandial = list(decay = "linear", L = 150))),
        bi(dummy, days = 100, name = "dh", halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 0.5))),
        cov)
})

test_that("calculate_impact output has the documented structure", {
    res = calculate_impact(c(bi(std, name = "A"), bi(dummy, name = "B")), cov, model_p)
    expect_s3_class(res, "data.table")
    expect_equal(names(res)[1:2], c("intervention", "name"))
    expect_true(all(c("coverage", "exposure", "avg_vc", "avg_impact") %in% names(res)))
    expect_equal(res$intervention, rep(1:2, each = length(cov)))
    expect_equal(res$name, rep(c("A", "B"), each = length(cov)))
    expect_equal(res$coverage, rep(cov, 2))
    expect_equal(res$killing_during_host_seeking_absolute, c(rep(NA, length(cov)), rep(2, length(cov))))

    timed = calculate_impact(bi(std), cov, model_p, keep_time = TRUE)
    expect_named(timed, c("impact", "time"))
    expect_equal(nrow(timed$time), 60 * length(cov))
    expect_equal(range(timed$time$day), c(0, 59))
    expect_named(timed$time, c("intervention", "name", "coverage", "day", "vc", "impact"))
})

test_that("calculate_impact rejects nested lists and hand-made objects", {
    expect_error(calculate_impact(list(bi(std), bi(std)), cov, model_p), "c\\(\\), not list")
    hand = bi(std)
    hand[[1]]$param_set = NULL
    expect_error(calculate_impact(hand, cov, model_p), "intervention 1.*build_intervention")
    expect_error(calculate_impact(list(), cov, model_p), "non-empty")
})

# ---------------------------------------------------------------------------
# 2. Impact is relative to coverage 0 even when it is not requested
# ---------------------------------------------------------------------------
test_that("calculate_impact without coverage 0 is relative to no intervention", {
    interv = bi(std)
    cov_no0 = c(0.3, 0.6, 1)
    new = calculate_impact(interv, cov_no0, model_p, keep_time = TRUE)
    ref = reference_calculate_impact(interv, c(0, cov_no0), model_p)$interventions_vec[[1]]
    expect_equal(new$impact$coverage, cov_no0)
    expect_equal(new$impact$avg_impact, ref$effects$avg_impact[-1], tolerance = 1e-10)
    expect_equal(per_day(new$time, 1, "vc"), unname(ref$effects$vc[, -1]), tolerance = 1e-10)
    expect_true(all(new$impact$avg_impact > 0))
})

# ---------------------------------------------------------------------------
# 3. A hand-written run_interventions table gives the reference loop's impact
#    of the same intervention (calculate_impact() is run_interventions() on
#    the table built by build_intervention(), so comparing the two only
#    checks that table; the reference loop is independent)
# ---------------------------------------------------------------------------
test_that("a hand-written run_interventions table matches the reference loop", {
    ps = data.frame(name = "x", deterrency = 0.5, preprandial = 0.4,
                    postprandial = 0.3, halflife_attrition = 180,
                    shape_attrition = 2, decay_attrition = "weibull",
                    stringsAsFactors = FALSE)
    res = suppressMessages(run_interventions(ps, model_p, cov,
                                             duration_years = 60 / 365))
    ref = reference_calculate_impact(bi(std), cov, model_p)$interventions_vec[[1]]$effects
    expect_equal(res$avg_impact, unname(ref$avg_impact), tolerance = 1e-10)
    expect_equal(res$avg_vc, unname(ref$avg_vc), tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# 3b. Exposure acts on coverage (effective coverage = coverage x exposure)
# ---------------------------------------------------------------------------
test_that("calculate_impact exposure is equivalent to scaling the coverage", {
    interv = bi(std)
    with_e = calculate_impact(interv, cov, model_p, exposure = 0.6)
    scaled = calculate_impact(interv, cov * 0.6, model_p)
    expect_equal(with_e$avg_impact, scaled$avg_impact, tolerance = 1e-12)
    expect_equal(with_e$coverage, cov)   # reported coverage stays nominal
    expect_equal(unique(with_e$exposure), 0.6)
    expect_error(calculate_impact(interv, cov, model_p, exposure = 1.2), "exposure")
    expect_error(calculate_impact(interv, cov, model_p, exposure = c(0.5, 0.6)), "exposure")
})

test_that("run_interventions applies exposure and exposure_sets", {
    ps = data.frame(name = "x", deterrency = 0.5, preprandial = 0.4,
                    postprandial = 0.3, halflife_attrition = 180,
                    shape_attrition = 2, decay_attrition = "weibull",
                    stringsAsFactors = FALSE)
    run = function(...) suppressMessages(
        run_interventions(ps, model_p, c(0.5, 1), duration_years = 60 / 365, ...))

    e   = run(exposure = 0.6)
    ref = run(coverage_sets = list(c(0.5, 1) * 0.6))
    expect_equal(e$avg_impact, ref$avg_impact, tolerance = 1e-12)
    expect_equal(e$coverage, c(0.5, 1))
    expect_equal(unique(e$exposure), 0.6)

    sets = run(exposure_sets = c(1, 0.6))
    expect_equal(sort(unique(sets$draw)), 1:2)
    expect_equal(sets$avg_impact[sets$draw == 2], e$avg_impact, tolerance = 1e-12)
    expect_equal(sets$avg_impact[sets$draw == 1], run()$avg_impact, tolerance = 1e-12)

    expect_error(run(exposure_sets = c(1, 0.6), coverage_sets = list(1, 1, 1)),
                 "same number of draws")
    expect_error(run(exposure = -0.1), "exposure")
})

# ---------------------------------------------------------------------------
# 4. calculate_combined_impact, standard model
# ---------------------------------------------------------------------------
test_that("calculate_combined_impact matches the reference for standard interventions", {
    itn = bi(std, days = 120)[[1]]
    irs = bi(c(Deterrency = 0, PrePrandial = 0.3, PostPrandial = 0.8), days = 120)[[1]]
    expect_equal(combined(intervention1 = itn, intervention2 = irs,
                          cov_intervention1 = 0.8, cov_intervention2 = 0.6),
                 reference_combined_impact(itn, irs, 0.8, 0.6, model_p = model_p),
                 tolerance = 1e-10)
    expect_equal(combined(intervention1 = itn, intervention2 = irs,
                          cov_intervention1 = 0.8, cov_intervention2 = 0.6,
                          cov_combination = 0.5),
                 reference_combined_impact(itn, irs, 0.8, 0.6, 0.5, model_p = model_p),
                 tolerance = 1e-10)
})

test_that("calculate_combined_impact reduces to the single intervention", {
    itn  = bi(std)[[1]]
    none = bi(null)[[1]]
    single = avg_impact(calculate_impact(list(itn), 0.7, model_p))
    expect_equal(combined(intervention1 = itn, intervention2 = none,
                          cov_intervention1 = 0.7, cov_intervention2 = 0.5),
                 single, tolerance = 1e-10)
    irs = bi(c(Deterrency = 0, PrePrandial = 0.3, PostPrandial = 0.8))[[1]]
    expect_equal(combined(intervention1 = itn, intervention2 = irs,
                          cov_intervention1 = 0.7, cov_intervention2 = 0),
                 single, tolerance = 1e-10)
})

test_that("calculate_combined_impact is symmetric", {
    itn = bi(std)[[1]]
    irs = bi(c(Deterrency = 0, PrePrandial = 0.3, PostPrandial = 0.8))[[1]]
    expect_equal(combined(intervention1 = itn, intervention2 = irs,
                          cov_intervention1 = 0.7, cov_intervention2 = 0.5),
                 combined(intervention1 = irs, intervention2 = itn,
                          cov_intervention1 = 0.5, cov_intervention2 = 0.7),
                 tolerance = 1e-10)
})

test_that("calculate_combined_impact still validates the coverages", {
    itn = bi(std)[[1]]
    expect_error(combined(intervention1 = itn, intervention2 = itn,
                          cov_intervention1 = 0.8, cov_intervention2 = 0.8,
                          cov_combination = 0.9),
                 "Coverage of combination must be between")
})

# ---------------------------------------------------------------------------
# 5. calculate_combined_impact, dummy-host model (additive host-seeking killing)
# ---------------------------------------------------------------------------
test_that("dummy-host combined with a null intervention equals the single dummy-host impact", {
    dh   = bi(dummy)[[1]]
    none = bi(null)[[1]]
    single = avg_impact(calculate_impact(list(dh), 0.7, model_p))
    expect_equal(combined(intervention1 = dh, intervention2 = none,
                          cov_intervention1 = 0.7, cov_intervention2 = 0.5),
                 single, tolerance = 1e-10)
    expect_equal(combined(intervention1 = none, intervention2 = dh,
                          cov_intervention1 = 0.5, cov_intervention2 = 0.7),
                 single, tolerance = 1e-10)
})

test_that("dummy-host combinations run and add host-seeking killing", {
    dh  = bi(dummy)[[1]]
    dh2 = bi(c(Deterrency = 0.2, KillingDuringHostSeekingAbsolute = 1.5, PostPrandial = 0.4))[[1]]
    itn = bi(std)[[1]]

    both = combined(intervention1 = dh, intervention2 = dh2,
                    cov_intervention1 = 0.7, cov_intervention2 = 0.7)
    expect_true(both >= 0 && both <= 1)
    expect_gt(both, avg_impact(calculate_impact(list(dh),  0.7, model_p)))
    expect_gt(both, avg_impact(calculate_impact(list(dh2), 0.7, model_p)))

    mixed = combined(intervention1 = dh, intervention2 = itn,
                     cov_intervention1 = 0.6, cov_intervention2 = 0.6)
    expect_true(mixed >= 0 && mixed <= 1)
    expect_equal(mixed,
                 combined(intervention1 = itn, intervention2 = dh,
                          cov_intervention1 = 0.6, cov_intervention2 = 0.6),
                 tolerance = 1e-10)
})

test_that("host-seeking killing of two dummy-host interventions adds up exactly", {
    # Constant effects, everyone covered by both: the combination is one
    # dummy-host intervention with the summed host-seeking killing (and the
    # multiplicative deterrency and post-prandial killing)
    constant = function(efficacy, days = 30) suppressMessages(build_intervention(
        efficacy, decay_attrition = "none", duration_years = days / 365,
        model_p = model_p, insecticide_decay = FALSE))[[1]]
    a = c(Deterrency = 0,   KillingDuringHostSeekingAbsolute = 0.6, PostPrandial = 0)
    b = c(Deterrency = 0,   KillingDuringHostSeekingAbsolute = 1.5, PostPrandial = 0)
    expect_equal(combined(intervention1 = constant(a), intervention2 = constant(b),
                          cov_intervention1 = 1, cov_intervention2 = 1),
                 avg_impact(calculate_impact(list(constant(a + b)), 1, model_p)),
                 tolerance = 1e-10)
    a = c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.6, PostPrandial = 0.2)
    b = c(Deterrency = 0.5, KillingDuringHostSeekingAbsolute = 1.5, PostPrandial = 0.4)
    ab = c(Deterrency = 1 - 0.7 * 0.5, KillingDuringHostSeekingAbsolute = 2.1,
           PostPrandial = 1 - 0.8 * 0.6)
    expect_equal(combined(intervention1 = constant(a), intervention2 = constant(b),
                          cov_intervention1 = 1, cov_intervention2 = 1),
                 avg_impact(calculate_impact(list(constant(ab)), 1, model_p)),
                 tolerance = 1e-10)
})

test_that("calculate_combined_impact evaluates both interventions over the shorter duration", {
    itn = bi(std, days = 90, halflife_insecticide = c(Deterrency = 1))[[1]]
    irs = bi(c(Deterrency = 0, PrePrandial = 0.3, PostPrandial = 0.8), days = 40)[[1]]
    itn_40 = bi(std, days = 40, halflife_insecticide = c(Deterrency = 1))[[1]]
    for (order in 1:2) {
        long  = list(itn, irs)[c(order, 3 - order)]
        short = list(itn_40, irs)[c(order, 3 - order)]
        expect_equal(combined(intervention1 = long[[1]], intervention2 = long[[2]],
                              cov_intervention1 = 0.7, cov_intervention2 = 0.5),
                     combined(intervention1 = short[[1]], intervention2 = short[[2]],
                              cov_intervention1 = 0.7, cov_intervention2 = 0.5),
                     tolerance = 1e-12, info = order)
    }
    # The truncation matters: the 90-day net alone has a different impact
    expect_false(isTRUE(all.equal(avg_impact(calculate_impact(list(itn), 0.7, model_p)),
                                  avg_impact(calculate_impact(list(itn_40), 0.7, model_p)))))
})

test_that("calculate_combined_impact uses the supplied model, not the one that built the interventions", {
    other_p = build_model_obj(def_vector_params("Anopheles funestus", verbose = FALSE),
                              def_host_params(verbose = FALSE))
    build_with = function(mp, efficacy, days = 60) suppressMessages(build_intervention(
        efficacy, halflife_attrition = 180, shape_attrition = 2, duration_years = days / 365,
        halflife_insecticide = c(PostPrandial = 2), model_p = mp))[[1]]
    irs_eff = c(Deterrency = 0, PrePrandial = 0.3, PostPrandial = 0.8)
    for (eff2 in list(irs_eff, dummy)) {
        same  = combined(intervention1 = build_with(model_p, std),
                         intervention2 = build_with(model_p, eff2, days = 40),
                         cov_intervention1 = 0.7, cov_intervention2 = 0.5)
        other = combined(intervention1 = build_with(other_p, std),
                         intervention2 = build_with(other_p, eff2, days = 40),
                         cov_intervention1 = 0.7, cov_intervention2 = 0.5)
        mixed = combined(intervention1 = build_with(model_p, std),
                         intervention2 = build_with(other_p, eff2, days = 40),
                         cov_intervention1 = 0.7, cov_intervention2 = 0.5)
        expect_equal(other, same, tolerance = 1e-12)
        expect_equal(mixed, same, tolerance = 1e-12)
        # The supplied model does matter
        with_other = suppressMessages(calculate_combined_impact(
            intervention1 = build_with(model_p, std),
            intervention2 = build_with(model_p, eff2, days = 40),
            cov_intervention1 = 0.7, cov_intervention2 = 0.5, model_p = other_p))
        expect_false(isTRUE(all.equal(with_other, same)))
    }
    # A second intervention at coverage 0 gives calculate_impact() of the first,
    # whichever model built them (insecticide decay included)
    itn = build_with(other_p, std)
    single = avg_impact(calculate_impact(list(itn), 0.7, model_p))
    expect_equal(combined(intervention1 = itn, intervention2 = build_with(other_p, irs_eff),
                          cov_intervention1 = 0.7, cov_intervention2 = 0),
                 single, tolerance = 1e-10)
    # Not an intervention element
    expect_error(combined(intervention1 = list(itn), intervention2 = itn,
                          cov_intervention1 = 0.5, cov_intervention2 = 0.5),
                 "intervention1 must be an intervention")
})

test_that("complete protection survives combination with a null intervention", {
    constant = function(efficacy) suppressMessages(build_intervention(
        efficacy, decay_attrition = "none", duration_years = 2 / 365,
        model_p = model_p, insecticide_decay = FALSE))[[1]]
    none = constant(null)
    for (component in names(null)) {
        efficacy = null
        efficacy[component] = 1
        protected = constant(efficacy)
        for (coverage in c(0.7, 1)) {
            single = avg_impact(calculate_impact(list(protected), coverage, model_p))
            if (coverage == 1) expect_equal(single, 1)
            for (null_coverage in c(0, 0.5, 1)) {
                result = combined(intervention1 = protected, intervention2 = none,
                                  cov_intervention1 = coverage,
                                  cov_intervention2 = null_coverage)
                expect_equal(result, single, tolerance = 1e-12, info = component)
                expect_equal(result,
                    combined(intervention1 = none, intervention2 = protected,
                             cov_intervention1 = null_coverage,
                             cov_intervention2 = coverage),
                    tolerance = 1e-12, info = component)
            }
        }
    }
})

test_that("combined impact approaches complete protection continuously", {
    constant = function(efficacy) suppressMessages(build_intervention(
        efficacy, decay_attrition = "none", duration_years = 2 / 365,
        model_p = model_p, insecticide_decay = FALSE))[[1]]
    none = constant(null)
    for (component in names(null)) {
        impacts = vapply(c(0.99, 0.9999, 1), function(value) {
            efficacy = null
            efficacy[component] = value
            combined(intervention1 = constant(efficacy), intervention2 = none,
                     cov_intervention1 = 1, cov_intervention2 = 0)
        }, numeric(1))
        expect_true(all(is.finite(impacts)), info = component)
        expect_true(all(diff(impacts) >= 0), info = component)
        expect_equal(impacts[3], 1, info = component)
        expect_equal(impacts[2], impacts[3], tolerance = 1e-3, info = component)
    }
})

test_that("complete dummy-host protection can be combined with an active intervention", {
    constant = function(efficacy) suppressMessages(build_intervention(
        efficacy, decay_attrition = "none", duration_years = 2 / 365,
        model_p = model_p, insecticide_decay = FALSE))[[1]]
    other = constant(std)
    for (component in c("Deterrency", "PostPrandial")) {
        efficacy = c(Deterrency = 0, KillingDuringHostSeekingAbsolute = 2, PostPrandial = 0)
        efficacy[component] = 1
        protected = constant(efficacy)
        expect_equal(avg_impact(calculate_impact(list(protected), 1, model_p)), 1)
        expect_equal(combined(intervention1 = protected, intervention2 = other,
                              cov_intervention1 = 1, cov_intervention2 = 0.6), 1)
        expect_equal(combined(intervention1 = other, intervention2 = protected,
                              cov_intervention1 = 0.6, cov_intervention2 = 1), 1)
    }
})

test_that("zero baseline VC gives an explicit undefined-relative-impact error", {
    vec_p = def_vector_params()
    vec_p$Chi = 0
    zero_model = build_model_obj(vec_p, def_host_params())
    interv = suppressMessages(build_intervention(
        std, decay_attrition = "none", duration_years = 2 / 365,
        model_p = zero_model, insecticide_decay = FALSE))
    expect_error(calculate_impact(interv, c(0, 1), zero_model),
                 "baseline vectorial capacity.*zero.*undefined")
    ps = data.frame(deterrency = 0.5, preprandial = 0.4, postprandial = 0.3,
                    halflife_attrition = 180, shape_attrition = 2,
                    decay_attrition = "none")
    expect_error(suppressMessages(run_interventions(
        ps, zero_model, 1, duration_years = 2 / 365)),
        "baseline vectorial capacity.*zero.*undefined")
})

test_that("non-finite VC is reported instead of becoming zero impact", {
    # Checked on the time loop itself: validated inputs cannot produce it
    interv = bi(std, days = 2)[[1]]
    h   = model_p$host_params
    eff = AnophelesModel:::apply_eff_matrices(
        AnophelesModel:::make_mult_matrices(interv$param_set, 0:1), h)
    eff$PBi_1[1, 2] = NA_real_
    expect_error(AnophelesModel:::simulate_vc(eff, AnophelesModel:::host_base(h),
                                               model_p$vec_params, h, 1),
                 "non-finite.*vectorial capacity.*time point 2")
})

test_that("run_interventions keeps the daily values of every parameter set", {
    ps = data.frame(name = c("a", "b"), deterrency = c(0.5, 0.2), preprandial = 0.4,
                    postprandial = 0.3, halflife_attrition = 180,
                    shape_attrition = 2, decay_attrition = "weibull")
    res = suppressMessages(run_interventions(ps, model_p, c(0.5, 1),
                                             duration_years = 30 / 365, keep_time = TRUE))
    expect_equal(nrow(res$time), 2 * 2 * 30)
    # Daily means are the averages, for each parameter set and coverage
    means = aggregate(impact ~ param_set + coverage, res$time, mean)
    means = means[order(means$param_set, means$coverage), ]
    expect_equal(means$impact, res$impact$avg_impact, tolerance = 1e-12)
    expect_equal(unique(res$time$name[res$time$param_set == 2]), "b")
    expect_error(suppressMessages(run_interventions(ps, model_p, 1, keep_time = NA)),
                 "keep_time")
})

test_that("plot_impact draws calculate_impact and run_interventions outputs", {
    impacts = calculate_impact(c(bi(std, name = "A"), bi(dummy, name = "B")), cov, model_p)
    p = plot_impact(impacts, title = "An. gambiae")
    expect_s3_class(p, "ggplot")
    expect_equal(sort(unique(p$data$name)), c("A", "B"))
    expect_s3_class(plot_impact(calculate_impact(bi(std), cov, model_p, keep_time = TRUE)),
                    "ggplot")
    # Draws: median and 95% band
    ps = data.frame(name = "x", deterrency = 0.5, preprandial = 0.4, postprandial = 0.3,
                    halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull")
    draws = suppressMessages(run_interventions(ps, model_p, c(0.5, 1), duration_years = 10 / 365,
                                               exposure_sets = c(1, 0.8, 0.6)))
    p = plot_impact(draws)
    at1 = 100 * draws$avg_impact[draws$coverage == 1]
    expect_equal(p$data$impact[p$data$coverage == 1], median(at1))
    expect_equal(p$data$lo[p$data$coverage == 1], unname(quantile(at1, 0.025)))
    expect_equal(p$data$hi[p$data$coverage == 1], unname(quantile(at1, 0.975)))
    expect_true(all(p$data$lo < p$data$hi))   # a real band, not the median
    expect_error(plot_impact(list(a = 1)), "output of calculate_impact")
})
