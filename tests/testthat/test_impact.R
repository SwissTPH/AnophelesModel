library(AnophelesModel2)
library(testthat)

# calculate_impact() and calculate_combined_impact() run on the shared
# simulate_vc() engine; they are checked against the pre-refactoring loops
# kept in helper-reference.R.

model_p = build_model_obj(def_vector_params("Anopheles gambiae"),
                          def_host_params())

bi = function(efficacy, days = 60, ...) {
    suppressMessages(build_intervention(
        efficacy = efficacy,
        halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
        duration_years = days / 365, model_p = model_p, ...))
}
avg_impact = function(res, i = 1) res$interventions_vec[[i]]$effects$avg_impact
combined   = function(...) suppressMessages(
    calculate_combined_impact(..., model_p = model_p))

std   = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3)
dummy = c(Deterrency = 0.4, HostSeekingKilling = 2, PostPrandial = 0.3)
null  = c(Deterrency = 0, PrePrandial = 0, PostPrandial = 0)
cov   = c(0, 0.25, 0.5, 0.75, 1)

expect_same_impact = function(interv_list, coverage_vec) {
    new = calculate_impact(interv_list, coverage_vec, model_p)
    ref = reference_calculate_impact(interv_list, coverage_vec, model_p)
    for (i in seq_along(interv_list)) {
        n = new$interventions_vec[[i]]
        r = ref$interventions_vec[[i]]
        expect_equal(n$coverages, r$coverages)
        for (nm in c("vc", "impact", "avg_vc", "avg_impact"))
            expect_equal(unname(n$effects[[nm]]), unname(r$effects[[nm]]),
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

test_that("calculate_impact output has the documented structure", {
    res = calculate_impact(bi(std), cov, model_p)
    e   = res$interventions_vec[[1]]$effects
    expect_equal(dim(e$vc),     c(60, length(cov)))
    expect_equal(dim(e$impact), c(60, length(cov)))
    expect_length(e$avg_vc,     length(cov))
    expect_length(e$avg_impact, length(cov))
    expect_equal(res$interventions_vec[[1]]$coverages, cov)
})

# ---------------------------------------------------------------------------
# 2. Impact is relative to coverage 0 even when it is not requested
# ---------------------------------------------------------------------------
test_that("calculate_impact without coverage 0 is relative to no intervention", {
    interv = bi(std)
    cov_no0 = c(0.3, 0.6, 1)
    new = calculate_impact(interv, cov_no0, model_p)$interventions_vec[[1]]
    ref = reference_calculate_impact(interv, c(0, cov_no0), model_p)$interventions_vec[[1]]
    expect_equal(new$coverages, cov_no0)
    expect_equal(new$effects$avg_impact, ref$effects$avg_impact[-1], tolerance = 1e-10)
    expect_equal(new$effects$vc, ref$effects$vc[, -1], tolerance = 1e-10)
    expect_true(all(new$effects$avg_impact > 0))
})

# ---------------------------------------------------------------------------
# 3. calculate_impact and run_interventions agree
# ---------------------------------------------------------------------------
test_that("calculate_impact and run_interventions give the same avg_impact", {
    ps = data.frame(name = "x", deterrency = 0.5, preprandial = 0.4,
                    postprandial = 0.3, halflife_attrition = 180,
                    shape_attrition = 2, decay_attrition = "weibull",
                    stringsAsFactors = FALSE)
    res = suppressMessages(run_interventions(ps, model_p, cov,
                                             duration_years = 60 / 365))
    expect_equal(res$avg_impact, avg_impact(calculate_impact(bi(std), cov, model_p)),
                 tolerance = 1e-12)
})

# ---------------------------------------------------------------------------
# 3b. Exposure acts on coverage (effective coverage = coverage x exposure)
# ---------------------------------------------------------------------------
test_that("calculate_impact exposure is equivalent to scaling the coverage", {
    interv = bi(std)
    with_e = calculate_impact(interv, cov, model_p, exposure = 0.6)$interventions_vec[[1]]
    scaled = calculate_impact(interv, cov * 0.6, model_p)$interventions_vec[[1]]
    expect_equal(with_e$effects$avg_impact, scaled$effects$avg_impact, tolerance = 1e-12)
    expect_equal(with_e$coverages, cov)   # reported coverage stays nominal
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
    dh2 = bi(c(Deterrency = 0.2, HostSeekingKilling = 1.5, PostPrandial = 0.4))[[1]]
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
