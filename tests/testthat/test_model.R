library(AnophelesModel)
library(testthat)
########################################
# Tests for build_model_obj()
########################################
test_that("test that a model object is correctly built", {
    # Create the necessary objects
    # Species name must match the default def_host_params() species ("Anopheles gambiae")
    species_name = "Anopheles gambiae"
    M = 0.623
    M.sd = 0.01
    Chi = 0.939
    A0 = 0.313
    A0.sd = 0.01
    zeta.3 = 1
    td = 0.33
    tau = 3
    ts = 10
    to = 5
    endophily = 1
    endophily.sd = 0
    endophagy = 1
    endophagy.sd = 0

    # Define data frame with custom values
    custom_params = as.data.frame(cbind.data.frame(species_name, M, M.sd, Chi,
                                                   A0, A0.sd, zeta.3, td,
                                                   tau, ts, to, endophily, endophily.sd,
                                                   endophagy, endophagy.sd))

    vec_params = def_vector_params(mosquito_species = "Anopheles gambiae custom",
                                vector_table = custom_params)
    # Define other objects with default values
    hosts_params = def_host_params()
    total_pop = 2000

    # Build the model object
    model_params = build_model_obj(vec_p = vec_params, hosts_p = hosts_params)

    sol_obj = NULL
    sol_obj$vec_params = vec_params
    sol_obj$host_params = hosts_params
    sol_obj$host_params$muvA = 0.2362186
    # alphai is now per-unit-host (Ni_baseline = 1), so 1000x larger than the
    # old per-1000-host values (0.0008464341, 0.00005498667)
    sol_obj$host_params$alphai = c(0.8464341, 0.05498667)

    expect_equal(model_params, sol_obj, tolerance = 0.0001)
})


test_that("Albimanus: calculate_impact produces valid outputs for custom species parameters", {
    # Checks that the package runs end-to-end with custom An. albimanus parameters
    # and produces numerically sensible impact values (LLINs04-like efficacy).
    custom_params = as.data.frame(cbind.data.frame(
        species_name = "Anopheles albimanus",
        M = 0.4914393, M.sd = 0.01,
        Chi = 0.224185,
        A0 = 0.405, A0.sd = 0.01,
        zeta.3 = 1, td = 0.33, tau = 2.044137, ts = 10, to = 5,
        endophily = 0.3406517, endophily.sd = 0.01,
        endophagy = 0.2727916, endophagy.sd = 0.01
    ))
    vec_obj  = def_vector_params(mosquito_species = "Anopheles albimanus",
                                 vector_table = custom_params)
    host_obj = def_host_params()
    model_obj = build_model_obj(vec_obj, host_obj)

    # LLINs04-like efficacy: deterrency + pre/post-prandial killing
    interv = build_intervention(
        efficacy = c(Deterrency = 0.45, PrePrandial = 0.53, PostPrandial = 0.34),
        halflife_attrition = 2.64 * 365, shape_attrition = 1,
        decay_attrition = "weibull", duration_years = 3 / 365,
        model_p = model_obj, name = "LLINs04"
    )

    # Exact check at t=0: ratio protected/unprotected = 1 - efficacy
    eff0 = interv[[1]]$effects
    expect_equal(eff0$alphai[1, 1] / eff0$alphai[1, 2], 1 - 0.45, tolerance = 1e-10)
    expect_equal(eff0$PBi  [1, 1] / eff0$PBi  [1, 2], 1 - 0.53, tolerance = 1e-10)
    expect_equal(eff0$PCi  [1, 1] / eff0$PCi  [1, 2], 1 - 0.34, tolerance = 1e-10)

    coverages = c(seq(0, 0.9, by = 0.1), 0.95, 0.98, 0.99)
    eff       = calculate_impact(interv, coverages, model_obj)

    # avg_impact in [0, 1]
    expect_true(all(eff$avg_impact >= -1e-10))
    expect_true(all(eff$avg_impact <=  1 + 1e-10))
    # impact at coverage 0 is exactly 0
    expect_equal(eff$avg_impact[1], 0, tolerance = 1e-10)
    # impact non-decreasing with coverage
    expect_true(all(diff(eff$avg_impact) >= -1e-8))
    # avg_vc <= baseline (first coverage is 0 → vc at coverage 0 is baseline)
    expect_true(all(eff$avg_vc <= eff$avg_vc[1] + 1e-10))
})

test_that("build_intervention output has correct structure and exact initial efficacy values", {
    # Replaces the old test that manually constructed effects matrices via the
    # removed def_interventions_effects() function. Uses An. gambiae whose
    # baseline alphai[1] = 0.8464341 (verified by test_model block 1).
    # At t=0 (day index 1): survival=1, no insecticide decay, so ratios are exact.
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"), def_host_params())
    duration_years = 1   # 365 days — needed so survival[181] exists for the halflife check
    npoints = max(round(365 * duration_years), 1L)

    det  = 0.45; pre = 0.53; post = 0.34
    interv = build_intervention(
        efficacy = c(Deterrency = det, PrePrandial = pre, PostPrandial = post),
        halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
        duration_years = duration_years, model_p = model_p, name = "test"
    )[[1]]$effects

    expected_matrices = c("alphai", "PBi", "PCi", "PDi", "PEi", "Kvi", "survival")
    expect_true(all(expected_matrices %in% names(interv)))

    # Dimensions: npoints rows × 3 host columns (standard model)
    for (mat in c("alphai", "PBi", "PCi", "PDi", "PEi")) {
        expect_equal(nrow(interv[[mat]]), npoints, info = mat)
        expect_equal(ncol(interv[[mat]]), 3,       info = mat)
    }

    # At t=0: survival=1, no insecticide decay → exact ratios
    expect_equal(interv$alphai[1, 1] / interv$alphai[1, 2], 1 - det,  tolerance = 1e-10)
    expect_equal(interv$PBi  [1, 1] / interv$PBi  [1, 2], 1 - pre,  tolerance = 1e-10)
    expect_equal(interv$PCi  [1, 1] / interv$PCi  [1, 2], 1 - post, tolerance = 1e-10)

    # Unprotected humans and animals unchanged from baseline
    expect_equal(interv$alphai[, 2], interv$alphai[, 3] * model_p$host_params$alphai[1] /
                     model_p$host_params$alphai[2], tolerance = 1e-10)

    # Survival: exactly 1 at t=0; at t=halflife (day 180), Weibull → 0.5 by definition
    expect_equal(interv$survival[1],   1,   tolerance = 1e-12)
    expect_equal(interv$survival[181], 0.5, tolerance = 1e-6)

    # Probability matrices in [0, 1]
    for (mat in c("PBi", "PCi", "PDi", "PEi")) {
        expect_true(all(interv[[mat]] >= 0 & interv[[mat]] <= 1 + 1e-10), info = mat)
    }
})
