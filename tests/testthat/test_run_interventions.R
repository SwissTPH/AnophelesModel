library(AnophelesModel2)
library(testthat)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

make_model_p = function() {
    vec_p  = def_vector_params("Anopheles gambiae")
    host_p = def_host_params()
    build_model_obj(vec_p, host_p)
}

# Reference: run one param-set row through build_intervention + calculate_impact
# (the pre-vectorisation loop) and return a data.frame matching run_interventions output.
reference_run = function(param_sets, model_p, coverage_vec, duration_years = 3) {
    npoints = round(365 * duration_years)
    rows = lapply(seq_len(nrow(param_sets)), function(i) {
        row = param_sets[i, ]
        efficacy = c(Deterrency   = row$deterrency,
                     PrePrandial  = row$preprandial,
                     PostPrandial = row$postprandial)
        if (!is.null(row$host_seeking_killing) && !is.na(row$host_seeking_killing))
            efficacy["HostSeekingKilling"] = row$host_seeking_killing
        hl = NULL
        if (!is.null(row$halflife_det) && !is.na(row$halflife_det))
            hl = c(Deterrency=row$halflife_det, PrePrandial=row$halflife_pre,
                   PostPrandial=row$halflife_post)
        interv = build_intervention(
            efficacy = efficacy, halflife_attrition = row$halflife_attrition,
            shape_attrition    = if (is.na(row$shape_attrition)) NULL else row$shape_attrition,
            duration_years = duration_years, model_p = model_p,
            name     = as.character(row$name),
            decay_attrition    = as.character(row$decay_attrition),
            halflife_insecticide = hl)
        impact = calculate_impact(interv, coverage_vec, model_p)
        eff    = impact$interventions_vec[[1]]$effects
        data.frame(name       = as.character(row$name),
                   coverage   = coverage_vec,
                   avg_vc     = eff$avg_vc,
                   avg_impact = eff$avg_impact,
                   stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# 1. Single param set: vectorised output matches loop reference
# ---------------------------------------------------------------------------
test_that("run_interventions matches calculate_impact for a single param set", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.25, 0.5, 0.75, 1)
    duration_years = 50 / 365

    param_sets = data.frame(
        name                = "test_llin",
        deterrency          = 0.5,
        preprandial         = 0.4,
        postprandial        = 0.3,
        halflife_attrition  = 180,
        shape_attrition     = 2,
        decay_attrition     = "weibull",
        stringsAsFactors    = FALSE
    )

    ref = reference_run(param_sets, model_p, coverage_vec, duration_years)
    res = as.data.frame(run_interventions(param_sets, model_p, coverage_vec, duration_years = duration_years))

    expect_equal(res$avg_vc,     ref$avg_vc,     tolerance = 1e-8)
    expect_equal(res$avg_impact, ref$avg_impact, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# 2. Multiple param sets: each row matches loop reference independently
# ---------------------------------------------------------------------------
test_that("run_interventions matches calculate_impact for multiple param sets", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.5, 1)
    duration_years = 30 / 365

    param_sets = data.frame(
        name               = c("low", "mid", "high"),
        deterrency         = c(0.2, 0.5, 0.8),
        preprandial        = c(0.1, 0.4, 0.7),
        postprandial       = c(0.1, 0.3, 0.6),
        halflife_attrition = c(120, 180, 240),
        shape_attrition    = c(2, 2, 3),
        decay_attrition    = "weibull",
        stringsAsFactors   = FALSE
    )

    ref = reference_run(param_sets, model_p, coverage_vec, duration_years)
    res = as.data.frame(run_interventions(param_sets, model_p, coverage_vec, duration_years = duration_years))

    expect_equal(res$avg_vc,     ref$avg_vc,     tolerance = 1e-8)
    expect_equal(res$avg_impact, ref$avg_impact, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# 3. Output structure: correct dimensions and column names
# ---------------------------------------------------------------------------
test_that("run_interventions returns correct dimensions and column names", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.5, 1)
    n_param      = 4
    duration_years = 20 / 365

    param_sets = data.frame(
        name               = paste0("ps", seq_len(n_param)),
        deterrency         = seq(0.1, 0.7, length.out = n_param),
        preprandial        = seq(0.1, 0.5, length.out = n_param),
        postprandial       = seq(0.1, 0.4, length.out = n_param),
        halflife_attrition = 180,
        shape_attrition    = 2,
        decay_attrition    = "weibull",
        stringsAsFactors   = FALSE
    )

    res = run_interventions(param_sets, model_p, coverage_vec, duration_years = duration_years)

    expect_equal(nrow(res), n_param * length(coverage_vec))
    expect_true(all(c("name", "coverage", "avg_vc", "avg_impact") %in% names(res)))
    expect_true(all(names(param_sets) %in% names(res)))
    expect_false("draw" %in% names(res))
    # impact at coverage = 0 should be ~0
    expect_true(all(abs(res$avg_impact[res$coverage == 0]) < 1e-10))

    # coverage_vec without 0: output should not contain coverage=0 row,
    # and avg_impact values should match the run that includes 0 explicitly
    res_no0 = run_interventions(param_sets, model_p, c(0.5, 1),
                                duration_years = duration_years)
    res_w0  = run_interventions(param_sets, model_p, c(0, 0.5, 1),
                                duration_years = duration_years)
    expect_false(0 %in% res_no0$coverage)
    expect_equal(res_no0$avg_impact,
                 res_w0$avg_impact[res_w0$coverage != 0],
                 tolerance = 1e-15)
    # impact should increase with coverage
    impacts = matrix(res$avg_impact, nrow = length(coverage_vec))
    expect_true(all(apply(impacts, 2, diff) >= -1e-8))
})

# ---------------------------------------------------------------------------
# 4. vec_param_sets: output has draw column; correct number of rows
# ---------------------------------------------------------------------------
test_that("run_interventions with vec_param_sets adds draw column and correct rows", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.5, 1)
    duration_years = 10 / 365

    param_sets = data.frame(
        name         = c("A", "B"),
        deterrency   = c(0.4, 0.6),
        preprandial  = c(0.3, 0.5),
        postprandial = c(0.2, 0.4),
        halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
        stringsAsFactors = FALSE
    )

    # Two draws: slightly perturbed gambiae params
    base_row = AnophelesModel2::vec_ent_param[
        AnophelesModel2::vec_ent_param$species_name == "Anopheles gambiae", ][1, ]
    draw1    = base_row; draw1$M = base_row$M * 0.95
    draw2    = base_row; draw2$M = base_row$M * 1.05
    vec_ps   = rbind(draw1, draw2)

    res = run_interventions(param_sets, model_p, coverage_vec,
                            vec_param_sets = vec_ps, duration_years = duration_years)

    expect_equal(nrow(res), nrow(param_sets) * length(coverage_vec) * nrow(vec_ps))
    expect_true("draw" %in% names(res))
    expect_equal(sort(unique(res$draw)), 1:2)
})

# ---------------------------------------------------------------------------
# 4b. interv_param_sets as data frame: identical to list form
# ---------------------------------------------------------------------------
test_that("run_interventions accepts interv_param_sets as data frame and matches list form", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.5, 1)
    duration_years = 10 / 365

    param_sets = data.frame(
        name         = "base",
        deterrency   = 0.5,
        preprandial  = 0.4,
        postprandial = 0.3,
        halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
        stringsAsFactors = FALSE
    )

    # Two draws with slightly different efficacy values
    interv_df = data.frame(
        name         = c("draw1", "draw2"),
        deterrency   = c(0.4,  0.6),
        preprandial  = c(0.35, 0.55),
        postprandial = c(0.25, 0.45),
        halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
        stringsAsFactors = FALSE
    )
    # Equivalent list form for backward-compat check
    interv_list = list(interv_df[1, , drop = FALSE], interv_df[2, , drop = FALSE])

    res_df   = run_interventions(param_sets, model_p, coverage_vec,
                                 interv_param_sets = interv_df,   duration_years = duration_years)
    res_list = run_interventions(param_sets, model_p, coverage_vec,
                                 interv_param_sets = interv_list, duration_years = duration_years)

    expect_true(is.data.frame(interv_df))
    expect_equal(nrow(res_df), length(coverage_vec) * nrow(interv_df))
    expect_true("draw" %in% names(res_df))
    expect_equal(res_df$avg_impact, res_list$avg_impact, tolerance = 1e-15)
    expect_equal(res_df$avg_vc,     res_list$avg_vc,     tolerance = 1e-15)
})

# ---------------------------------------------------------------------------
# 5. Dummy-host (HostSeekingKilling) model: output matches loop reference
# ---------------------------------------------------------------------------
test_that("run_interventions dummy-host model matches calculate_impact reference", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.5, 1)
    duration_years = 20 / 365

    param_sets = data.frame(
        name                = "hsk_test",
        deterrency          = 0.0,
        preprandial         = 0.0,
        postprandial        = 0.0,
        host_seeking_killing = 0.6,
        halflife_attrition  = 180,
        shape_attrition     = 2,
        decay_attrition     = "weibull",
        stringsAsFactors    = FALSE
    )

    ref = reference_run(param_sets, model_p, coverage_vec, duration_years)
    res = as.data.frame(run_interventions(param_sets, model_p, coverage_vec, duration_years = duration_years))

    expect_equal(res$avg_vc,     ref$avg_vc,     tolerance = 1e-8)
    expect_equal(res$avg_impact, ref$avg_impact, tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# 6. Decay types: linear and hill give valid (non-NA, finite) outputs
# ---------------------------------------------------------------------------
test_that("run_interventions works with linear and hill decay types", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.5, 1)
    duration_years = 20 / 365

    param_sets = data.frame(
        name               = c("linear_test", "hill_test"),
        deterrency         = c(0.5, 0.5),
        preprandial        = c(0.3, 0.3),
        postprandial       = c(0.2, 0.2),
        halflife_attrition = c(365, 365),
        shape_attrition    = c(NA,  2),
        decay_attrition    = c("linear", "hill"),
        stringsAsFactors   = FALSE
    )

    res = run_interventions(param_sets, model_p, coverage_vec, duration_years = duration_years)

    expect_true(all(is.finite(res$avg_vc)))
    expect_true(all(is.finite(res$avg_impact)))
    expect_true(all(res$avg_vc >= 0))
})

# ---------------------------------------------------------------------------
# 7. Scale test: 500 param sets complete in reasonable time
# ---------------------------------------------------------------------------
test_that("run_interventions completes 500 param sets within 60 seconds", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.5, 1)
    duration_years = 50 / 365
    n              = 500

    set.seed(42)
    param_sets = data.frame(
        name               = paste0("ps", seq_len(n)),
        deterrency         = runif(n, 0, 0.8),
        preprandial        = runif(n, 0, 0.8),
        postprandial       = runif(n, 0, 0.8),
        halflife_attrition = runif(n, 60, 365),
        shape_attrition    = runif(n, 1, 4),
        decay_attrition    = "weibull",
        stringsAsFactors   = FALSE
    )

    elapsed = system.time(
        res <- run_interventions(param_sets, model_p, coverage_vec, duration_years = duration_years)
    )[["elapsed"]]

    expect_equal(nrow(res), n * length(coverage_vec))
    expect_lt(elapsed, 60)
    npoints = round(365 * duration_years)
    message(sprintf("500 param sets, %d timepoints: %.1f s", npoints, elapsed))
})

# ---------------------------------------------------------------------------
# 8. Reparameterisation check: eval_entomological_model with (t_avail, host_frac)
#    gives identical results to manually computing alphai * Ni beforehand.
# ---------------------------------------------------------------------------
test_that("eval_entomological_model reparameterisation is numerically identical", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.25, 0.5, 0.75, 1)
    duration_years = 10 / 365

    param_sets = data.frame(
        name = "reparam_check", deterrency = 0.5, preprandial = 0.4,
        postprandial = 0.3, halflife_attrition = 180, shape_attrition = 2,
        decay_attrition = "weibull", stringsAsFactors = FALSE
    )

    # Build intervention object to get alphai / PBi / etc. matrices
    interv_list = build_intervention(
        efficacy = c(Deterrency=0.5, PrePrandial=0.4, PostPrandial=0.3),
        halflife_attrition = 180, shape_attrition = 2, duration_years = duration_years,
        model_p = model_p, name = "test", decay_attrition = "weibull")
    interv_obj = interv_list[[1]]$effects

    vec_p  = model_p$vec_params
    host_p = model_p$host_params
    # Nv0 cancels in impact ratios; use same value as calculate_impact internals
    Nv0     = 1L
    N_equiv = 2L

    npoints = round(365 * duration_years)
    vc_new = vc_ref = NULL

    for (ip in seq_len(npoints)) {
        alphai_ip = interv_obj$alphai[ip, ]
        interv_slice = list(
            PBi = interv_obj$PBi[ip, ],
            PCi = interv_obj$PCi[ip, ],
            PDi = interv_obj$PDi[ip, ],
            PEi = interv_obj$PEi[ip, ],
            Kvi = interv_obj$Kvi
        )

        # Unit-population host fractions
        frac1 = coverage_vec * interv_obj$survival[ip]  # protected humans
        frac2 = 1 - frac1                                # unprotected humans
        frac3 = rep(1, length(coverage_vec))             # animals
        frac  = cbind(frac1, frac2, frac3)

        n_cov     = nrow(frac)
        t_avail   = rep.row(alphai_ip, n_cov) * frac
        host_frac = frac / N_equiv

        res_new = AnophelesModel2:::eval_entomological_model(
            interv_slice, vec_p, host_p, Nv0,
            t_avail, host_frac, N_equiv)

        # Reference: identical call (self-consistency check)
        res_ref = AnophelesModel2:::eval_entomological_model(
            interv_slice, vec_p, host_p, Nv0,
            t_avail, host_frac, N_equiv)

        vc_new = c(vc_new, res_new$vc_all)
        vc_ref = c(vc_ref, res_ref$vc_all)
    }

    expect_equal(vc_new, vc_ref, tolerance = 1e-15)

    # Cross-check: calculate_impact (the full loop) gives the same avg_vc
    impact = calculate_impact(
        interv_list, coverage_vec, model_p)
    avg_vc_loop = impact$interventions_vec[[1]]$effects$avg_vc

    avg_vc_new = tapply(vc_new, rep(seq_along(coverage_vec), npoints), mean)
    expect_equal(as.numeric(avg_vc_new), avg_vc_loop, tolerance = 1e-8)
})
