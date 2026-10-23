library(AnophelesModel2)
library(testthat)

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

make_model_p = function() {
    vec_p  = def_vector_params("Anopheles gambiae ss/coluzzii")
    host_p = def_host_params()
    build_model_obj(vec_p, host_p)
}

# Reference: run one param-set row through build_intervention +
# reference_calculate_impact (the pre-vectorisation loop, helper-reference.R)
# and return a data.frame matching run_interventions output.
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
        impact = reference_calculate_impact(interv, coverage_vec, model_p)
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
test_that("run_interventions matches the reference loop for a single param set", {
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
test_that("run_interventions matches the reference loop for multiple param sets", {
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
        AnophelesModel2::vec_ent_param$species_name == "Anopheles gambiae ss/coluzzii", ][1, ]
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
test_that("run_interventions dummy-host model matches the reference loop", {
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
    duration_years = 1   # longer than L: exercises behaviour after time to zero

    param_sets = data.frame(
        name               = c("linear_test", "hill_test"),
        deterrency         = c(0.5, 0.5),
        preprandial        = c(0.3, 0.3),
        postprandial       = c(0.2, 0.2),
        halflife_attrition = c(120, 120),
        shape_attrition    = c(NA,  2),
        decay_attrition    = c("linear", "hill"),
        stringsAsFactors   = FALSE
    )

    res = run_interventions(param_sets, model_p, coverage_vec, duration_years = duration_years)

    expect_true(all(is.finite(res$avg_vc)))
    expect_true(all(is.finite(res$avg_impact)))
    expect_true(all(res$avg_vc >= 0))
    expect_true(all(res$avg_impact >= 0 & res$avg_impact <= 1))
})

# ---------------------------------------------------------------------------
# 7. Scale test: 500 param sets complete in reasonable time
# ---------------------------------------------------------------------------
test_that("run_interventions completes 500 param sets within 60 seconds", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.5, 1)
    duration_years = 50 / 365
    n              = 500

    withr::local_seed(42)
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
# 8. Reparameterisation check: eval_entomological_model with total
#    availabilities (t_avail = alphai x Ni) and host fractions (host_frac =
#    Ni / N_total) gives the vectorial capacity of Chitnis et al. 2008
#    (eq. 21, with alphai and Ni), written out independently here, for any
#    population size.
# ---------------------------------------------------------------------------
# Vectorial capacity (Nv0 = 1) of eq. 21 for one coverage, from per-host
# availabilities alphai and host numbers Ni (humans first, m = 2 humans)
chitnis_vc = function(alphai, Ni, P, vec_p, muvA) {
    total = sum(alphai * Ni) + muvA
    PA    = exp(-total * vec_p$td)
    PAi   = (1 - PA) * alphai * Ni / total
    cycle = P$PBi * P$PCi * P$PDi * P$PEi
    Pdf   = sum(PAi * cycle)
    psts  = AnophelesModel2:::calc_psts(vec_p$ts, vec_p$tau, PA, Pdf)$psts
    h = 1:2
    1 / sum(Ni[h]) * sum(PAi[h] * P$PBi[h]) * sum(PAi[h] * cycle[h]) /
        (1 - PA - Pdf)^2 * psts
}

test_that("eval_entomological_model reparameterisation gives the vectorial capacity of eq. 21", {
    model_p      = make_model_p()
    coverage_vec = c(0, 0.25, 0.5, 0.75, 1)
    duration_years = 10 / 365

    # Build intervention object to get alphai / PBi / etc. matrices
    interv_list = build_intervention(
        efficacy = c(Deterrency=0.5, PrePrandial=0.4, PostPrandial=0.3),
        halflife_attrition = 180, shape_attrition = 2, duration_years = duration_years,
        model_p = model_p, name = "test", decay_attrition = "weibull")
    interv_obj = interv_list[[1]]$effects

    vec_p  = model_p$vec_params
    host_p = model_p$host_params
    npoints = round(365 * duration_years)

    vc_new = NULL
    for (ip in seq_len(npoints)) {
        P = list(PBi = interv_obj$PBi[ip, ], PCi = interv_obj$PCi[ip, ],
                 PDi = interv_obj$PDi[ip, ], PEi = interv_obj$PEi[ip, ],
                 Kvi = interv_obj$Kvi)
        # Unit host populations, as the package: one human (protected share
        # coverage x survival) and one animal
        prot  = coverage_vec * interv_obj$survival[ip]
        Ni    = cbind(prot, 1 - prot, 1)
        N_tot = 2
        alpha = matrix(interv_obj$alphai[ip, ], nrow(Ni), 3, byrow = TRUE)
        res = AnophelesModel2:::eval_entomological_model(
            P, vec_p, host_p, 1L, t_avail = alpha * Ni,
            host_frac = Ni / N_tot, N_total = N_tot)
        expected = vapply(seq_along(coverage_vec), function(k)
            chitnis_vc(interv_obj$alphai[ip, ], Ni[k, ], P, vec_p, host_p$muvA),
            numeric(1))
        expect_equal(res$vc_all, expected, tolerance = 1e-12, info = paste("day", ip))
        vc_new = c(vc_new, res$vc_all)
    }

    # Averaged over the days: the reference loop's vectorial capacity
    impact = reference_calculate_impact(interv_list, coverage_vec, model_p)
    avg_vc = as.numeric(tapply(vc_new, rep(seq_along(coverage_vec), npoints), mean))
    expect_equal(avg_vc, unname(impact$interventions_vec[[1]]$effects$avg_vc),
                 tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Matrix dimensions: one-day and sub-day batches
# ---------------------------------------------------------------------------
test_that("one-day, sub-day and multi-day batches match separate single-row runs", {
    model_p = suppressMessages(make_model_p())
    ps = data.frame(deterrency = c(0.2, 0.5, 0.8), preprandial = c(0.1, 0.3, 0.5),
                    postprandial = 0.3, decay_attrition = c("weibull", "step", "linear"),
                    halflife_attrition = c(30, 2, 90), shape_attrition = 2,
                    stringsAsFactors = FALSE)
    cov = c(0.5, 0.8)
    for (days in c(1, 0.5, 10)) {
        run = function(p) suppressMessages(run_interventions(
            p, model_p, cov, duration_years = days / 365, keep_time = TRUE))
        batch = run(ps)
        # Attrition matrix: one row per parameter set, one column per day
        mm = AnophelesModel2:::make_mult_matrices(ps, seq_len(max(1, round(days))) - 1)
        expect_equal(dim(mm$surv_mat), c(nrow(ps), max(1L, round(days))), info = days)
        for (i in seq_len(nrow(ps))) {
            single = run(ps[i, ])
            rows = (i - 1) * length(cov) + seq_along(cov)
            expect_equal(batch$impact$avg_impact[rows], single$impact$avg_impact,
                         tolerance = 1e-12, info = paste(days, i))
            expect_equal(batch$impact$avg_vc[rows], single$impact$avg_vc,
                         tolerance = 1e-12, info = paste(days, i))
            t_batch = batch$time[batch$time$param_set == i, ]
            expect_equal(t_batch$vc, single$time$vc, tolerance = 1e-12, info = paste(days, i))
            expect_equal(t_batch$impact, single$time$impact, tolerance = 1e-12,
                         info = paste(days, i))
        }
    }
})
