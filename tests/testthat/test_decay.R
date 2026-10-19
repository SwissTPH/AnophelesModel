library(AnophelesModel2)
library(testthat)

L_test = 100
t_test = 0:(3 * L_test)
shapes = list(weibull          = 2,
              exponential      = NULL,
              hill             = 2,
              linear           = NULL,
              step             = NULL,
              "smooth-compact" = 2,
              none             = NULL)

# ---------------------------------------------------------------------------
# 1. Every shape starts at 1, stays in [0, 1] and is non-increasing
# ---------------------------------------------------------------------------
test_that("decay_function is bounded, starts at 1 and is non-increasing", {
    for (shape in names(shapes)) {
        v = decay_function(L = L_test, kappa = shapes[[shape]],
                           datej = t_test, decay = shape)
        expect_length(v, length(t_test))
        expect_equal(v[1], 1, info = shape)
        expect_true(all(v >= 0 & v <= 1), info = shape)
        expect_true(all(diff(v) <= 0), info = shape)
    }
})

# ---------------------------------------------------------------------------
# 2. Meaning of L: half-life vs time to zero
# ---------------------------------------------------------------------------
test_that("L is the half-life for weibull, exponential and hill", {
    for (shape in c("weibull", "exponential", "hill")) {
        expect_equal(decay_function(L = L_test, kappa = shapes[[shape]],
                                    datej = L_test, decay = shape),
                     0.5, info = shape)
    }
})

test_that("L is the time to zero for linear, step and smooth-compact", {
    after = t_test[t_test >= L_test]
    for (shape in c("linear", "step", "smooth-compact")) {
        expect_silent(decay_function(L = L_test, kappa = shapes[[shape]],
                                     datej = after, decay = shape))
        v = decay_function(L = L_test, kappa = shapes[[shape]],
                           datej = after, decay = shape)
        expect_true(all(v == 0), info = shape)
    }
    # Before L, the effect is still positive
    for (shape in c("linear", "step", "smooth-compact")) {
        expect_gt(decay_function(L = L_test, kappa = shapes[[shape]],
                                 datej = L_test - 1, decay = shape), 0)
    }
})

# ---------------------------------------------------------------------------
# 3. Names are case-insensitive, aliases map to the canonical shape
# ---------------------------------------------------------------------------
test_that("decay names are case-insensitive and aliases are accepted", {
    expect_equal(decay_function(L_test, 2, t_test, "Weibull"),
                 decay_function(L_test, 2, t_test, "weibull"))
    expect_equal(decay_function(L_test, 2, t_test, "smoothcompact"),
                 decay_function(L_test, 2, t_test, "smooth-compact"))
    expect_equal(decay_function(L_test, NULL, t_test, "constant"),
                 rep(1, length(t_test)))
    expect_equal(decay_function(L_test, NULL, t_test, "no decay"),
                 rep(1, length(t_test)))
    # L is not needed for no decay
    expect_equal(decay_function(NULL, NULL, t_test, "none"),
                 rep(1, length(t_test)))
})

# ---------------------------------------------------------------------------
# 4. Input errors are explicit
# ---------------------------------------------------------------------------
test_that("decay_function errors clearly on invalid input", {
    expect_error(decay_function(L_test, 2, t_test, "gompertz"), "Unknown decay type")
    expect_error(decay_function(L_test, 2, t_test, c("weibull", "hill")), "single string")
    for (shape in c("weibull", "hill", "smooth-compact")) {
        expect_error(decay_function(L_test, NULL, t_test, shape), "kappa")
        expect_error(decay_function(L_test, NA,   t_test, shape), "kappa")
    }
    expect_error(decay_function(0,    2, t_test, "weibull"), "L to be a single positive")
    expect_error(decay_function(-5,   2, t_test, "linear"),  "L to be a single positive")
    expect_error(decay_function(NULL, 2, t_test, "weibull"), "L to be a single positive")
})

# ---------------------------------------------------------------------------
# 5. Attrition shorter than the intervention duration, through the pipeline
# ---------------------------------------------------------------------------
test_that("build_intervention survival stays in [0, 1] after L", {
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"),
                              def_host_params())
    for (shape in c("smooth-compact", "linear", "step")) {
        interv = suppressMessages(build_intervention(
            efficacy = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
            halflife_attrition = 180, shape_attrition = shapes[[shape]],
            decay_attrition = shape, duration_years = 1, model_p = model_p))
        surv = interv[[1]]$effects$survival
        expect_true(all(surv >= 0 & surv <= 1), info = shape)
        expect_true(all(surv[181:length(surv)] == 0), info = shape)   # days >= 180
    }
})

test_that("run_interventions handles attrition reaching zero before the end", {
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"),
                              def_host_params())
    param_sets = data.frame(
        name               = c("linear", "step", "smooth-compact"),
        deterrency         = 0.5,
        preprandial        = 0.4,
        postprandial       = 0.3,
        halflife_attrition = 90,
        shape_attrition    = c(NA, NA, 2),
        decay_attrition    = c("linear", "step", "smooth-compact"),
        stringsAsFactors   = FALSE
    )
    res = suppressMessages(run_interventions(param_sets, model_p,
                                             coverage_vec = c(0.5, 1),
                                             duration_years = 1))
    expect_true(all(is.finite(res$avg_impact)))
    expect_true(all(res$avg_impact >= 0 & res$avg_impact <= 1))
})

# ---------------------------------------------------------------------------
# 6. Linear insecticide decay (halflife_insecticide, years) and its switch
# ---------------------------------------------------------------------------
build_ins = function(insecticide_decay, ...) {
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"),
                              def_host_params())
    suppressMessages(build_intervention(
        efficacy = c(Deterrency = 0.4, PrePrandial = 0.2, PostPrandial = 0.3),
        halflife_insecticide = c(Deterrency = 4, PrePrandial = -6),
        duration_years = 3, model_p = model_p,
        insecticide_decay = insecticide_decay, ...))[[1]]
}

test_that("halflife_insecticide is a linear time-to-zero in years, negatives kept", {
    interv = build_ins(TRUE, decay_attrition = "none")
    e      = interv$effects
    d      = c(0, 365, 3 * 365 - 1)
    ratio  = function(m) m[d + 1, 1] / m[d + 1, 2]
    expect_equal(ratio(e$alphai), 1 - 0.4 * (1 - d / (365 * 4)))
    expect_equal(ratio(e$PBi),    1 - 0.2 * (1 - d / (365 * -6)))  # effect increases
    expect_equal(ratio(e$PCi),    rep(1 - 0.3, length(d)))         # no insecticide decay
})

test_that("insecticide_decay = FALSE keeps initial efficacies throughout", {
    interv = build_ins(FALSE, decay_attrition = "none")
    e      = interv$effects
    expect_equal(unique(e$alphai[, 1] / e$alphai[, 2]), 1 - 0.4)
    expect_equal(unique(e$PBi[, 1]    / e$PBi[, 2]),    1 - 0.2)
    expect_equal(unique(e$PCi[, 1]    / e$PCi[, 2]),    1 - 0.3)
    expect_null(interv$fitted_decay)
})

test_that("run_interventions insecticide_decay = FALSE ignores insecticide columns", {
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"),
                              def_host_params())
    base = data.frame(name = "x", deterrency = 0.4, preprandial = 0.2,
                      postprandial = 0.3, halflife_attrition = 180,
                      shape_attrition = 2, decay_attrition = "weibull",
                      stringsAsFactors = FALSE)
    with_ins = cbind(base, halflife_det = 4, halflife_pre = 4, halflife_post = 4)
    run = function(ps, ...) suppressMessages(
        run_interventions(ps, model_p, c(0.5, 1), duration_years = 1, ...))
    off = run(with_ins, insecticide_decay = FALSE)
    expect_equal(off$avg_impact, run(base)$avg_impact)
    expect_false(isTRUE(all.equal(off$avg_impact, run(with_ins)$avg_impact)))
})

# ---------------------------------------------------------------------------
# 7. Explicit component shapes take priority and must be complete
# ---------------------------------------------------------------------------

test_that("constant component shapes override linear decay through both VC interfaces", {
    mp = build_model_obj(def_vector_params(), def_host_params())
    ps = data.frame(deterrency = 0, preprandial = 0.4, postprandial = 0,
                     decay_attrition = "none", halflife_attrition = NA_real_,
                     shape_attrition = NA_real_, halflife_pre = 1)
    run = function(p) suppressMessages(run_interventions(
        p, mp, 0.8, duration_years = 1, keep_time = TRUE))
    constant = ps
    constant$halflife_pre = NA_real_
    expected = run(constant)
    for (shape in c("none", "constant", "no decay", "NONE")) {
        p = ps
        p$decay_pre = shape
        actual = run(p)
        expect_equal(actual$time$vc, expected$time$vc, info = shape)
        expect_equal(actual$impact$avg_impact, expected$impact$avg_impact)
        iv = suppressMessages(build_intervention(
            c(PrePrandial = 0.4), decay_attrition = "none",
            halflife_insecticide = c(PrePrandial = 1),
            component_decay = list(PrePrandial = list(decay = shape)),
            duration_years = 1, model_p = mp))
        built = calculate_impact(iv, 0.8, mp, keep_time = TRUE)
        expect_equal(built$time$vc, expected$time$vc, info = shape)
    }
    expect_lt(tail(run(ps)$time$impact, 1), expected$time$impact[1])
})

test_that("incomplete explicit component decays fail instead of falling back", {
    mp = build_model_obj(def_vector_params(), def_host_params())
    specs = list(list(decay = "exponential"),
                 list(decay = "weibull", L = 90),
                 list(decay = "hill", L = 90, kappa = 0),
                 list(decay = "linear", L = 0),
                 list(decay = "typo"))
    for (spec in specs) {
        expect_error(suppressMessages(build_intervention(
            c(PrePrandial = 0.4), decay_attrition = "none",
            halflife_insecticide = c(PrePrandial = 1),
            component_decay = list(PrePrandial = spec),
            duration_years = 2 / 365, model_p = mp)),
            "PrePrandial.*row 1")
        ps = data.frame(deterrency = 0, preprandial = 0.4, postprandial = 0,
                          decay_attrition = "none", halflife_attrition = NA_real_,
                          shape_attrition = NA_real_, halflife_pre = 1,
                          decay_pre = c(NA_character_, spec$decay))
        if (!is.null(spec$L)) ps$L_pre = c(NA_real_, spec$L)
        if (!is.null(spec$kappa)) ps$kappa_pre = c(NA_real_, spec$kappa)
        expect_error(suppressMessages(run_interventions(
            ps, mp, 0.8, duration_years = 2 / 365)), "PrePrandial.*row 2")
        expect_no_error(suppressMessages(run_interventions(
            ps, mp, 0.8, duration_years = 2 / 365, insecticide_decay = FALSE)))
    }
})

test_that("valid general decay overrides linear decay with the expected daily VC", {
    mp = build_model_obj(def_vector_params(), def_host_params())
    iv = suppressMessages(build_intervention(
        c(PrePrandial = 0.4), decay_attrition = "none",
        halflife_insecticide = c(PrePrandial = 1),
        component_decay = list(PrePrandial = list(decay = "exponential", L = 100)),
        duration_years = 201 / 365, model_p = mp))
    ps = iv[[1]]$param_set
    built = calculate_impact(iv, 0.8, mp, keep_time = TRUE)
    batch = suppressMessages(run_interventions(
        ps, mp, 0.8, duration_years = 201 / 365, keep_time = TRUE))
    expect_equal(built$time$vc, batch$time$vc)
    for (day in c(0, 100, 200)) {
        snapshot = suppressMessages(build_intervention(
            c(PrePrandial = 0.4 * 2^(-day / 100)), decay_attrition = "none",
            duration_years = 2 / 365, model_p = mp, insecticide_decay = FALSE))
        expected = calculate_impact(snapshot, 0.8, mp)$avg_vc
        expect_equal(built$time$vc[day + 1], expected, tolerance = 1e-12)
    }
})

test_that("linear insecticide decay rejects zero and NaN while keeping existing conventions", {
    mp = build_model_obj(def_vector_params(), def_host_params())
    for (hl in c(0, NaN)) {
        expect_error(suppressMessages(build_intervention(
            c(PrePrandial = 0.4), decay_attrition = "none",
            halflife_insecticide = c(PrePrandial = hl),
            duration_years = 2 / 365, model_p = mp)), "PrePrandial.*row 1.*non-zero")
    }
    ps = data.frame(deterrency = 0, preprandial = 0.4, postprandial = 0,
                     decay_attrition = "none", halflife_attrition = NA_real_,
                     shape_attrition = NA_real_, halflife_pre = c(NA_real_, Inf, -2))
    mm = AnophelesModel2:::make_mult_matrices(ps, c(0, 365))
    expect_equal(mm$mult_pre[1:2, ], matrix(0.6, 2, 2))
    expect_equal(mm$mult_pre[3, ], c(0.6, 0.4))
    ps$halflife_pre[2] = 0
    expect_error(suppressMessages(run_interventions(
        ps, mp, 0.8, duration_years = 2 / 365)), "PrePrandial.*row 2.*non-zero")
})

# ---------------------------------------------------------------------------
# 8. Weibull fitted to attrition x insecticide decay (for OpenMalaria export)
# ---------------------------------------------------------------------------
test_that("fitted_decay recovers the attrition Weibull when there is no insecticide decay", {
    interv = build_ins(TRUE, halflife_attrition = 180, shape_attrition = 2,
                       decay_attrition = "weibull")
    fd = interv$fitted_decay
    expect_setequal(fd$component, c("Deterrency", "PrePrandial", "PostPrandial"))
    post = fd[fd$component == "PostPrandial", ]
    expect_equal(post$L,     180, tolerance = 1e-3)
    expect_equal(post$kappa, 2,   tolerance = 1e-3)
    expect_equal(post$a,     1,   tolerance = 1e-3)
})

test_that("fitted_decay accounts for the extra insecticide decay", {
    interv = build_ins(TRUE, halflife_attrition = 2 * 365, shape_attrition = 2,
                       decay_attrition = "weibull")
    fd  = interv$fitted_decay
    det = fd[fd$component == "Deterrency", ]
    expect_lt(det$L, 2 * 365)        # faster decay than attrition alone
    expect_lt(det$rmse, 0.05)        # a single Weibull approximates the product
})
