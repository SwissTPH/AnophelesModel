validation_model = build_model_obj(def_vector_params(), def_host_params())
validation_ps = data.frame(deterrency = 0.4, preprandial = 0.3,
                           postprandial = 0.2, halflife_attrition = NA_real_,
                           shape_attrition = NA_real_, decay_attrition = "none")
validation_build = function(efficacy = c(Deterrency = 0.4), duration_years = 2 / 365, ...) {
    suppressMessages(build_intervention(efficacy, decay_attrition = "none",
        duration_years = duration_years, model_p = validation_model,
        insecticide_decay = FALSE, ...))
}
validation_run = function(ps = validation_ps, coverage = c(0, 1),
                          duration_years = 2 / 365, ...) {
    suppressMessages(run_interventions(ps, validation_model, coverage,
        duration_years = duration_years, insecticide_decay = FALSE, ...))
}

test_that("efficacy requires a non-empty numeric vector with unique valid names", {
    invalid = list(0.8, numeric(0), c(Deterrency = "0.8"),
                   list(Deterrency = 0.8), c(Deterrency = 0.8, 0.2),
                   c(Deterrency = 0.8, Deterrency = 0.2),
                   c(Typo = 0.8), matrix(0.8, 1, 1))
    for (efficacy in invalid) expect_error(validation_build(efficacy), "efficacy|component")
})

test_that("both interfaces reject invalid probability efficacies", {
    mapping = c(Deterrency = "deterrency", PrePrandial = "preprandial",
                PostPrandial = "postprandial")
    for (component in names(mapping)) {
        for (value in list(-0.1, 1.1, NA_real_, NaN, Inf, -Inf, "0.5")) {
            expect_error(validation_build(stats::setNames(value, component)),
                         "efficacy", info = component)
            ps = validation_ps
            ps[[mapping[[component]]]] = value
            expect_error(validation_run(ps), mapping[[component]], info = component)
            # Invalid values in later uncertainty draws are checked too.
            expect_error(validation_run(interv_param_sets = list(validation_ps, ps)),
                         "interv_param_sets.*2.*efficacy|interv_param_sets.*2.*finite",
                         info = component)
        }
    }
})

test_that("host-seeking killing is a finite non-negative rate, not a probability", {
    for (value in list(-0.1, Inf, -Inf, NaN, "2", matrix(NA, 1, 1))) {
        expect_error(validation_build(c(HostSeekingKilling = value)), "efficacy")
        ps = validation_ps
        ps$preprandial = 0
        ps$host_seeking_killing = value
        expect_error(validation_run(ps), "host_seeking_killing")
    }
    expect_error(validation_build(c(HostSeekingKilling = NA_real_)), "missing")
    ps = validation_ps
    ps$preprandial = 0
    ps$host_seeking_killing = 2
    interv = validation_build(c(Deterrency = 0.4, PostPrandial = 0.2,
                                HostSeekingKilling = 2))
    expect_equal(validation_run(ps)$avg_impact,
        calculate_impact(interv, c(0, 1), validation_model)$avg_impact)
    # NA is the documented inactive marker in batch tables, including a
    # logical all-NA column. It must not disable the standard model's killing.
    ps = validation_ps
    ps$host_seeking_killing = NA
    expect_equal(validation_run(ps)$avg_impact, validation_run()$avg_impact)
    ps = validation_ps[rep(1, 2), ]
    ps$preprandial = 0
    ps$host_seeking_killing = c(2, NA_real_)
    expect_true(all(is.finite(validation_run(ps)$avg_impact)))
    expect_equal(ncol(validation_build(mode = "dummy_host")[[1]]$effects$alphai), 4)
})

test_that("batch efficacy tables require rows and unambiguous efficacy columns", {
    for (ps in list(NULL, list(deterrency = 0.4), validation_ps[FALSE, ]))
        expect_error(validation_run(ps), "param_sets.*non-empty data frame")
    ps = validation_ps
    ps$deterrency = NULL
    expect_error(validation_run(ps), "missing efficacy.*deterrency")
    ps = validation_ps
    names(ps)[2] = "deterrency"
    expect_error(validation_run(ps), "unique column names")
    expect_error(validation_run(interv_param_sets = list()), "interv_param_sets.*non-empty")
    expect_error(validation_run(interv_param_sets = validation_ps[FALSE, ]), "interv_param_sets.*non-empty")
    ps = rbind(validation_ps, validation_ps)
    ps$deterrency[2] = -0.1
    expect_error(validation_run(interv_param_sets = ps), "interv_param_sets.*2.*deterrency")
})

test_that("coverages are validated before exposure scaling in all interfaces", {
    interv = validation_build()
    invalid = list(-0.1, 1.1, NA_real_, NaN, Inf, -Inf, "0.5", numeric(0), matrix(0.5))
    for (coverage in invalid) {
        expect_error(calculate_impact(interv, coverage, validation_model, exposure = 0),
                     "coverage_vec")
        expect_error(validation_run(coverage = coverage, exposure = 0), "coverage_vec")
        expect_error(validation_run(coverage_sets = list(c(0, 1), coverage)),
                     "coverage_sets.*2")
        expect_error(suppressMessages(calculate_combined_impact(
            intervention1 = interv[[1]], intervention2 = interv[[1]],
            cov_intervention1 = coverage, cov_intervention2 = 0.5,
            model_p = validation_model)), "cov_intervention1")
        expect_error(suppressMessages(calculate_combined_impact(
            intervention1 = interv[[1]], intervention2 = interv[[1]],
            cov_intervention1 = 0.5, cov_intervention2 = coverage,
            model_p = validation_model)), "cov_intervention2")
        expect_error(suppressMessages(calculate_combined_impact(
            intervention1 = interv[[1]], intervention2 = interv[[1]],
            cov_intervention1 = 0.5, cov_intervention2 = 0.5,
            cov_combination = coverage, model_p = validation_model)), "cov_combination")
    }
    expect_error(validation_run(coverage_sets = list()), "coverage_sets.*non-empty")
    expect_error(validation_run(coverage_sets = c(0.5, 1)), "coverage_sets.*list")
    expect_error(suppressMessages(calculate_combined_impact(
        intervention1 = interv[[1]], intervention2 = interv[[1]],
        cov_intervention1 = c(0.5, 1), cov_intervention2 = 0.5,
        model_p = validation_model)), "cov_intervention1.*single")
})

test_that("impact errors name the draw, parameter sets or intervention", {
    vec = vec_ent_param[vec_ent_param$species_name == "Anopheles gambiae ss/coluzzii" &
                            vec_ent_param$level == "species", ]
    bad = vec
    bad$Chi = 0   # zero human blood index: baseline vectorial capacity is zero
    expect_error(validation_run(validation_ps[rep(1, 3), ], vec_param_sets = rbind(vec, bad)),
                 "draw 2.*zero.*parameter set\\(s\\) 1, 2, 3")

    zero_model = build_model_obj(def_vector_params(vector_table = bad, verbose = FALSE),
                                 def_host_params())
    interv = suppressMessages(build_intervention(
        c(Deterrency = 0.4), decay_attrition = "none", duration_years = 2 / 365,
        model_p = zero_model, insecticide_decay = FALSE))
    msg = tryCatch(calculate_impact(interv, c(0, 1), zero_model),
                   error = conditionMessage)
    expect_match(msg, "intervention 1.*zero")
    expect_false(grepl("parameter set", msg))   # one set per intervention
})

test_that("duration is finite, positive and scalar with consistent subday handling", {
    for (duration in list(-1, 0, NA_real_, NaN, Inf, -Inf, "1", numeric(0), c(1, 2))) {
        expect_error(validation_build(duration_years = duration), "duration_years")
        expect_error(validation_run(duration_years = duration), "duration_years")
    }
    interv = validation_build(c(Deterrency = 0.4, PrePrandial = 0.3, PostPrandial = 0.2),
                               duration_years = 0.1 / 365)
    expect_equal(interv[[1]]$duration, 1)
    expect_equal(validation_run(duration_years = 0.1 / 365)$avg_impact,
        calculate_impact(interv, c(0, 1), validation_model)$avg_impact)
    expect_error(validation_run(exposure_sets = numeric(0)), "exposure")
})
