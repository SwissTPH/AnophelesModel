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

test_that("misspelled decay component names are errors, not silently ignored", {
    # halflife_insecticide / component_decay of build_intervention()
    expect_error(validation_build(c(PrePrandial = 0.4),
                                  halflife_insecticide = c(Preprandial = 2)),
                 "halflife_insecticide: unknown component\\(s\\) Preprandial")
    expect_error(validation_build(c(PrePrandial = 0.4), component_decay = list(
        Preprandial = list(decay = "exponential", L = 30))),
        "component_decay: unknown component\\(s\\) Preprandial")
    expect_error(validation_build(c(PrePrandial = 0.4), halflife_insecticide = 2),
                 "must be named by component")
    expect_error(validation_build(c(PrePrandial = 0.4),
                                  halflife_insecticide = c(PrePrandial = 2, PrePrandial = 3)),
                 "duplicated component")
    # The same through om_vector_control()
    expect_error(om_vector_control(c(PrePrandial = 0.4), decay_attrition = "none",
                                   halflife_insecticide = c(Preprandial = 2)),
                 "unknown component")
    # Valid names still work
    expect_no_error(validation_build(c(PrePrandial = 0.4),
                                     halflife_insecticide = c(PrePrandial = 2)))
    # Batch tables: misspelled decay columns of run_interventions()
    ps = validation_ps
    ps$halflife_pree = 2
    expect_error(validation_run(ps), "unknown decay column\\(s\\) halflife_pree")
    ps = validation_ps
    ps$decay_Pre = "exponential"
    expect_error(validation_run(ps), "unknown decay column\\(s\\) decay_Pre")
    ps = validation_ps
    ps$halflife_pre = 2
    ps$my_label = "kept"                     # other columns are still allowed
    expect_no_error(validation_run(ps))
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
        expect_error(validation_build(c(KillingDuringHostSeekingAbsolute = value)), "efficacy")
        ps = validation_ps
        ps$preprandial = 0
        ps$killing_during_host_seeking_absolute = value
        expect_error(validation_run(ps), "killing_during_host_seeking_absolute")
    }
    expect_error(validation_build(c(KillingDuringHostSeekingAbsolute = NA_real_)), "missing")
    ps = validation_ps
    ps$preprandial = 0
    ps$killing_during_host_seeking_absolute = 2
    interv = validation_build(c(Deterrency = 0.4, PostPrandial = 0.2,
                                KillingDuringHostSeekingAbsolute = 2))
    expect_equal(validation_run(ps)$avg_impact,
        calculate_impact(interv, c(0, 1), validation_model)$avg_impact)
    # NA is the documented inactive marker in batch tables, including a
    # logical all-NA column. It must not disable the standard model's killing.
    ps = validation_ps
    ps$killing_during_host_seeking_absolute = NA
    expect_equal(validation_run(ps)$avg_impact, validation_run()$avg_impact)
    ps = validation_ps[rep(1, 2), ]
    ps$preprandial = 0
    ps$killing_during_host_seeking_absolute = c(2, NA_real_)
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

test_that("incomplete or misspelled decay specifications are errors", {
    # component_decay without a shape, or with an unknown field
    expect_error(validation_build(c(PostPrandial = 0.4),
                                  component_decay = list(PostPrandial = list(L = 100))),
                 "component_decay\\$PostPrandial must be a list with a decay shape")
    expect_error(validation_build(c(PostPrandial = 0.4), component_decay = list(
        PostPrandial = list(decay = "weibull", L = 100, shape = 2))),
        "unknown field\\(s\\) shape")
    # Batch tables: L / kappa without a shape, missing attrition shape column
    ps = validation_ps
    ps$L_det = 100
    expect_error(suppressMessages(run_interventions(ps, validation_model, c(0, 1),
                                                    duration_years = 2 / 365)),
                 "Deterrency.*row 1.*without a decay shape")
    ps = validation_ps
    ps$decay_attrition = NULL
    expect_error(validation_run(ps), "Missing column decay_attrition")
    # Attrition errors name the row
    ps = validation_ps[c(1, 1), ]
    ps$decay_attrition = c("none", "weibull")
    expect_error(validation_run(ps), "Attrition decay.*row 2.*requires L")
    # A missing shape_attrition is not taken from another column ($ partial
    # matching picked up kappa_det): a Weibull attrition then lacks its shape
    ps = validation_ps
    ps$shape_attrition = NULL
    ps$decay_attrition = "weibull"
    ps$halflife_attrition = 30
    ps$decay_det = "weibull"
    ps$L_det = 1000
    ps$kappa_det = 5
    expect_error(validation_run(ps), "Attrition decay.*row 1.*requires kappa")
    # Shapes without a shape parameter do not need the column
    ps$decay_attrition = "exponential"
    expect_equal(nrow(validation_run(ps)), 2)
})

test_that("arguments that must be single values or flags are checked", {
    for (args in list(list(halflife_attrition = c(30, 60), decay_attrition = "weibull",
                           shape_attrition = 2),
                      list(decay_attrition = c("none", "none")),
                      list(name = c("a", "b")))) {
        expect_error(suppressMessages(do.call(build_intervention, c(list(
            efficacy = c(Deterrency = 0.4), duration_years = 2 / 365,
            model_p = validation_model), args))), "single", info = names(args)[1])
    }
    expect_error(suppressMessages(build_intervention(
        c(Deterrency = 0.4), decay_attrition = "none", duration_years = 2 / 365,
        model_p = validation_model, insecticide_decay = NA)), "insecticide_decay must be")
    expect_error(suppressMessages(run_interventions(validation_ps, validation_model, c(0, 1),
                                                    duration_years = 2 / 365,
                                                    insecticide_decay = "yes")),
                 "insecticide_decay must be")
})

test_that("uncertainty draws cannot mix the standard and dummy-host models", {
    std = validation_ps
    dh  = validation_ps
    dh$preprandial = 0
    std$killing_during_host_seeking_absolute = NA
    dh$killing_during_host_seeking_absolute  = 0.5
    expect_error(suppressMessages(run_interventions(std, validation_model, c(0, 1),
        interv_param_sets = rbind(std, dh), duration_years = 2 / 365)),
        "mixes the standard and dummy-host models")
})

test_that("custom vector tables need the model's columns, in range", {
    vec = vec_ent_param[vec_ent_param$species_name == "Anopheles gambiae ss/coluzzii" &
                            vec_ent_param$level == "species", ]
    expect_no_error(def_vector_params(vector_table = vec, verbose = FALSE))
    no_tau = vec[setdiff(names(vec), "tau")]
    expect_error(def_vector_params(vector_table = no_tau, verbose = FALSE),
                 "missing column\\(s\\) tau")
    for (case in list(list(col = "tau", v = 0, msg = "tau must be > 0"),
                      list(col = "A0", v = 0, msg = "A0 must be in \\(0, 1\\)"),
                      list(col = "A0", v = 1, msg = "A0 must be in \\(0, 1\\)"),
                      list(col = "M", v = 1.2, msg = "M must be in \\[0, 1\\]"))) {
        bad = vec
        bad[[case$col]] = case$v
        expect_error(def_vector_params(vector_table = bad, verbose = FALSE), case$msg,
                     info = case$col)
    }
    # Draws of run_interventions(): the error names the draw
    bad = vec
    bad$tau = 0
    expect_error(validation_run(vec_param_sets = rbind(vec, bad)),
                 "vec_param_sets draw 2.*tau must be > 0")
    # A non-integer incubation period (whole days in the model and in OpenMalaria)
    frac = vec
    frac$ts = 10.5
    expect_error(def_vector_params(vector_table = frac, verbose = FALSE),
                 "ts \\(extrinsic incubation period\\) must be a whole number")
    expect_error(validation_run(vec_param_sets = rbind(vec, frac)),
                 "vec_param_sets draw 2.*whole number")
    tab = vec_ent_param
    tab$ts[1] = 10.5
    expect_error(AnophelesModel:::check_bionomics_table(tab),
                 "Bionomics table\\$ts.*whole number")
    # An incubation period shorter than the resting duration
    short = vec
    short$tau = 3
    short$ts = 2
    mp = build_model_obj(def_vector_params(vector_table = short, verbose = FALSE),
                         def_host_params(verbose = FALSE))
    expect_error(suppressMessages(run_interventions(validation_ps, mp, c(0, 1),
                                                    duration_years = 2 / 365,
                                                    insecticide_decay = FALSE)),
                 "incubation period ts.*shorter than the resting duration")
})

test_that("an unknown activity pattern ID is an error", {
    expect_error(def_activity_patterns(list(HBI = 99999, HBO = 166, humans_indoors = 4,
                                            humans_in_bed = 24)),
                 "Unknown activity pattern ID 99999")
})
