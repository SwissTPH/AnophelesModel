library(AnophelesModel)
library(testthat)

########################################
# Tests for def_vector_params()
########################################
# This test checks whether custom values can be provided for the vector
# entomological parameters.
test_that("test: check loading custom entomological parameter values", {
    # Define custom values
    species_name = "Anopheles Anonymus"
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

    vec_obj = def_vector_params(mosquito_species = "Anopheles gambiae custom",
                                vector_table = custom_params)
    c_sol = as.list(custom_params)

    expect_equal(c_sol, vec_obj)
})

# This test checks whether providing non-numeric custom values for the
# vector entomological parameters throws an error
test_that("test: check loading non-numeric entomological parameter values", {
    # Define custom values
    species_name = "Anopheles err"
    M = 0.623
    M.sd = 0.01
    Chi = 0.939
    A0 = 0.313
    A0.sd = 0.01
    zeta.3 = "1abds"
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
                                                   tau, ts, to, endophily,
                                                   endophily.sd,
                                                   endophagy, endophagy.sd))

    expect_error(def_vector_params(mosquito_species = "Anopheles err",
                                   vector_table = custom_params),
                 "Must be of type 'numeric'")
})

# This test checks whether providing negative custom values for the
# vector entomological parameters throws an error
test_that("test: check loading negative entomological parameter values", {
    # Define custom values
    species_name = "Anopheles err"
    M = 0.623
    M.sd = 0.01
    Chi = 0.939
    A0 = 0.313
    A0.sd = 0.01
    zeta.3 = -1
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
                                                   tau, ts, to, endophily,
                                                   endophily.sd,
                                                   endophagy, endophagy.sd))

    expect_error(def_vector_params(mosquito_species = "Anopheles err",
                                   vector_table = custom_params),
                 "is not >= 0")
})

#######################################
# Tests for def_host_params()
#######################################
# This test checks whether custom values can be provided for the host
# entomological parameters.
test_that("test: check loading custom host entomological parameter values", {

    species_name = "Anopheles Anonymus"
    host = c("human", "animal")
    PBi = c(0.67, 0.44)
    PCi = c(0.33, 0.434)
    PDi = c(0.647, 0.344)
    PEi = c(0.672, 0.214)
    Kvi = c(0.52, 0.314)

    # Define data frame with custom values
    custom_params = cbind.data.frame(species_name, host,
                                     PBi, PCi, PDi, PEi, Kvi)

    my_host_obj = def_host_params(host_table = custom_params)

    c_sol = as.list(custom_params)

    expect_equal(c_sol, my_host_obj)

    # species_name is optional (host parameters are species-independent)
    no_species = custom_params[names(custom_params) != "species_name"]
    expect_equal(def_host_params(host_table = no_species, verbose = FALSE),
                 as.list(no_species))
})

test_that("custom host row order does not change parameters or model results", {
    custom = as.data.frame(def_host_params(verbose = FALSE))
    custom$PBi = c(0.95, 0.9)
    custom$PCi = c(0.96, 0.94)
    custom$PDi = c(0.99, 0.98)
    custom$PEi = c(0.88, 0.87)
    custom$Kvi = c(0.03, 0)

    human_first = def_host_params(custom, verbose = FALSE)
    animal_first = def_host_params(custom[2:1, ], verbose = FALSE)
    expect_equal(human_first, as.list(custom))
    expect_equal(animal_first$host, c("human", "animal"))
    expect_equal(animal_first$Kvi, c(0.03, 0))
    expect_equal(animal_first, human_first)

    vec_p = def_vector_params()
    models = list(build_model_obj(vec_p, human_first),
                  build_model_obj(vec_p, animal_first))
    expect_equal(models[[2]], models[[1]])
    impacts = lapply(models, function(model_p) {
        interv = suppressMessages(build_intervention(
            c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
            decay_attrition = "none", duration_years = 2 / 365,
            model_p = model_p, insecticide_decay = FALSE))
        calculate_impact(interv, c(0, 0.5, 1), model_p)
    })
    expect_equal(impacts[[2]], impacts[[1]])
})

# This test checks whether non-numeric custom values cannot be provided
# for the host entomological parameters.
test_that("test: check loading non-numeric host parameter values", {

    species_name = "Anopheles Anonymus"
    host = c("human", "animal")
    PBi = c(0.67, "0.44asd")
    PCi = c(0.33, 0.434)
    PDi = c(0.647, 0.344)
    PEi = c(0.672, 0.214)
    Kvi = c(0.52, 0.314)

    # Define data frame with custom values
    custom_params = cbind.data.frame(species_name, host,
                                     PBi, PCi, PDi, PEi, Kvi)

    expect_error(def_host_params(host_table = custom_params),
                 "Must be of type 'numeric'")
})

# This test checks whether negative custom values cannot be provided
# for the host entomological parameters.
test_that("test: check loading negative host parameter values", {

    species_name = "Anopheles Anonymus"
    host = c("human", "animal")
    PBi = c(0.67, 0.44)
    PCi = c(0.33, 0.434)
    PDi = c(0.647, -0.344)
    PEi = c(0.672, 0.214)
    Kvi = c(0.52, 0.314)

    # Define data frame with custom values
    custom_params = cbind.data.frame(species_name, host,
                                     PBi, PCi, PDi, PEi, Kvi)

    expect_error(def_host_params(host_table = custom_params), "is not >= 0")
})

# This test checks whether custom values above 1 cannot be provided
# for the host entomological parameters.
test_that("test: check loading host parameter values larger than 1", {

    species_name = "Anopheles Anonymus"
    host = c("human", "animal")
    PBi = c(0.67, 0.44)
    PCi = c(0.33, 0.434)
    PDi = c(0.647, 1.344)
    PEi = c(0.672, 0.214)
    Kvi = c(0.52, 0.314)

    # Define data frame with custom values
    custom_params = cbind.data.frame(species_name, host,
                                     PBi, PCi, PDi, PEi, Kvi)

    expect_error(def_host_params(host_table = custom_params), "is not <= 1")
})

# This test checks whether custom values in a different format cannot be
# provided for the host entomological parameters.
test_that("test: check loading different format for host parameter values", {

    # Valid values: only the missing host column makes the table invalid
    species_name = "Anopheles Anonymus"
    PBi = c(0.67, 0.44)
    PCi = c(0.33, 0.434)
    PDi = c(0.647, 0.344)
    PEi = c(0.672, 0.214)
    Kvi = c(0.52, 0.314)

    # Define data frame with custom values
    custom_params = cbind.data.frame(species_name,
                                     PBi, PCi, PDi, PEi, Kvi)

    expect_error(def_host_params(host_table = custom_params),
                 "host_table\\$host.*setequal to \\{'human','animal'\\}")
})

# This test checks whether custom values cannot be
# provided for the host entomological parameters without specifying the human
# and animal values
test_that("test: check loading host parameters for other hosts than human and animal", {

    # Valid values: only the host names make the table invalid
    species_name = "Anopheles Anonymus"
    host = c("horse", "sheep")
    PBi = c(0.67, 0.44)
    PCi = c(0.33, 0.434)
    PDi = c(0.647, 0.344)
    PEi = c(0.672, 0.214)
    Kvi = c(0.52, 0.314)

    # Define data frame with custom values
    custom_params = cbind.data.frame(species_name, host,
                                     PBi, PCi, PDi, PEi, Kvi)

    expect_error(def_host_params(host_table = custom_params),
                 "\\{'horse','sheep'\\}")
})


#######################################
# Tests for def_host_params()
#######################################
# This test checks whether custom values can be provided for the activity
# patterns of humans and mosquitoes as a data frame
test_that("test: loading data frame of activity patterns with Haiti rythms", {
    # Biting of An. albimanus in Haiti, Dame Marie (Briet et al 2019, Table S5)
    HBO = c(0.25, 0.35, 0.37, 0.33, 0.36, 0.32, 0.13, 0.14, 0.09,
            0.15, 0.36, 0.23, 0.25)
    HBI = c(0.22, 0.21, 0.22, 0.10, 0.13, 0.17, 0.08, 0.12, 0.03,
            0.12, 0.17, 0.18, 0.25)
    # Human activity patterns from Briet et al 2019, Knutson et al. 2014
    humans_in_bed = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                      83.202297, 92.736838, 96.734455, 96.637604,
                      92.983136, 85.264515, 73.021653, 57.079495)/100
    humans_indoors = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                       83.202297, 92.736838, 96.734455, 96.637604,
                       92.983136, 85.264515, 73.021653, 57.079495)/100
    custom_params = as.data.frame(cbind(HBI, HBO,
                                        humans_indoors, humans_in_bed ))
    my.activity_obj = def_activity_patterns(custom_params)

    c_sol = as.list(custom_params)
    expect_equal(c_sol, my.activity_obj)
})

# This test checks whether custom values can be provided for the activity
# patterns of humans and mosquitoes as a list
test_that("test: loading list of activity patterns with Haiti rythms", {
    # Biting of An. albimanus in Haiti, Dame Marie (Briet et al 2019, Table S5)
    HBO = c(0.25, 0.35, 0.37, 0.33, 0.36, 0.32, 0.13, 0.14, 0.09,
            0.15, 0.36, 0.23, 0.25)
    HBI = c(0.22, 0.21, 0.22, 0.10, 0.13, 0.17, 0.08, 0.12, 0.03,
            0.12, 0.17, 0.18, 0.25)
    # Human activity patterns from Briet et al 2019, Knutson et al. 2014
    humans_in_bed = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                      83.202297, 92.736838, 96.734455, 96.637604,
                      92.983136, 85.264515, 73.021653, 57.079495)/100
    humans_indoors = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                       83.202297, 92.736838, 96.734455, 96.637604,
                       92.983136, 85.264515, 73.021653, 57.079495)/100
    custom_params = list(HBI = HBI, HBO = HBO, humans_indoors = humans_indoors,
                         humans_in_bed = humans_in_bed)
    my.activity_obj = def_activity_patterns(custom_params)

    c_sol = custom_params
    expect_equal(c_sol, my.activity_obj)
})

# This test checks whether negative custom values cannot be provided for
# the activity patterns of humans and mosquitoes.
test_that("test: check loading negative activity patterns with Haiti rythms", {
    HBO = c(0.25, 0.35, 0.37, 0.33, -0.36, 0.32, 0.13, 0.14, 0.09,
            0.15, 0.36, 0.23, 0.25)
    HBI = c(0.22, 0.21, 0.22, 0.10, 0.13, 0.17, 0.08, 0.12, 0.03,
            0.12, 0.17, 0.18, 0.25)
    humans_in_bed = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                      83.202297, 92.736838, 96.734455, 96.637604,
                      92.983136, 85.264515, 73.021653, 57.079495)/100
    humans_indoors = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                       83.202297, 92.736838, 96.734455, 96.637604,
                       92.983136, 85.264515, 73.021653, 57.079495)/100
    custom_params = as.data.frame(cbind(HBI, HBO,
                                        humans_indoors, humans_in_bed ))
    expect_error(def_activity_patterns(custom_params), "is not >= 0")
})

# This test checks whether incomplete custom values cannot be provided for
# the activity patterns of humans and mosquitoes.
test_that("test: check loading incomplete activity patterns", {
    HBO = c(0.25, 0.35, 0.37, 0.33, 0.36, 0.32, 0.13, 0.14, 0.09,
            0.15, 0.36, 0.23, 0.25)
    HBI = c(0.22, 0.21, 0.22, 0.10, 0.13, 0.17, 0.08, 0.12, 0.03,
            0.12, 0.17, 0.18, 0.25)
    humans_in_bed = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                      83.202297, 92.736838, 96.734455, 96.637604,
                      92.983136, 85.264515, 73.021653, 57.079495)/100
    custom_params = as.data.frame(cbind(HBI, HBO, humans_in_bed ))
    expect_error(def_activity_patterns(custom_params), "not 'NULL'")
})

# This test checks whether custom time series of different lenghts cannot be
# provided for the activity patterns of humans and mosquitoes.
test_that("test: check loading inequal activity patterns", {
    HBO = c(0.25, 0.35, 0.37, 0.33, 0.36, 0.32, 0.13, 0.14, 0.09,
            0.15, 0.36)
    HBI = c(0.22, 0.21, 0.22, 0.10, 0.13, 0.17, 0.08, 0.12, 0.03,
            0.12, 0.17, 0.18)
    humans_in_bed = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                      83.202297, 92.736838, 96.734455, 96.637604,
                      92.983136, 85.264515, 73.021653, 57.079495)/100
    humans_indoors = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964,
                       83.202297, 92.736838, 96.734455, 96.637604,
                       92.983136, 85.264515, 73.021653, 57.079495)/100
    custom_params = list(HBI = HBI, HBO = HBO, humans_in_bed = humans_in_bed,
                         humans_indoors = humans_indoors)
    expect_error(def_activity_patterns(custom_params), "lengths\\(activity_patterns\\)")
})

#######################################
# Tests for build_intervention() — replaces the removed def_interventions_effects()
#######################################
test_that("test: check defining intervention effects via build_intervention", {
    vec_params  = def_vector_params()
    hosts_params = def_host_params()
    model_params = build_model_obj(vec_p = vec_params, hosts_p = hosts_params)

    duration_years = 5 / 365
    npoints = round(365 * duration_years)

    # LLIN-like intervention: deterrency + pre/post-prandial killing
    llin = build_intervention(
        efficacy = c(Deterrency = 0.45, PrePrandial = 0.53, PostPrandial = 0.34),
        halflife_attrition = 2.64 * 365, shape_attrition = 1, duration_years = duration_years,
        model_p = model_params, name = "LLINs", decay_attrition = "weibull")[[1]]$effects

    # IRS-like intervention: deterrency + pre/post-prandial killing
    irs = build_intervention(
        efficacy = c(Deterrency = 0.53, PrePrandial = 0.56, PostPrandial = 0.24),
        halflife_attrition = 0.5 * 365, shape_attrition = 1, duration_years = duration_years,
        model_p = model_params, name = "IRS", decay_attrition = "weibull")[[1]]$effects

    # Effects matrices should have npoints rows and 3 host columns
    expect_equal(nrow(llin$alphai), npoints)
    expect_equal(ncol(llin$alphai), 3)
    expect_equal(nrow(irs$alphai),  npoints)

    # Protected-host column (col 1) should always be <= unprotected (col 2)
    expect_true(all(llin$alphai[, 1] <= llin$alphai[, 2] + 1e-10))
    expect_true(all(llin$PBi[,  1] <= llin$PBi[,  2] + 1e-10))
    expect_true(all(llin$PCi[,  1] <= llin$PCi[,  2] + 1e-10))

    # Survival (attrition) should be 1 at t=0 and non-increasing
    expect_equal(llin$survival[1], 1)
    expect_true(all(diff(llin$survival) <= 1e-10))

    # Over a longer period: attrition is the share of people still protected
    # (survival), not a change of the effects, which stay at their initial
    # values without insecticide decay (insecticide decay: test_decay.R)
    irs_long = suppressMessages(build_intervention(
        efficacy = c(Deterrency = 0.53, PrePrandial = 0.56, PostPrandial = 0.24),
        halflife_attrition = 0.5 * 365, shape_attrition = 1, duration_years = 1,
        model_p = model_params, decay_attrition = "weibull"))[[1]]$effects
    s = decay_function(L = 0.5 * 365, kappa = 1, datej = 0:364, decay = "weibull")
    expect_equal(irs_long$survival, s, tolerance = 1e-12)
    expect_equal(irs_long$alphai[, 1], irs_long$alphai[, 2] * (1 - 0.53), tolerance = 1e-12)
    expect_equal(irs_long$PBi[, 1],    irs_long$PBi[, 2]    * (1 - 0.56), tolerance = 1e-12)
    expect_equal(irs_long$PCi[, 1],    irs_long$PCi[, 2]    * (1 - 0.24), tolerance = 1e-12)
})


test_that("build_intervention output matches supplied efficacy inputs exactly at t=0", {
    # Replaces old test that checked def_interventions_effects() stored custom
    # matrix values verbatim. At t=0: survival=1, no insecticide decay, so the
    # ratio protected/unprotected equals (1 - efficacy) exactly.
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"), def_host_params())
    det = 0.45; pre = 0.53; post = 0.34
    interv  = build_intervention(
        efficacy = c(Deterrency = det, PrePrandial = pre, PostPrandial = post),
        halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
        duration_years = 3 / 365, model_p = model_p, name = "custom"
    )[[1]]$effects

    # Exact efficacy ratios at t=0
    expect_equal(interv$alphai[1, 1] / interv$alphai[1, 2], 1 - det,  tolerance = 1e-10)
    expect_equal(interv$PBi  [1, 1] / interv$PBi  [1, 2], 1 - pre,  tolerance = 1e-10)
    expect_equal(interv$PCi  [1, 1] / interv$PCi  [1, 2], 1 - post, tolerance = 1e-10)

    # Unprotected humans and animals unchanged (col 2 = col 3 scaled by their baseline rates)
    expect_equal(interv$PDi[, 2], interv$PDi[, 3], tolerance = 1e-10)  # PDi not modified
    expect_equal(interv$PEi[, 2], interv$PEi[, 3], tolerance = 1e-10)  # PEi not modified
})

test_that("build_intervention errors on out-of-range probability efficacy values", {
    # Replaces old test that expected def_interventions_effects() to error when
    # alphai > 1 was passed. Deterrency/PrePrandial/PostPrandial are probabilities
    # and must be in [0,1]; KillingDuringHostSeekingAbsolute is an absolute rate (>= 0, can exceed 1).
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"), def_host_params())

    # Probability components outside [0,1] must error
    expect_error(
        build_intervention(
            efficacy = c(Deterrency = 1.5, PrePrandial = 0.5, PostPrandial = 0.3),
            halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
            duration_years = 1 / 365, model_p = model_p
        ),
        regexp = "\\[0, 1\\]"
    )
    expect_error(
        build_intervention(
            efficacy = c(Deterrency = -0.1, PrePrandial = 0.5, PostPrandial = 0.3),
            halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
            duration_years = 1 / 365, model_p = model_p
        ),
        regexp = "\\[0, 1\\]"
    )
    # KillingDuringHostSeekingAbsolute > 1 is valid (absolute rate, not a probability)
    expect_no_error(
        build_intervention(
            efficacy = c(Deterrency = 0.3, PostPrandial = 0.4, KillingDuringHostSeekingAbsolute = 3.58),
            halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
            duration_years = 1 / 365, model_p = model_p
        )
    )
})

test_that("build_intervention errors on unrecognised efficacy component names", {
    # Replaces old test that checked wrong-dimension matrix inputs to
    # def_interventions_effects(). With the current API, dimensions are computed
    # internally; the analogous validation is rejecting unknown component names.
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"), def_host_params())
    expect_error(
        build_intervention(
            efficacy = c(Typo = 0.5, PostPrandial = 0.3),
            halflife_attrition = 180, shape_attrition = 2, decay_attrition = "weibull",
            duration_years = 1 / 365, model_p = model_p
        ),
        "unrecognised efficacy component\\(s\\): Typo"
    )
})

test_that("KillingDuringHostSeekingAbsolute is used directly as the per-day dummy availability (no td)", {
    # Provisional convention: a hut-night is treated as a day (see CLAUDE.md)
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"), def_host_params())
    interv = suppressMessages(build_intervention(
        efficacy = c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.61, PostPrandial = 0.4),
        halflife_attrition = 180, shape_attrition = 2,
        duration_years = 1 / 365, model_p = model_p))
    expect_equal(interv[[1]]$effects$alphai[1, 4], 0.61)
})

test_that("standard and dummy-host models are mutually exclusive", {
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"), def_host_params())
    bi = function(efficacy, ...) suppressMessages(build_intervention(
        efficacy = efficacy, halflife_attrition = 180, shape_attrition = 2,
        duration_years = 1 / 365, model_p = model_p, ...))

    expect_error(bi(c(Deterrency = 0.3, PostPrandial = 0.4), mode = "dummy"),
                 "mode must be")
    expect_error(bi(c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 2), mode = "standard"),
                 "requires mode")
    expect_error(bi(c(Deterrency = 0.3, PrePrandial = 0.2, KillingDuringHostSeekingAbsolute = 2)),
                 "mutually exclusive")
    # PrePrandial = 0 alongside KillingDuringHostSeekingAbsolute is accepted
    expect_no_error(bi(c(Deterrency = 0.3, PrePrandial = 0, KillingDuringHostSeekingAbsolute = 2)))

    ps = data.frame(name = "x", deterrency = 0.3, preprandial = 0.2,
                    postprandial = 0.4, killing_during_host_seeking_absolute = 2,
                    halflife_attrition = 180, shape_attrition = 2,
                    decay_attrition = "weibull", stringsAsFactors = FALSE)
    expect_error(suppressMessages(run_interventions(ps, model_p, c(0, 1),
                                                    duration_years = 1 / 365)),
                 "mutually exclusive")
})

test_that("former and relative host-seeking killing names give an error naming the absolute one", {
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"), def_host_params())
    bi = function(efficacy, ...) suppressMessages(build_intervention(
        efficacy = efficacy, halflife_attrition = 180, shape_attrition = 2,
        duration_years = 1 / 365, model_p = model_p, mode = "dummy_host", ...))
    for (nm in c("HostSeekingKilling", "KillingDuringHostSeeking",
                 "KillingDuringHostSeekingRelative")) {
        expect_error(bi(stats::setNames(c(0.3, 0.6, 0.4),
                                        c("Deterrency", nm, "PostPrandial"))),
                     paste0("efficacy: ", nm, " not accepted.*KillingDuringHostSeekingAbsolute"))
        # also as a decay name
        expect_error(bi(c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.6,
                          PostPrandial = 0.4),
                        halflife_insecticide = stats::setNames(2, nm)),
                     paste0("halflife_insecticide: ", nm, " not accepted"))
    }
    # A batch column with the former name would otherwise run the standard model
    for (nm in c("host_seeking_killing", "killing_during_host_seeking_relative")) {
        ps = data.frame(name = "x", deterrency = 0.3, preprandial = 0,
                        postprandial = 0.4, halflife_attrition = 180,
                        shape_attrition = 2, decay_attrition = "weibull",
                        stringsAsFactors = FALSE)
        ps[[nm]] = 0.6
        expect_error(suppressMessages(run_interventions(ps, model_p, c(0, 1),
                                                        duration_years = 1 / 365)),
                     paste0("param_sets: ", nm, " not accepted.*",
                            "killing_during_host_seeking_absolute"))
    }
})

test_that("removed deprecated arguments are no longer accepted", {
    model_p = build_model_obj(def_vector_params("Anopheles gambiae ss/coluzzii"), def_host_params())
    expect_error(build_intervention(efficacy = c(Deterrency = 0.3), L = 180,
                                    model_p = model_p), "unused argument")
    expect_error(build_intervention(efficacy = c(Deterrency = 0.3), npoints = 10,
                                    halflife_attrition = 180, shape_attrition = 2,
                                    model_p = model_p), "unused argument")
})

