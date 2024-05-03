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
                                   vector_table = custom_params))
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
                                   vector_table = custom_params))
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

    my_host_obj = def_host_params(mosquito_species = species_name,
                                  host_table = custom_params)

    c_sol = as.list(custom_params)

    expect_equal(c_sol, my_host_obj)
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

    expect_error(def_host_params(mosquito_species = species_name,
                                 host_table = custom_params))
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

    expect_error(def_host_params(mosquito_species = species_name,
                                 host_table = custom_params))
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

    expect_error(def_host_params(mosquito_species = species_name,
                                 host_table = custom_params))
})

# This test checks whether custom values in a different format cannot be
# provided for the host entomological parameters.
test_that("test: check loading different format for host parameter values", {

    species_name = "Anopheles Anonymus"
    PBi = c(0.67, 0.44)
    PCi = c(0.33, 0.434)
    PDi = c(0.647, -0.344)
    PEi = c(0.672, 0.214)
    Kvi = c(0.52, 0.314)

    # Define data frame with custom values
    custom_params = cbind.data.frame(species_name,
                                     PBi, PCi, PDi, PEi, Kvi)

    expect_error(def_host_params(mosquito_species = species_name,
                                 host_table = custom_params))
})

# This test checks whether custom values cannot be
# provided for the host entomological parameters without specifying the human
# and animal values
test_that("test: check loading non-numeric host parameter values", {

    species_name = "Anopheles Anonymus"
    host = c("horse", "sheep")
    PBi = c(0.67, 0.44)
    PCi = c(0.33, 0.434)
    PDi = c(0.647, -0.344)
    PEi = c(0.672, 0.214)
    Kvi = c(0.52, 0.314)

    # Define data frame with custom values
    custom_params = cbind.data.frame(species_name, host,
                                     PBi, PCi, PDi, PEi, Kvi)

    expect_error(def_host_params(mosquito_species = species_name,
                                 host_table = custom_params))
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
    expect_error(def_activity_patterns(custom_params))
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
    expect_error(def_activity_patterns(custom_params))
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
    expect_error(def_activity_patterns(custom_params))
})

#######################################
# Tests for def_intervention_efects()
#######################################
test_that("test: check defining intervention effects", {
    # Set the vector parameters
    vec_params = def_vector_params()
    # Set the activity patterns
    activity = def_activity_patterns()
    # Set the host parameters
    hosts_params = def_host_params()
    # Initialize entomological model-specific parameters
    model_params = build_model_obj(vec_p = vec_params, hosts_p = hosts_params,
                                   activity = activity, total_pop = 2000)

    # Define intervention effects on the transition probabilities between
    # consecutive stages of the mosquito oviposition cycle
    intervention_vec = def_interventions_effects(intervention_obj_examples,
                                                 model_params,
                                                 5)

    decay_obj = list(
        LLINs_alphai_decay =
                            intervention_vec$LLINs_example$effects$alphai_decay,
        LLINs_PBi_decay = intervention_vec$LLINs_example$effects$PBi_decay,
        LLINs_PCi_decay = intervention_vec$LLINs_example$effects$PCi_decay,
        IRS_alphai_decay =
                as.vector(intervention_vec$IRS_example$effects$alphai_decay),
        IRS_PBi_decay =
                as.vector(intervention_vec$IRS_example$effects$PBi_decay),
        IRS_PCi_decay =
                as.vector(intervention_vec$IRS_example$effects$PCi_decay))

    sol_obj = list(LLINs_alphai_decay = c(0.6893384, 0.6377113, 0.6014325,
                                          0.5703420, 0.5421073),
                   LLINs_PBi_decay = c(0.8862125, 0.7536105, 0.5683662,
                                       0.3880709, 0.2494714),
                   LLINs_PCi_decay = c(0.5213965, 0.3867372, 0.2872797,
                                       0.2103163, 0.1519516),
                   IRS_alphai_decay = c(0.3689327, 0.5395866, 0.6271549,
                                        0.5924592, 0.3963210),
                   IRS_PBi_decay = c(0.11597726,  0.11825639,  0.07937855,
                                     0.02933801, -0.00187099),
                   IRS_PCi_decay = c(0.7594490, 0.6589532, 0.4756708,
                                     0.2714698, 0.1082178))

    expect_equal(sol_obj, decay_obj, tolerance = 0.0001)
})


# This test checks whether custom values can be
# provided for the intervention object
# test_that("test: check loading custom intervention object", {
#     # create custom parameter values
#     Kvi=c(0.021, 0.039, 0.004)
#     alphai = matrix(data = c(0.00031, 0.00043, 0.00075,
#                            0.00028, 0.00053, 0.00043,
#                            0.00045, 0.00076, 0.00032), ncol = 3)
#
#     PBi = matrix(data = c(0.23, 0.88, 0.99,
#                         0.55, 0.65, 0.67,
#                         0.53, 0.43, 0.698), ncol = 3)
#
#     PCi = matrix(data = c(0.32, 0.21, 0.12,
#                         0.321, 0.432, 0.33,
#                         0.55, 0.54, 0.42), ncol = 3)
#
#     PDi = matrix(data = c(0.822, 0.621, 0.712,
#                         0.921, 0.732, 0.933,
#                         0.355, 0.654, 0.942), ncol = 3)
#
#     PEi = matrix(data = c(0.82, 0.63, 0.742,
#                         0.521, 0.52, 0.93,
#                         0.85, 0.54, 0.94), ncol=3)
#
#     survival = c(0.54, 0.67, 0.99)
#
#     effects_list = list("Kvi"=Kvi, "alphai" = alphai, "PBi" = PBi, "PCi" = PCi,
#                         "PDi" = PDi, "PEi" = PEi, "survival" = survival)
#
#     custom_example = list(custom_interv = list("id" = "new_intervention",
#                             "description" = "new intervention parameterisation",
#                             parameterisation = "custom parameterisation",
#                             "effects" = effects_list))
#
#     # use default values for other parameters
#     my_default_model= build_model_obj(vec_p = def_vector_params(),
#                                         hosts_p = def_host_params(),
#                                         activity = def_activity_patterns(),
#                                         total_pop = 2000)
#
#     my_intervention_obj = def_interventions_effects(
#                                 intervention_list = custom_example,
#                                 model_p = my_default_model, num_ip_points = 3)
#
#     c_sol = custom_example$custom_interv$effects
#     expect_equal(c_sol, my_intervention_obj$custom_interv$effects)
# })

# This test checks whether non-probability custom values cannot be
# provided for the intervention object
# test_that("test: check loading non-probability custom intervention object", {
#     # create custom parameter values
#     Kvi=c(0.021, 0.039, 0.004)
#     alphai = matrix(data = c(0.00031, 2.00043, 0.00075,
#                              0.00028, 0.00053, 0.00043,
#                              0.00045, 0.00076, 0.00032), ncol = 3)
#
#     PBi = matrix(data = c(0.23, 0.88, 0.99,
#                           0.55, 0.65, 0.67,
#                           0.53, 0.43, 0.698), ncol = 3)
#
#     PCi = matrix(data = c(0.32, 0.21, 0.12,
#                           0.321, 0.432, 0.33,
#                           0.55, 0.54, 0.42), ncol = 3)
#
#     PDi = matrix(data = c(0.822, 0.621, 0.712,
#                           0.921, 0.732, 0.933,
#                           0.355, 0.654, 0.942), ncol = 3)
#
#     PEi = matrix(data = c(0.82, 0.63, 0.742,
#                           0.521, 0.52, 0.93,
#                           0.85, 0.54, 0.94), ncol=3)
#
#     survival = c(0.54, 0.67, 0.99)
#
#     effects_list = list("Kvi"=Kvi, "alphai" = alphai, "PBi" = PBi, "PCi" = PCi,
#                         "PDi" = PDi, "PEi" = PEi, "survival" = survival)
#
#     custom_example = list(custom_interv = list("id" = "new_intervention",
#                                                "description" = "new intervention parameterisation",
#                                                parameterisation = "custom parameterisation",
#                                                "effects" = effects_list))
#
#     # use default values for other parameters
#     my_default_model= build_model_obj(vec_p = def_vector_params(),
#                                       hosts_p = def_host_params(),
#                                       activity = def_activity_patterns(),
#                                       total_pop = 2000)
#
#     expect_error(def_interventions_effects(
#         intervention_list = custom_example,
#         model_p = my_default_model, num_ip_points = 3))
# })

# This test checks whether custom values with different dimensions cannot be
# provided for the intervention object
# test_that("test: check loading custom effects with wrong dimension", {
#     # create custom parameter values
#     Kvi=c(0.021, 0.039, 0.004)
#     alphai = matrix(data = c(0.00031, 0.00043, 0.00075,
#                              0.00028, 0.00053, 0.00043,
#                              0.00045, 0.00076, 0.00032, 0.00031), ncol = 2)
#
#     PBi = matrix(data = c(0.23, 0.88, 0.99,
#                           0.55, 0.65, 0.67,
#                           0.53, 0.43, 0.698), ncol = 3)
#
#     PCi = matrix(data = c(0.32, 0.21, 0.12,
#                           0.321, 0.432, 0.33,
#                           0.55, 0.54, 0.42), ncol = 3)
#
#     PDi = matrix(data = c(0.822, 0.621, 0.712,
#                           0.921, 0.732, 0.933,
#                           0.355, 0.654, 0.942), ncol = 3)
#
#     PEi = matrix(data = c(0.82, 0.63, 0.742,
#                           0.521, 0.52, 0.93,
#                           0.85, 0.54, 0.94), ncol=3)
#
#     survival = c(0.54, 0.67, 0.99)
#
#     effects_list = list("Kvi"=Kvi, "alphai" = alphai, "PBi" = PBi, "PCi" = PCi,
#                         "PDi" = PDi, "PEi" = PEi, "survival" = survival)
#
#     custom_example = list(custom_interv = list("id" = "new_intervention",
#                                                "description" = "new intervention parameterisation",
#                                                parameterisation = "custom parameterisation",
#                                                "effects" = effects_list))
#
#     # use default values for other parameters
#     my_default_model= build_model_obj(vec_p = def_vector_params(),
#                                       hosts_p = def_host_params(),
#                                       activity = def_activity_patterns(),
#                                       total_pop = 2000)
#
#     expect_error(def_interventions_effects(
#         intervention_list = custom_example,
#         model_p = my_default_model, num_ip_points = 3))
# })

