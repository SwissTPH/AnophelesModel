library(AnophelesModel)
library(testthat)
########################################
# Tests for build_model_obj()
########################################
test_that("test that a model object is correctly built", {
    # Create the necessary objects
    species_name = "Anopheles Gambiae test"
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
    activity = def_activity_patterns()
    hosts_params = def_host_params()
    total_pop = 2000

    # Build the model object
    model_params = build_model_obj(vec_p = vec_params, hosts_p = hosts_params,
                                   activity = activity, total_pop = total_pop)

    sol_obj = NULL
    sol_obj$vec_params = vec_params
    sol_obj$host_params = hosts_params
    sol_obj$host_params$muvA = 0.2362186
    sol_obj$host_params$alphai = c(0.0008464341, 0.00005498667)
    sol_obj$activity = activity
    sol_obj$total_pop = total_pop

    expect_equal(model_params, sol_obj, tolerance = 0.0001)
})

#
# test_that("same impact values for known custom parameter model (Albimanus)", {
#     # checks if the package reproduces the impact values from a known model application
#
#
#     LLINs_albimanus = list(id = "LLINs", description = "LLINs",
#                            parameterisation = "LLINs04",
#                            type = "Default",
#                            insecticide_decay = "lambdacyhalothrin (albimanus)",
#                            country = "Kenya")
#
#     interventions_list = list(LLINs_albimanus)
#
#     ##### With custom parameters
#     # Define vector object (with cuastom parameters for Albimanus)
#     custom_params = as.data.frame(cbind.data.frame(species_name = "Anopheles albimanus", M = 0.4914393, Chi = 0.224185,
#                                                    A0 = 0.405, zeta.3 = 1, td = 0.33,
#                                                    tau = 2.044137, ts = 10, to = 5, endophily = 0.3406517, endophagy = 0.2727916))
#     vec_obj = def_vector_params(mosquito_species = "Anopheles albimanus", vector_table = custom_params)
#
#     # Define host object
#     host_obj = def_host_params()
#
#     # Define activity patterns
#     activity=NULL
#     activity$HBI<- c(NA,21,21,21,16,11,11,8.36,5.68,3,2.25,1.5,1.5)/100
#     activity$HBO<- c(NA,32,32,32,27,22,22,18.04,14.02,10,6.5,3,3)/100
#     activity$humans_in_bed = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964, 83.202297, 92.736838, 96.734455, 96.637604,
#                                92.983136, 85.264515, 73.021653, 57.079495)/100
#     activity$humans_indoors = c(5.628637, 14.084496, 28.558507, 47.761203, 67.502964, 83.202297, 92.736838, 96.734455, 96.637604,
#                                 92.983136, 85.264515, 73.021653, 57.079495)/100
#     activity_obj = def_activity_patterns(activity)
#
#     # Initialize entomological model
#     model_obj = build_model_obj(vec_obj, host_obj, activity_obj, 2000)
#
#     # Define intervention object
#     my_num_ip_points = 3
#     intervention_obj = def_interventions_effects(interventions_list, model_obj, num_ip_points = my_num_ip_points)
#
#     # Calculate intervention impact
#     impacts = calculate_impact(interventions=intervention_obj,
#                                coverage_vec = c(seq(0,0.9, by = 0.1), 0.95, 0.98, 0.99),
#                                model_p=model_obj, Nv0= 10000, num_points = my_num_ip_points)
#
#     # old value to be compared to
#     old_impacts=matrix(c(0.00000000, 0.07689926, 0.14852081, 0.21521174, 0.27729574, 0.33507480, 0.38883076, 0.43882676, 0.48530853, 0.52850561, 0.54893999, 0.56084294, 0.56475217,
#                          0.00000000, 0.05777946, 0.11264684, 0.16473941, 0.21418784, 0.26111666, 0.30564444, 0.34788420, 0.38794361, 0.42592531, 0.44416777, 0.45488060, 0.45841334,
#                          0.00000000, 0.03959029, 0.07782617, 0.11475057, 0.15040505, 0.18482986, 0.21806396, 0.25014507, 0.28110972, 0.31099329, 0.32554041, 0.33414482, 0.33699251),
#                        ncol = 13, nrow=3, byrow=T)
#
#     old_vc=matrix(c(0.128488, 0.1186074, 0.1094049, 0.1008359, 0.09285885, 0.08543494, 0.07852794, 0.07210405, 0.06613170, 0.06058139, 0.05795582, 0.05642643, 0.05592414,
#                     0.128488, 0.1210641, 0.1140143, 0.1073210, 0.10096746, 0.09493767, 0.08921638, 0.08378908, 0.07864193, 0.07376173, 0.07141779, 0.07004132, 0.06958741,
#                     0.128488, 0.1234012, 0.1184883, 0.1137440, 0.10916279, 0.10473961, 0.10046943, 0.09634739, 0.09236880, 0.08852912, 0.08665999, 0.08555443, 0.08518853),
#                   ncol = 13, nrow=3, byrow=T)
#
#
#     # testing
#     expect_equal(round(impacts$interventions_vec[[1]]$effects$impact, digits = 6), round(old_impacts, digits = 6))
#     expect_equal(round(impacts$interventions_vec[[1]]$effects$vc, digits = 6), round(old_vc, digits = 6))
# })
#
# # This test checks the calculation of intervention impact with custom values for
# # the intervention effects.
# test_that("test: check calculating impact with a custom intervention object", {
#     coverages = c(seq(0,0.9, by = 0.1), 0.95, 0.99)
#     # Use random parameter values to define the intervention effects
#     Kvi=c(0.021, 0.039, 0.004)
#     alphai = matrix(data = c(0.00031, 0.00043, 0.00075,
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
#     # Define the list of interventions containig the new, custom intervention
#     custom_intervention = list("custom_interv" = list("id" = "new_intervention",
#                                                       "description" = "new intervention parameterisation",
#                                                       parameterisation = "custom parameterisation",
#                                                       "effects" = effects_list))
#
#     # Use default values for other parameters and build a model object
#     my_default_model= build_model_obj(vec_p = def_vector_params(),
#                                       hosts_p = def_host_params(),
#                                       activity = def_activity_patterns(),
#                                       total_pop = 2000 )
#
#     # Incorporate the intervention effects
#     my_intervention_obj = def_interventions_effects(
#                                         intervention_list = custom_intervention,
#                                         model_p = my_default_model,
#                                         num_ip_points = 3)
#
#     # Calculate intervention impact using the custom intervention effects
#     my_impacts = calculate_impact(interventions_vec = my_intervention_obj,
#                                   coverage_vec = coverages,
#                                   model_p = my_default_model,
#                                   Nv0 = 2000, num_points = 3)
#
#     # TO DO: use the package database to generate "custom" intervention effects
#     # (adjusted for location, etc.) and check whether the obtained impact
#     # using the custom effects matches the one obtained with the package
#     # database in the first place. To modify the test accordingly
#     c_sol = custom_intervention$custom_interv$effects
#     expect_equal(c_sol, my.intervention_obj$custom_interv$effects)
# })
