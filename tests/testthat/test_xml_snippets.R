# library(AnophelesModel)
# library(testthat)
#
# test_that("correct ento snippet for OM", {
#     # checks if the package reproduces the impact values from a known model application
#
#     ##### With custom parameters
#     # Define vector object (with custom parameters for Albimanus)
#     custom_params = as.data.frame(cbind.data.frame(species_name = "Anopheles albimanus", M = 0.4914393, Chi = 0.224185,
#                                                    A0 = 0.405, zeta.3 = 1, td = 0.33,
#                                                    tau = 2.044137, ts = 10, to = 5, endophily = 0.3406517, endophagy = 0.2727916))
#     vec_obj = def_vector_params(mosquito_species = "Anopheles albimanus", vector_table = custom_params)
#
#     # Define host object
#     species_name = "Anopheles albimanus" # made-up species
#     host = c("human", "animal") # made-up species
#     PBi = c(0.67,0.44)
#     PCi = c(0.33,0.434)
#     PDi = c(0.647,0.344)
#     PEi = c(0.672,0.214)
#     Kvi = c(0.52,0.314)
#
#     custom_params = cbind.data.frame(species_name, host,
#                                      PBi, PCi, PDi, PEi, Kvi)
#     host_obj = def_host_params(mosquito_species = species_name,
#                                   host_table = custom_params[,-1])
#
#     my.ento.snippet = get_OM_ento_snippet(vec_params=vec_obj,
#                                           hosts_params=host_obj, xml_file="")
#
#     old.ento.snippet=c("<mosq minInfectedThreshold=\"0.01\">",
#     "        <mosqRestDuration value=\"2\"/>",
#     "        <extrinsicIncubationPeriod value=\"10\"/>",
#     "        <mosqLaidEggsSameDayProportion value=\"0.405\"/>",
#     "        <mosqSeekingDuration value=\"2.044137\"/>",
#     "        <mosqSurvivalFeedingCycleProbability value=\"0.4914393\"/>",
#     "        <availabilityVariance value=\"0\"/>",
#     "        <mosqProbBiting mean=\"0.67\" variance=\"0\"/>",
#     "        <mosqProbFindRestSite mean=\"0.33\" variance=\"0\"/>",
#     "        <mosqProbResting mean=\"0.647\" variance=\"0\"/>",
#     "        <mosqProbOvipositing value=\"0.672\"/>",
#     "        <mosqHumanBloodIndex value=\"0.224185\"/>",
#     "        </mosq> ",
#     "",
#     "        <nonHumanHosts name=\"unprotectedAnimals\" />",
#     "        <mosqRelativeEntoAvailability value=\"1\"/>",
#     "        <mosqProbBiting value=\"0.44\"/>",
#     "        <mosqProbFindRestSite value=\"0.434\"/>",
#     "        <mosqProbResting value=\"0.344\"/>",
#     "        </nonHumanHosts>\"")
#
#     # testing
#     expect_equal(my.ento.snippet, old.ento.snippet)
# })
#
#
#
# test_that("correct GVI snippet for OM", {
#     # checks if the package reproduces the impact values from a known model application
#
#     ##### With custom parameters
#     # Define vector object (with cuastom parameters for Albimanus)
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
#     intervention_obj = def_interventions_effects(model_p=model_obj, num_ip_points = my_num_ip_points)
#
#     impacts = calculate_impact(interventions=intervention_obj,
#                                coverage_vec = c(seq(0,0.9, by = 0.1), 0.95, 0.98, 0.99),
#                                model_p=model_obj, Nv0= 10000, num_points = my_num_ip_points)
#
#     my.GVI.snippet = get_OM_GVI_snippet(species = "Anopheles albimanus",
#                         intervention_impact = impacts$interventions_vec[[1]],
#                         GVI_file="truc.txt")
#
#     old.GIV.snippet=c()
#
#     # testing
#     expect_equal(my.GVI.snippet, old.GIV.snippet)
# })
#
#
