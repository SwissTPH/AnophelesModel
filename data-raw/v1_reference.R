# Reference results of AnophelesModel v1 on inputs identical to v2's, for
# tests/testthat/test_v1_reference.R (which runs without v1 installed).
#
# Run once, from the package root, with both packages installed:
#   remotes::install_github("SwissTPH/AnophelesModel")   # v1
#   source("data-raw/v1_reference.R")
#
# Identical inputs, so that differences come from the calculation only:
# - the same custom vector parameters (whole-day tau, ts = 10), not the
#   bundled databases (the bionomics differ on purpose between versions);
# - the same host parameters: v1's database values for the species, passed
#   to v2 as a custom host table (v1's own custom host table check refers to
#   a missing column);
# - unit populations: v1 with total_pop = 2 (one human, one animal), as v2;
# - the same interventions: built with v2, their daily effect matrices given
#   to v1's calculate_impact() as a custom intervention (used as given, no
#   exposure adjustment), exposure 1;
# - standard model only (v1 has no dummy host).

stopifnot(requireNamespace("AnophelesModel"), requireNamespace("AnophelesModel2"))

species = "Anopheles gambiae"
vec_table = data.frame(species_name = species, M = 0.6, Chi = 0.9, A0 = 0.6,
                       zeta.3 = 1, td = 0.33, tau = 3, ts = 10,
                       endophily = 0.8, endophagy = 0.85, stringsAsFactors = FALSE)

# v1 model
vec1   = AnophelesModel::def_vector_params(mosquito_species = species,
                                           vector_table = vec_table, verbose = FALSE)
host1  = AnophelesModel::def_host_params(mosquito_species = species, vec_params = vec1,
                                         verbose = FALSE)
model1 = AnophelesModel::build_model_obj(
    vec1, host1, AnophelesModel::def_activity_patterns("default_Anopheles_gambiae"),
    total_pop = 2)

# The same host values for v2 (columns v2 knows)
host_table = as.data.frame(host1[intersect(names(host1),
                                           colnames(AnophelesModel2::host_ent_param))],
                           stringsAsFactors = FALSE)
model2 = AnophelesModel2::build_model_obj(
    AnophelesModel2::def_vector_params(vector_table = vec_table, verbose = FALSE),
    AnophelesModel2::def_host_params(host_table, verbose = FALSE))

# Interventions (arguments of build_intervention(), without model_p)
cases = list(
    constant = list(efficacy = c(Deterrency = 0.3, PrePrandial = 0.4, PostPrandial = 0.2),
                    decay_attrition = "none", insecticide_decay = FALSE,
                    duration_years = 30 / 365),
    attrition = list(efficacy = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
                     decay_attrition = "weibull", halflife_attrition = 180,
                     shape_attrition = 2, insecticide_decay = FALSE,
                     duration_years = 1),
    insecticide = list(efficacy = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3),
                       decay_attrition = "weibull", halflife_attrition = 2 * 365,
                       shape_attrition = 2,
                       halflife_insecticide = c(Deterrency = 3, PrePrandial = 4,
                                                PostPrandial = 2),
                       duration_years = 1),
    strong = list(efficacy = c(Deterrency = 0.8, PrePrandial = 0.9, PostPrandial = 0.7),
                  decay_attrition = "step", halflife_attrition = 200,
                  insecticide_decay = FALSE, duration_years = 1))
cov = c(0, 0.2, 0.5, 0.8, 1)   # v1 takes the first coverage as the baseline

v1_impact = list(); v1_vc = list(); v2_impact = list()
for (case in names(cases)) {
    iv = suppressMessages(do.call(AnophelesModel2::build_intervention,
                                  c(cases[[case]], list(model_p = model2))))[[1]]
    custom = list(id = "custom", description = case, parameterisation = "custom",
                  effects = iv$effects[c("alphai", "PBi", "PCi", "PDi", "PEi", "Kvi",
                                         "survival")])
    res = AnophelesModel::calculate_impact(list(custom), cov, model1, Nv0 = 1,
                                           num_ip_points = iv$duration)
    v1_impact[[case]] = unname(res$interventions_vec[[1]]$effects$avg_impact)
    v1_vc[[case]]     = unname(res$interventions_vec[[1]]$effects$avg_vc)
    v2_impact[[case]] = AnophelesModel2::calculate_impact(list(iv), cov, model2)$avg_impact
}

# A first look (the test checks the same)
print(rbind(muvA_v1 = model1$host_params$muvA, muvA_v2 = model2$host_params$muvA))
for (case in names(cases))
    print(rbind(coverage = cov, v1 = v1_impact[[case]], v2 = v2_impact[[case]]))

dir.create("tests/testthat/fixtures", showWarnings = FALSE, recursive = TRUE)
saveRDS(list(vec_table = vec_table, host_table = host_table, cov = cov, cases = cases,
             v1 = list(version = as.character(utils::packageVersion("AnophelesModel")),
                       muvA = model1$host_params$muvA,
                       alphai = model1$host_params$alphai,
                       impact = v1_impact, vc = v1_vc)),
        "tests/testthat/fixtures/v1_reference.rds")
message("Saved tests/testthat/fixtures/v1_reference.rds")
