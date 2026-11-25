################################################################################
# AnophelesModel -> OpenMalaria: a short demo
#
#   1. mosquito populations          get_OM_ento_snippet()
#   2. a net                          om_vector_control()
#   3. campaigns                      om_campaigns()
#   4. the XML                        get_OM_xml()          (no OMU needed)
#   5. alternative nets + scenarios   om_scenario_values()
#   6. same with openMalariaUtilities define_OM_entomology() / _vector_control()
################################################################################

library(AnophelesModel)


# 1. MOSQUITO POPULATIONS ------------------------------------------------------
# Two species; their share of the EIR = abundance x sporozoite rate.
# Activity patterns split each species into populations reached by nets
# (in bed), by IRS (resting indoors), or by neither (outdoor).

mix = eir_shares(abundance       = c("Anopheles gambiae ss/coluzzii" = 0.7, "Anopheles funestus" = 0.3),
                 sporozoite_rate = c("Anopheles gambiae ss/coluzzii" = 0.114, "Anopheles funestus" = 0.235))
mix


act_gamb = def_activity_patterns("default_Anopheles_gambiae")
act_fun  = def_activity_patterns(list(HBI = 64, HBO = 195,
                                      humans_indoors = 4, humans_in_bed = 24))

#list_activity("Anopheles funestus", sampling = "HBI")   # find database IDs
#list_activity("Anopheles funestus", sampling = "HBO")


om_ento = get_OM_ento_snippet(names(mix),                   # gambiae first, then funestus
                              activity = list(act_gamb, act_fun),
                              species_shares = mix)

# 2. A NET ---------------------------------------------------------------------
# Efficacies from the experimental hut trial database (Champagne et al. 2025),
# 2-year attrition. No model, no duration: the export builds it.

list_ITN_params()

ig2 = om_vector_control(get_ITN_params("Interceptor G2", "Kibondo", verbose = FALSE, mode="standard"),
                        halflife_attrition = 2 * 365, shape_attrition = 2)


# 3. CAMPAIGNS -----------------------------------------------------------------

schedule = om_campaigns(dates = c("2021-01-01", "2024-01-01"), coverage = c(0.8, 0.6))
schedule


# 4. THE XML (plain text, to paste into a scenario) ----------------------------

xml = get_OM_xml(om_ento, ig2, schedule, name = "IG2")
xml                     # each snippet with where it goes
cat(xml$entomology)     # mosquito populations (<vector>)
cat(xml$define)         # deterrency, pre- and post-prandial killing as a GVI
cat(xml$deploy)


# 5. ALTERNATIVE NETS ACROSS SCENARIOS -----------------------------------------
# One intervention "ITN", the net chosen per scenario. Placeholders only where
# something varies (efficacies, decays, coverages); the package fills them.

custom = om_vector_control(c(Deterrency = 0.4, PrePrandial = 0.5, PostPrandial = 0.3),
                           halflife_attrition = 2 * 365, shape_attrition = 2)
nets = list(IG2 = ig2, custom = custom)

schedule_ph = om_campaigns("2021-01-01", "2024-01-01", every = "3 years",
                           coverage = "@cov@", by_year = TRUE)    # @cov2021@, @cov2024@

xml_ph = get_OM_xml(om_ento, nets, schedule_ph, name = "ITN",
                    decay = "placeholder", entomology = FALSE)
cat(xml_ph$define)

# Your scenario table; om_scenario_values() adds the placeholder values
scens = expand.grid(net = c("IG2", "custom"), attrition = c(2, 3),   # attrition in years
                    cov2021 = c(0.5, 0.8), cov2024 = 0.6)
scens = om_scenario_values(scens, xml_ph, net = "net", halflife = "attrition")
head(scens)


# 6. SAME WITH openMalariaUtilities (writes the whole scenario) ----------------

library(openMalariaUtilities)
library(omucompat)

baseList = list(expName = "Demo_AM2", OMVersion = 44L,
                demography = list(), monitoring = list(), interventions = list(),
                healthSystem = list(), entomology = list(), model = list())
baseList = write_demography_compat(baseList, pop = 1000L, country = "TZA")
baseList = write_monitoring_compat(baseList, y1 = 2020, y2 = 2026, detect = 100,
                                   SIMSTART = "1918-01-01", upperbounds = c(5, 100))

# Entomology: total EIR and seasonality as placeholders (@EIR@, @m1@..@m12@)
baseList = define_OM_entomology(baseList, om_ento, scaledAnnualEIR = "@EIR@")

# Nets: define, then deploy with a schedule
baseList = define_OM_vector_control(baseList, nets, name = "ITN", decay = "placeholder")
baseList = deploy_OM_vector_control(baseList, "ITN", schedule_ph)

baseList = write_healthsys_compat(baseList, access = 0.3)
baseList = write_end_compat(baseList, seed = "@seed@", modelname = "base")

setupDirs(experimentName = "Demo_AM2")
createBaseXml(baseList, replace = TRUE)

# Scenarios: EIR, seasonality, seed + net values from the baseList
scens = expand.grid(net = c("IG2", "custom"), attrition = c(2, 3),
                    cov2021 = c(0.5, 0.8), cov2024 = 0.6, EIR = c(10, 50), seed = 1)
season = c(2, 3, 5, 8, 6, 3, 1, 1, 1, 1, 1, 2)
scens[paste0("m", 1:12)] = as.list(season)
scens = om_scenario_values(scens, baseList, "ITN", net = "net", halflife = "attrition")

scens = finalize_scenarios(scens)
storeScenarios(scens)
# then generate and run as usual (setupOM(), generate scenarios, run)
