library(AnophelesModel2)
library(testthat)

# Export to OpenMalaria through openMalariaUtilities (OMU) input lists.

gss      = "Anopheles gambiae ss/coluzzii"
model_p  = build_model_obj(def_vector_params(gss), def_host_params())
activity = def_activity_patterns()
eff      = c(Deterrency = 0.5, PrePrandial = 0.4, PostPrandial = 0.3)
bi = function(...) suppressMessages(build_intervention(
    efficacy = eff, duration_years = 3, model_p = model_p, ...))

# ---------------------------------------------------------------------------
# 1. Entomology
# ---------------------------------------------------------------------------
test_that("mosquito parameters follow the OMU format and the bionomics", {
    om  = get_OM_ento_snippet(gss)
    vec = def_vector_params(gss, hbi = "weighted")
    hp  = def_host_params()
    expect_named(om, c("mosquitoParameters", "seasonalityParameters", "shares",
                       "model_params"))
    expect_named(om$mosquitoParameters, gss)
    # Host-seeking death rate of the population's model (its HBI)
    expect_equal(om$shares$muvA,
                 build_model_obj(vec, hp)$host_params$muvA)

    m = om$mosquitoParameters[[gss]]
    expect_equal(m$mosqRestDuration$value, as.character(max(1, floor(vec$tau + 0.5))))
    expect_equal(m$mosqSeekingDuration$value, as.character(vec$td))      # not tau
    expect_equal(m$extrinsicIncubationPeriod$value, as.character(vec$ts))
    expect_equal(as.numeric(m$mosqLaidEggsSameDayProportion$value), vec$A0, tolerance = 1e-5)
    expect_equal(as.numeric(m$mosqSurvivalFeedingCycleProbability$value), vec$M, tolerance = 1e-5)
    # OMU conventions: mean/variance for the beta samples, mean for these two
    expect_named(m$mosqProbBiting, c("mean", "variance"))
    expect_named(m$mosqProbOvipositing, "mean")
    expect_equal(as.numeric(m$mosqHumanBloodIndex$mean), vec$Chi, tolerance = 1e-5)
    expect_equal(as.numeric(m$mosqProbBiting$mean), hp$PBi[1], tolerance = 1e-5)
    # Animal hosts from host_ent_param
    expect_equal(as.numeric(m$nonHumanHosts$mosqProbBiting$value), hp$PBi[2], tolerance = 1e-5)
    expect_equal(om$shares$type, "all")
})

test_that("gambiae aliases are accepted with a warning and canonical names", {
    expect_warning(om <- get_OM_ento_snippet("Anopheles gambiae"), "merged")
    expect_named(om$mosquitoParameters, gss)
})

test_that("activity splits each taxon into indoor1 / indoor2 / outdoor", {
    om  = get_OM_ento_snippet(gss, activity = activity)
    vec = def_vector_params(gss)
    exposure = calculate_exposure(activity, vec)
    in_bed  = exposure$Exposure_Indoor_whileinbed
    resting = exposure$indoor_resting

    sh = om$shares
    expect_equal(sh$mosquito, paste0(gss, c("_indoor1", "_indoor2", "_outdoor")))
    expect_equal(sh$share, c(min(in_bed, resting),
                             max(in_bed, resting) - min(in_bed, resting),
                             1 - max(in_bed, resting)))
    expect_equal(sum(sh$share), 1)
    expect_equal(sh$in_bed,         c(TRUE, in_bed > resting, FALSE))
    expect_equal(sh$indoor_resting, c(TRUE, in_bed <= resting, FALSE))

    # Indoor populations use the indoor HBI, the outdoor one the outdoor HBI
    hbi = sapply(om$mosquitoParameters, function(m) as.numeric(m$mosqHumanBloodIndex$mean))
    expect_equal(unname(hbi), c(vec$HBI_indoor, vec$HBI_indoor, vec$HBI_outdoor),
                 tolerance = 1e-5)
    # All other parameters identical
    strip = function(m) m[names(m) != "mosqHumanBloodIndex"]
    expect_identical(strip(om$mosquitoParameters[[1]]), strip(om$mosquitoParameters[[3]]))

    # Seasonality skeleton with sanitised EIR placeholders
    expect_named(om$seasonalityParameters, sh$mosquito)
    expect_match(om$seasonalityParameters[[1]]$annualEIR, "^@EIR_[A-Za-z0-9_]+@$")
    expect_length(om$seasonalityParameters[[1]]$seasonality, 12)
})

test_that("several taxa can be exported together", {
    om = get_OM_ento_snippet(c(gss, "Anopheles funestus"), activity = activity)
    expect_length(om$mosquitoParameters, 6)
    expect_equal(c(tapply(om$shares$share, om$shares$taxon, sum)),
                 c("Anopheles funestus" = 1, "Anopheles gambiae ss/coluzzii" = 1))
})

# ---------------------------------------------------------------------------
# 2. Vector control (GVI)
# ---------------------------------------------------------------------------
om = get_OM_ento_snippet(gss, activity = activity)

test_that("ITN without insecticide decay: attrition decay, in-bed populations", {
    itn = bi(halflife_attrition = 2 * 365, shape_attrition = 2, name = "ITN",
             insecticide_decay = FALSE)
    gvi = get_OM_GVI_snippet(itn, om, "ITN")
    expect_named(gvi, "ITN")
    expect_named(gvi$ITN, c("deterrency", "preprandialKillingEffect",
                            "postprandialKillingEffect"))
    d = gvi$ITN$deterrency
    expect_equal(d$decay, list(L = "2", "function" = "weibull", k = "2"))
    expect_equal(d$anophelesParams[[1]], list(propActive = 1, value = "0.5"))
    expect_equal(d$anophelesParams[[3]], list(propActive = 0, value = "0"))   # outdoor
    expect_equal(sapply(d$anophelesParams, `[[`, "propActive"),
                 setNames(as.numeric(om$shares$in_bed), om$shares$mosquito))
})

test_that("IRS: no attrition, exact per-effect Weibull, indoor-resting populations", {
    w = list(decay = "weibull", L = 180, kappa = 2)
    irs = bi(decay_attrition = "none", name = "IRS",
             component_decay = list(Deterrency = w, PrePrandial = w, PostPrandial = w))
    gvi = get_OM_GVI_snippet(irs, om, "IRS")
    expect_equal(gvi$IRS$preprandialKillingEffect$decay,
                 list(L = as.character(signif(180 / 365, 6)), "function" = "weibull", k = "2"))
    expect_equal(sapply(gvi$IRS$deterrency$anophelesParams, `[[`, "propActive"),
                 setNames(as.numeric(om$shares$indoor_resting), om$shares$mosquito))
})

test_that("linear insecticide decay without attrition is exported exactly", {
    net = bi(decay_attrition = "none", halflife_insecticide = c(Deterrency = 4))
    expect_warning(gvi <- get_OM_GVI_snippet(net, om, "ITN"), "without attrition")
    expect_equal(gvi$GVI$deterrency$decay, list(L = "4", "function" = "linear"))
    expect_equal(gvi$GVI$postprandialKillingEffect$decay, list("function" = "constant"))
})

test_that("attrition x insecticide decay is exported as the fitted Weibull", {
    net = bi(halflife_attrition = 2 * 365, shape_attrition = 2,
             halflife_insecticide = c(Deterrency = 4, PrePrandial = 6))
    expect_message(gvi <- get_OM_GVI_snippet(net, om, "ITN"), "fitted")
    fd = net[[1]]$fitted_decay
    expect_equal(gvi$GVI$deterrency$decay,
                 list(L = as.character(signif(fd$L[fd$component == "Deterrency"] / 365, 6)),
                      "function" = "weibull",
                      k = as.character(signif(fd$kappa[fd$component == "Deterrency"], 6))))
    # Without insecticide decay for an effect: its attrition decay
    expect_equal(gvi$GVI$postprandialKillingEffect$decay,
                 list(L = "2", "function" = "weibull", k = "2"))
})

test_that("dummy-host GVI has no pre-prandial killing", {
    dh = suppressMessages(build_intervention(
        efficacy = c(Deterrency = 0.3, HostSeekingKilling = 0.6, PostPrandial = 0.4),
        halflife_attrition = 365, shape_attrition = 2, duration_years = 1,
        model_p = model_p))
    expect_message(gvi <- get_OM_GVI_snippet(dh, om, "ITN"), "define_OM_vector_control")
    expect_named(gvi[[1]], c("deterrency", "postprandialKillingEffect"))
    expect_equal(gvi[[1]]$postprandialKillingEffect$anophelesParams[[1]]$value, "0.4")
})

test_that("unsupported cases give clear errors or warnings", {
    neg = bi(decay_attrition = "none", halflife_insecticide = c(Deterrency = -5))
    expect_error(suppressWarnings(get_OM_GVI_snippet(neg, om, "ITN")), "Negative")

    irs_attr = bi(halflife_attrition = 365, shape_attrition = 2)
    expect_warning(get_OM_GVI_snippet(irs_attr, om, "IRS"), "IRS")
})

# ---------------------------------------------------------------------------
# 3. Host-seeking killing (dummy host) as seekingDeathRateIncrease steps
# ---------------------------------------------------------------------------
dh_net = function(hsk = 0.6, ...) suppressMessages(build_intervention(
    efficacy = c(Deterrency = 0.3, HostSeekingKilling = hsk, PostPrandial = 0.4),
    halflife_attrition = 365, shape_attrition = 2, duration_years = 2,
    model_p = model_p, ...))
sched_num = data.frame(date = c("2021-01-01", "2022-01-01"), coverage = c(0.8, 0.5))
sched_ph  = data.frame(date = c("2021-01-01", "2022-01-01"),
                       coverage = c("@cov2021@", "@cov2022@"))
end_date  = "2023-01-01"   # 730 days after the first campaign = net duration

test_that("step factors are the step means of the summed campaign curves", {
    net = dh_net(halflife_insecticide = c(HostSeekingKilling = 3))[[1]]
    sch = AnophelesModel2:::om_schedule(sched_num, end_date)
    fac = AnophelesModel2:::om_hsk_factors(net, sch)
    g = net$effects$alphai[, 4] * net$effects$survival
    # Brute force, day by day
    d = sapply(0:729, function(t) 0.8 * g[t + 1] + if (t >= 365) 0.5 * g[t - 365 + 1] else 0)
    expect_equal(as.vector(fac$B %*% c(0.8, 0.5)),
                 c(mean(d[1:365]), mean(d[366:730])))
    expect_equal(fac$B[1, 2], 0)                       # no nets before campaign 2
    expect_equal(fac$B[2, 1], mean(g[366:730]))        # leftover nets of campaign 1
    expect_equal(fac$L_years, c(1, 1))
    # Linear in the coverage
    expect_equal(fac$B %*% c(0.3, 0.2) + fac$B %*% c(0.5, 0.3), fac$B %*% c(0.8, 0.5))
})

test_that("schedules and durations are validated", {
    s = AnophelesModel2:::om_schedule
    expect_error(s(data.frame(date = "2021-01-01")), "columns date and coverage")
    expect_error(s(data.frame(date = c("2022-01-01", "2021-01-01"), coverage = 1)),
                 "increasing")
    expect_error(s(data.frame(date = "2021-01-01", coverage = 1.2)), "coverage")
    expect_error(s(data.frame(date = "2021-01-01", coverage = "cov")), "coverage")
    expect_error(s(sched_num, "2021-06-01"), "after the last campaign")
    sch = s(data.frame(date = "2021-01-01", coverage = 0.5), "2025-01-01")
    expect_error(AnophelesModel2:::om_hsk_factors(dh_net()[[1]], sch), "duration_years")
})

test_that("placeholder values: per net and coverage scenario, in-bed populations only", {
    nets = list(A = dh_net(0.6), B = dh_net(1.2))
    cov  = data.frame(cov2021 = c(0.8, 0.4), cov2022 = c(0.5, 0.5))
    v = suppressMessages(om_vector_control_values(nets, om, sched_ph, end_date,
                                                  coverages = cov, name = "ITN"))
    expect_equal(nrow(v), 4)
    expect_equal(v$net, c("A", "A", "B", "B"))
    expect_equal(v$cov2021, c(0.8, 0.4, 0.8, 0.4))
    expect_true(all(c("ITN_deterrency", "ITN_deterrency_L", "ITN_deterrency_k",
                      "ITN_postprandial", "ITN_hsk_1_pop1", "ITN_hsk_2_pop1") %in% names(v)))
    expect_false(any(grepl("preprandial", names(v))))
    expect_equal(v$ITN_deterrency, rep(0.3, 4))
    expect_equal(v$ITN_deterrency_L, rep(1, 4))          # exact Weibull, years
    # HSK only for in-bed populations, value = step mean / muvA
    pops = which(om$shares$in_bed)
    expect_setequal(grep("_hsk_1_", names(v), value = TRUE),
                    paste0("ITN_hsk_1_pop", pops))
    sch = AnophelesModel2:::om_schedule(sched_ph, end_date)
    fac = AnophelesModel2:::om_hsk_factors(nets$A[[1]], sch)
    expect_equal(v$ITN_hsk_2_pop1[1], sum(fac$B[2, ] * c(0.8, 0.5)) / om$shares$muvA[1])
    expect_equal(v$ITN_hsk_1_pop1[3], 2 * v$ITN_hsk_1_pop1[1])   # proportional to HSK

    # A single intervention with placeholder coverages: HSK columns only
    v1 = om_vector_control_values(nets$A, om, sched_ph, end_date, coverages = cov,
                                  name = "ITN")
    expect_equal(v1$ITN_hsk_1_pop1, v$ITN_hsk_1_pop1[1:2])
    expect_false("ITN_deterrency" %in% names(v1))
    expect_error(om_vector_control_values(nets$A, om, sched_ph, end_date), "coverages")
    expect_error(om_vector_control_values(list(A = nets$A, S = bi(halflife_attrition = 730,
                                                                 shape_attrition = 2)),
                                          om, sched_ph,
                                          end_date, coverages = cov), "same model")
})

# ---------------------------------------------------------------------------
# 4. Compatibility with openMalariaUtilities (if installed)
# ---------------------------------------------------------------------------
test_that("outputs are accepted by openMalariaUtilities", {
    skip_if_not_installed("openMalariaUtilities")
    # interventions$human must exist for defineVectorControl(append = TRUE)
    baseList = list(expName = "test", OMVersion = 44L, demography = list(),
                    monitoring = list(), interventions = list(human = list()),
                    healthSystem = list(), entomology = list(), model = list())
    itn = bi(halflife_attrition = 2 * 365, shape_attrition = 2, name = "ITN")
    # Numeric monthly seasonality: defineEntomology() currently fails on the
    # "@m1@".."@m12@" placeholders with R >= 4.3 (it uses grepl(...) && ... on
    # a length-12 vector), an OMU issue independent of this package.
    seas = lapply(om$seasonalityParameters, function(s) {
        s$seasonality = rep(1, 12); s })
    expect_no_error({
        baseList = openMalariaUtilities::defineEntomology(
            baseList, seas, om$mosquitoParameters)
        baseList = suppressMessages(openMalariaUtilities::defineVectorControl(
            baseList, get_OM_GVI_snippet(itn, om, "ITN")))
    })
})

test_that("add_OM_GVI writes one component when the decays are identical", {
    skip_if_not_installed("openMalariaUtilities")
    base = list(expName = "test", OMVersion = 44L, demography = list(),
                monitoring = list(), interventions = list(human = list()),
                healthSystem = list(), entomology = list(), model = list())
    same = bi(halflife_attrition = 2 * 365, shape_attrition = 2, name = "ITNsame",
              insecticide_decay = FALSE)
    diff = bi(halflife_attrition = 2 * 365, shape_attrition = 2, name = "ITNdiff",
              halflife_insecticide = c(Deterrency = 4))
    gvi = c(get_OM_GVI_snippet(same, om, "ITN"),
            suppressMessages(get_OM_GVI_snippet(diff, om, "ITN")))

    n_components = function(b) sum(names(b$interventions$human) == "component")
    expect_message(merged <- add_OM_GVI(base, gvi), "ITNsame, ITNdiff-deterrency")
    expect_equal(n_components(merged), 1 + 3)        # same decays -> 1, different -> 3
    unmerged = suppressMessages(add_OM_GVI(base, gvi, merge = FALSE))
    expect_equal(n_components(unmerged), 3 + 3)
    expect_error(add_OM_GVI(list(interventions = list()), gvi), "must exist")
})

om_base = function() list(expName = "test", OMVersion = 44L, demography = list(),
                          monitoring = list(), interventions = list(human = list()),
                          healthSystem = list(), entomology = list(), model = list())
by_name = function(l, nm) l[names(l) == nm]

test_that("define / deploy a dummy-host net with numeric coverages", {
    skip_if_not_installed("openMalariaUtilities")
    net = dh_net(name = "IG2")
    expect_message(b <- define_OM_vector_control(om_base(), net, om, sched_num, end_date),
                   "exact vs steps")
    # GVI without pre-prandial killing, merged (identical decays)
    ids = sapply(by_name(b$interventions$human, "component"), `[[`, "id")
    expect_equal(unname(ids), "IG2")
    # One step per campaign, deployed at its date, 0 for populations not in bed
    steps = by_name(b$interventions$vectorPop, "intervention")
    expect_equal(unname(sapply(steps, `[[`, "name")), c("IG2_hsk_1", "IG2_hsk_2"))
    expect_equal(steps[[2]]$timed$deploy$time, "2022-01-01")
    an = by_name(steps[[1]]$description, "anopheles")
    expect_equal(an[[3]]$seekingDeathRateIncrease$initial, "0")          # outdoor
    expect_equal(an[[1]]$seekingDeathRateIncrease$decay,
                 list("function" = "step", L = "1"))
    sch = AnophelesModel2:::om_schedule(sched_num, end_date)
    fac = AnophelesModel2:::om_hsk_factors(net[[1]], sch)
    expect_equal(as.numeric(an[[1]]$seekingDeathRateIncrease$initial),
                 sum(fac$B[1, ] * c(0.8, 0.5)) / om$shares$muvA[1], tolerance = 1e-5)
    expect_error(suppressMessages(define_OM_vector_control(b, net, om, sched_num, end_date)),
                 "already")

    # The deployment must match the schedule used for the killing steps
    other_cov = sched_num
    other_cov$coverage = c(0, 0.5)
    expect_error(deploy_OM_vector_control(b, "IG2", other_cov), "schedule differs")
    for (args in list(list(subpop = "x"), list(cumulative = TRUE), list(minAge = 5),
                      list(maxAge = 10)))
        expect_error(do.call(deploy_OM_vector_control, c(list(b, "IG2", sched_num), args)),
                     "not supported for dummy-host")

    b = suppressMessages(deploy_OM_vector_control(b, "IG2", sched_num))
    dep = by_name(b$interventions$human, "deployment")
    expect_length(dep, 1)
    deploys = by_name(dep[[1]]$timed, "deploy")
    expect_equal(unname(sapply(deploys, `[[`, "coverage")), c("0.8", "0.5"))
    expect_error(deploy_OM_vector_control(b, "IG2", sched_num[1, ]), "schedule differs")
    expect_error(deploy_OM_vector_control(b, "other", sched_num), "define_OM_vector_control")
})

test_that("define / deploy a template of nets: one deployment with all components", {
    skip_if_not_installed("openMalariaUtilities")
    nets = list(A = dh_net(0.6, halflife_insecticide = c(Deterrency = 3)), B = dh_net(1.2))
    b = suppressMessages(define_OM_vector_control(om_base(), nets, om, sched_ph,
                                                  end_date, name = "ITN"))
    comps = by_name(b$interventions$human, "component")
    expect_equal(unname(sapply(comps, `[[`, "id")),
                 c("ITN-deterrency", "ITN-postprandialKillingEffect"))
    expect_equal(comps[[1]]$GVI$decay,
                 list(L = "@ITN_deterrency_L@", "function" = "weibull", k = "@ITN_deterrency_k@"))
    an = by_name(by_name(b$interventions$vectorPop, "intervention")[[2]]$description,
                 "anopheles")
    expect_equal(an[[1]]$seekingDeathRateIncrease$initial, "@ITN_hsk_2_pop1@")

    sched_other = sched_ph
    sched_other$coverage = c("@cov2021@", "@covB@")
    expect_error(deploy_OM_vector_control(b, "ITN", sched_other), "schedule differs")
    b = suppressMessages(deploy_OM_vector_control(b, "ITN", sched_ph))
    dep = by_name(b$interventions$human, "deployment")[[1]]
    expect_equal(unname(sapply(by_name(dep, "component"), `[[`, "id")),
                 c("ITN-deterrency", "ITN-postprandialKillingEffect"))
    expect_equal(unname(sapply(by_name(dep$timed, "deploy"), `[[`, "coverage")),
                 c("@cov2021@", "@cov2022@"))
    # Every placeholder of the XML has a value
    v = suppressMessages(om_vector_control_values(nets, om, sched_ph, end_date,
        coverages = data.frame(cov2021 = 0.8, cov2022 = 0.5), name = "ITN"))
    strings = unlist(b)
    all_ph = unique(gsub("@", "", unlist(regmatches(strings, gregexpr("@[^@]+@", strings)))))
    expect_setequal(all_ph, names(v)[-1])
})

test_that("standard model: pre-prandial component and no host-seeking killing", {
    skip_if_not_installed("openMalariaUtilities")
    b = suppressMessages(define_OM_vector_control(
        om_base(), list(A = bi(halflife_attrition = 730, shape_attrition = 2),
                        B = bi(halflife_attrition = 365, shape_attrition = 1.5)),
        om, sched_ph, end_date, name = "ITN"))
    expect_true("ITN-preprandialKillingEffect" %in%
                sapply(by_name(b$interventions$human, "component"), `[[`, "id"))
    expect_null(b$interventions$vectorPop)

    # Restricted / cumulative deployments: one deployment with all components
    b = suppressMessages(deploy_OM_vector_control(b, "ITN", sched_ph, subpop = "eligible",
                                                  cumulative = TRUE))
    dep = by_name(b$interventions$human, "deployment")[[1]]
    expect_length(by_name(dep, "component"), 3)
    expect_equal(dep$timed$restrictToSubPop$id, "ITN-eligible")
    expect_equal(dep$timed$cumulativeCoverage$component, "ITN-eligible")
    expect_length(by_name(dep$timed, "deploy"), 2)
})

# ---------------------------------------------------------------------------
# 5. XML text snippets (no openMalariaUtilities needed)
# ---------------------------------------------------------------------------
to_xml = AnophelesModel2:::om_list_to_xml

test_that("the serializer follows the openMalariaUtilities conventions", {
    x = list(id = "a&b", n = 1, skip = NA, child = list(v = "1"), child = list(v = "2"))
    expect_equal(to_xml("el", x),
                 c('<el id="a&amp;b" n="1">', '  <child v="1"/>', '  <child v="2"/>', "</el>"))
    expect_equal(to_xml("el", list("text")), "<el>text</el>")
    expect_equal(to_xml("el", list(q = 'say "<hi>"')), '<el q="say &quot;&lt;hi&gt;&quot;"/>')
    expect_error(to_xml("el", list(a = NULL)), "single value")
    expect_error(to_xml("el", list(a = c("1", "2"))), "single value")
    expect_error(to_xml("el", list(list(a = "1"))), "unnamed child")
})

test_that("standard intervention: define and deploy snippets, no host-seeking killing", {
    itn = bi(halflife_attrition = 730, shape_attrition = 2, name = "ITN",
             insecticide_decay = FALSE)
    xml = get_OM_xml(om, itn, sched_num, end_date)
    expect_s3_class(xml, "om_xml")
    expect_match(xml$entomology, "^<vector>")
    expect_null(get_OM_xml(om, itn, sched_num, end_date, entomology = FALSE)$entomology)
    expect_null(xml$hsk)
    # One merged component (identical decays), deployed once per campaign
    expect_equal(lengths(regmatches(xml$define, gregexpr("<component ", xml$define))), 1)
    expect_match(xml$define, '<component id="ITN" name="ITN">', fixed = TRUE)
    expect_match(xml$define, '<decay L="2" function="weibull" k="2"/>', fixed = TRUE)
    expect_match(xml$deploy, '<deployment name="ITN">', fixed = TRUE)
    expect_match(xml$deploy, '<component id="ITN"/>', fixed = TRUE)
    expect_match(xml$deploy, '<deploy coverage="0.8" time="2021-01-01"/>', fixed = TRUE)
    expect_match(xml$deploy, '<deploy coverage="0.5" time="2022-01-01"/>', fixed = TRUE)
    expect_output(print(xml), "after all <component>")

    sub = get_OM_xml(om, itn, sched_num, end_date, subpop = "eligible", minAge = 5)
    expect_match(sub$deploy, '<restrictToSubPop id="ITN-eligible"/>', fixed = TRUE)
    expect_match(sub$deploy, 'minAge="5"', fixed = TRUE)
})

test_that("dummy-host intervention: no pre-prandial killing, host-seeking killing snippet", {
    xml = suppressMessages(get_OM_xml(om, dh_net(name = "IG2"), sched_ph, end_date))
    expect_match(xml$define, '<preprandialKillingEffect value="0"/>', fixed = TRUE)
    expect_match(xml$hsk, '<intervention name="IG2_hsk_2">', fixed = TRUE)
    expect_match(xml$hsk, 'initial="@IG2_hsk_1_pop1@"', fixed = TRUE)
    expect_match(xml$hsk, '<deploy time="2022-01-01"/>', fixed = TRUE)
    expect_match(xml$deploy, 'coverage="@cov2021@"', fixed = TRUE)
    expect_error(suppressMessages(get_OM_xml(om, dh_net(), sched_ph, end_date,
                                             subpop = "x")), "not supported")
    expect_message(get_OM_xml(om, dh_net(), sched_num, end_date), "exact vs steps")
})

test_that("template snippets carry every placeholder of om_vector_control_values()", {
    nets = list(A = dh_net(0.6), B = dh_net(1.2))
    # Entomology placeholders (EIR, seasonality) are not intervention values
    xml = suppressMessages(get_OM_xml(om, nets, sched_ph, end_date, name = "ITN",
                                      entomology = FALSE))
    text = paste(unlist(xml), collapse = "\n")
    ph = unique(gsub("@", "", regmatches(text, gregexpr("@[^@]+@", text))[[1]]))
    v = suppressMessages(om_vector_control_values(nets, om, sched_ph, end_date,
        coverages = data.frame(cov2021 = 0.8, cov2022 = 0.5), name = "ITN"))
    expect_setequal(ph, names(v)[-1])
})

test_that("entomology snippet: one <anopheles> per population, then the animal hosts", {
    xml = get_OM_xml(om)
    expect_null(xml$define)
    e = xml$entomology
    expect_equal(lengths(regmatches(e, gregexpr("<anopheles ", e))), nrow(om$shares))
    for (m in om$shares$mosquito)
        expect_match(e, paste0('<anopheles mosquito="', m, '" propInfected="0.078" ',
                               'propInfectious="0.021">'), fixed = TRUE)
    expect_match(e, 'annualEIR="@EIR_[A-Za-z0-9_]+@" input="EIR"')
    expect_match(e, '<monthlyValues smoothing="fourier">', fixed = TRUE)
    expect_match(e, "<value>@m12@</value>", fixed = TRUE)
    expect_match(e, '<mosq minInfectedThreshold="0.001">', fixed = TRUE)
    expect_match(e, '<availability distr="const"/>', fixed = TRUE)
    hbi = om$mosquitoParameters[[3]]$mosqHumanBloodIndex$mean      # outdoor HBI
    expect_match(e, paste0('<mosqHumanBloodIndex value="', hbi, '"/>'), fixed = TRUE)
    # Animal host parameters of hosts_p, declared once at the end
    pb = om$mosquitoParameters[[1]]$nonHumanHosts$mosqProbBiting$value
    expect_match(e, paste0('<mosqProbBiting value="', pb, '"/>'), fixed = TRUE)
    expect_match(e, '<nonHumanHosts name="unprotectedAnimals" number="1"/>\n</vector>$')

    daily = om
    daily$seasonalityParameters[[1]]$seasonality = rep(1, 365)
    expect_match(get_OM_xml(daily)$entomology, "<dailyValues", fixed = TRUE)
    daily$seasonalityParameters[[1]]$seasonality = 1:5
    expect_error(get_OM_xml(daily), "12 \\(monthly\\) or 365")
    expect_error(get_OM_xml(om, entomology = FALSE), "Nothing to write")
    expect_error(get_OM_xml(om, bi(halflife_attrition = 730, shape_attrition = 2)),
                 "schedule and end_date")
})

test_that("snippets are well-formed XML", {
    skip_if_not_installed("xml2")
    xml = suppressMessages(get_OM_xml(om, dh_net(name = "IG2"), sched_num, end_date))
    expect_no_error(xml2::read_xml(xml$entomology))
    expect_no_error(xml2::read_xml(paste0("<human>", xml$define, xml$deploy, "</human>")))
    expect_no_error(xml2::read_xml(paste0("<vectorPop>", xml$hsk, "</vectorPop>")))
})

test_that("entomology snippet matches openMalariaUtilities::defineEntomology()", {
    skip_if_not_installed("openMalariaUtilities")
    # Numeric seasonality: defineEntomology() rejects monthly placeholders
    # with R >= 4.3 (an openMalariaUtilities issue)
    om_num_seas = om
    om_num_seas$seasonalityParameters = lapply(om$seasonalityParameters, function(s) {
        s$seasonality = rep(1, 12); s })
    b = openMalariaUtilities::defineEntomology(
        om_base(), om_num_seas$seasonalityParameters, om_num_seas$mosquitoParameters)
    omu = b$entomology$vector
    ours = AnophelesModel2:::om_vector_list(om_num_seas)
    expect_equal(names(ours), names(omu))
    # Identical except the animal host parameters, fixed in defineEntomology()
    no_nhh = function(a) a[names(a) != "nonHumanHosts"]
    for (i in which(names(omu) == "anopheles"))
        expect_equal(to_xml("anopheles", no_nhh(ours[[i]])),
                     to_xml("anopheles", no_nhh(omu[[i]])), info = i)
    expect_equal(to_xml("nonHumanHosts", ours[[length(ours)]]),
                 to_xml("nonHumanHosts", omu[[length(omu)]]))
})

test_that("text snippets match what define / deploy write into a baseList", {
    skip_if_not_installed("openMalariaUtilities")
    for (net in list(dh_net(name = "IG2"),
                     bi(halflife_attrition = 730, shape_attrition = 2, name = "ITN",
                        halflife_insecticide = c(Deterrency = 4)))) {
        nm  = net[[1]]$description
        xml = suppressMessages(get_OM_xml(om, net, sched_num, end_date))
        b = suppressMessages(define_OM_vector_control(om_base(), net, om, sched_num, end_date))
        b = suppressMessages(deploy_OM_vector_control(b, nm, sched_num))
        human = b$interventions$human
        expect_equal(xml$define, paste(unlist(lapply(by_name(human, "component"),
                                                     function(x) to_xml("component", x))),
                                       collapse = "\n"), info = nm)
        expect_equal(xml$deploy, paste(to_xml("deployment", by_name(human, "deployment")[[1]]),
                                       collapse = "\n"), info = nm)
        steps = by_name(b$interventions$vectorPop, "intervention")
        if (length(steps))
            expect_equal(xml$hsk, paste(unlist(lapply(steps, function(x) to_xml("intervention", x))),
                                        collapse = "\n"))
    }
})
