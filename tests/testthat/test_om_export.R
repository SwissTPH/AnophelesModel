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
    expect_match(om$seasonalityParameters[[1]]$annualEIR, "^@contrib_[A-Za-z0-9_]+@$")
    expect_length(om$seasonalityParameters[[1]]$seasonality, 12)
})

test_that("species shares give relative annualEIR contributions", {
    taxa = c("Anopheles funestus", gss)
    sp = c("Anopheles funestus" = 0.9, "Anopheles gambiae" = 0.1)   # a synonym works
    om2 = suppressMessages(get_OM_ento_snippet(taxa, activity = activity, species_shares = sp))
    sh = om2$shares
    expect_equal(sh$species_share, rep(c(0.9, 0.1), each = 3))
    expect_equal(sh$eir_share, sh$species_share * sh$share)
    expect_equal(sum(sh$eir_share), 1)
    eir = sapply(om2$seasonalityParameters, `[[`, "annualEIR")
    expect_equal(unname(as.numeric(eir)), sh$eir_share, tolerance = 1e-5)
    expect_message(get_OM_ento_snippet(taxa, species_shares = sp), "scaledAnnualEIR")
    expect_output(print(get_OM_xml(om2)), "scaledAnnualEIR")

    bad = function(x) suppressMessages(get_OM_ento_snippet(taxa, species_shares = x))
    expect_error(bad(c("Anopheles funestus" = 0.9, "Anopheles gambiae ss/coluzzii" = 0.2)),
                 "sum to 1")
    expect_error(bad(c("Anopheles funestus" = 1)), "no share")
    expect_error(bad(c("Anopheles funestus" = 0.5, "Anopheles arabiensis" = 0.5)),
                 "not one of the taxa")
    expect_error(bad(c(0.5, 0.5)), "named")
})

test_that("eir_shares weights abundance by sporozoite rate", {
    s = eir_shares(c(a = 0.95, b = 0.05, c = 0), c(b = 0.114, a = 0.235, c = 0.049))
    expect_equal(s, c(a = 0.95 * 0.235, b = 0.05 * 0.114, c = 0) /
                     (0.95 * 0.235 + 0.05 * 0.114))
    expect_error(eir_shares(c(a = 0.9, b = 0.05), c(a = 0.1, b = 0.1)), "sum to 1")
    expect_error(eir_shares(c(a = 1), c(b = 0.1)), "same names")
    expect_error(eir_shares(c(a = 1), c(a = 0)), "No species")
    expect_error(eir_shares(c(a = 1), c(a = 2)), "proportions")
})

test_that("om_ento_values fills the contribution placeholders", {
    taxa = c("Anopheles funestus", gss)
    expect_message(om2 <- get_OM_ento_snippet(taxa, activity = activity),
                   "contrib_.*scaledAnnualEIR")
    mixes = rbind(fun = c("Anopheles funestus" = 0.9, "Anopheles gambiae ss/coluzzii" = 0.1),
                  eq  = c("Anopheles funestus" = 0.5, "Anopheles gambiae ss/coluzzii" = 0.5))
    v = om_ento_values(om2, mixes)
    expect_equal(v$composition, c("fun", "eq"))
    ph = gsub("@", "", sapply(om2$seasonalityParameters, `[[`, "annualEIR"))
    expect_true(all(startsWith(ph, "contrib_")))
    expect_true(all(ph %in% names(v)))
    expect_equal(unname(rowSums(v[ph])), c(1, 1))        # contributions sum to 1
    j = which(om2$shares$taxon == "Anopheles funestus")[1]
    expect_equal(v[[ph[j]]][1], 0.9 * om2$shares$share[j])
    # Same contributions as the numbers written with species_shares
    om3 = suppressMessages(get_OM_ento_snippet(taxa, activity = activity,
                                               species_shares = mixes[1, ]))
    expect_equal(unname(unlist(v[1, ph])), om3$shares$eir_share)
    one = om_ento_values(om2, mixes[1, ])                 # a single composition
    expect_equal(nrow(one), 1)
    expect_null(one$composition)
    expect_error(om_ento_values(om2, mixes * 2), "composition 1.*sum to 1")
    expect_error(om_ento_values(om3, mixes), "not placeholders")
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
    expect_message(gvi <- get_OM_GVI_snippet(dh, om, "ITN"), "deploy_OM_vector_control")
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
    # A new baseList, without human interventions yet
    fresh = suppressMessages(add_OM_GVI(list(interventions = list()), gvi))
    expect_equal(n_components(fresh), 1 + 3)
})

# Like a new openMalariaUtilities baseList: no human interventions yet
om_base = function() list(expName = "test", OMVersion = 44L, demography = list(),
                          monitoring = list(), interventions = list(),
                          healthSystem = list(), entomology = list(), model = list())
by_name = function(l, nm) l[names(l) == nm]
# baseList with the entomology of `om` (define_OM_vector_control() uses it)
om_base_ento = function() suppressMessages(define_OM_entomology(om_base(), om, rep(1, 12)))

test_that("define / deploy a dummy-host net with numeric coverages", {
    skip_if_not_installed("openMalariaUtilities")
    net = dh_net(name = "IG2")
    expect_message(b <- define_OM_vector_control(om_base_ento(), net),
                   "added by deploy_OM_vector_control")
    # GVI without pre-prandial killing, merged (identical decays); no steps yet
    ids = sapply(by_name(b$interventions$human, "component"), `[[`, "id")
    expect_equal(unname(ids), "IG2")
    expect_null(b$interventions$vectorPop)
    expect_false(is.null(b$anophelesmodel2$hsk$IG2))
    expect_error(suppressMessages(define_OM_vector_control(b, net)), "already")
    expect_error(define_OM_vector_control(om_base(), net), "define_OM_entomology")

    # Deploy: restrictions and end_date are checked, then the steps are written
    for (args in list(list(subpop = "x"), list(cumulative = TRUE), list(minAge = 5),
                      list(maxAge = 10)))
        expect_error(do.call(deploy_OM_vector_control,
                             c(list(b, "IG2", sched_num, end_date), args)),
                     "not supported for dummy-host")
    expect_error(deploy_OM_vector_control(b, "IG2", sched_num), "end_date is needed")
    expect_message(b <- deploy_OM_vector_control(b, "IG2", sched_num, end_date),
                   "exact vs steps")

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

    dep = by_name(b$interventions$human, "deployment")
    expect_length(dep, 1)
    deploys = by_name(dep[[1]]$timed, "deploy")
    expect_equal(unname(sapply(deploys, `[[`, "coverage")), c("0.8", "0.5"))
    # A dummy-host intervention is deployed once, with the whole schedule
    expect_error(deploy_OM_vector_control(b, "IG2", sched_num, end_date), "already deployed")
    expect_error(deploy_OM_vector_control(b, "other", sched_num), "define_OM_vector_control")
    # Duration too short for the schedule: checked at deploy
    b2 = suppressMessages(define_OM_vector_control(om_base_ento(), dh_net(name = "S")))
    expect_error(suppressMessages(deploy_OM_vector_control(b2, "S", sched_num, "2026-01-01")),
                 "duration_years")
})

test_that("define / deploy a template of nets: one deployment with all components", {
    skip_if_not_installed("openMalariaUtilities")
    nets = list(A = dh_net(0.6, halflife_insecticide = c(Deterrency = 3)), B = dh_net(1.2))
    b = suppressMessages(define_OM_vector_control(om_base_ento(), nets, name = "ITN"))
    comps = by_name(b$interventions$human, "component")
    expect_equal(unname(sapply(comps, `[[`, "id")),
                 c("ITN-deterrency", "ITN-postprandialKillingEffect"))
    expect_equal(comps[[1]]$GVI$decay,
                 list(L = "@ITN_deterrency_L@", "function" = "weibull", k = "@ITN_deterrency_k@"))

    b = suppressMessages(deploy_OM_vector_control(b, "ITN", sched_ph, end_date))
    an = by_name(by_name(b$interventions$vectorPop, "intervention")[[2]]$description,
                 "anopheles")
    expect_equal(an[[1]]$seekingDeathRateIncrease$initial, "@ITN_hsk_2_pop1@")
    dep = by_name(b$interventions$human, "deployment")[[1]]
    expect_equal(unname(sapply(by_name(dep, "component"), `[[`, "id")),
                 c("ITN-deterrency", "ITN-postprandialKillingEffect"))
    expect_equal(unname(sapply(by_name(dep$timed, "deploy"), `[[`, "coverage")),
                 c("@cov2021@", "@cov2022@"))
    # Every placeholder of the interventions has a value (the entomology has
    # its own: EIR, contributions, seasonality)
    v = suppressMessages(om_vector_control_values(nets, om, sched_ph, end_date,
        coverages = data.frame(cov2021 = 0.8, cov2022 = 0.5), name = "ITN"))
    strings = unlist(b$interventions)
    all_ph = unique(gsub("@", "", unlist(regmatches(strings, gregexpr("@[^@]+@", strings)))))
    expect_setequal(all_ph, names(v)[-1])
})

test_that("standard model: pre-prandial component and no host-seeking killing", {
    skip_if_not_installed("openMalariaUtilities")
    b = suppressMessages(define_OM_vector_control(
        om_base_ento(), list(A = bi(halflife_attrition = 730, shape_attrition = 2),
                             B = bi(halflife_attrition = 365, shape_attrition = 1.5)),
        name = "ITN"))
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
    expect_match(e, 'annualEIR="@contrib_[A-Za-z0-9_]+@" input="EIR"')
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
                 "schedule is needed")
    # end_date: needed for dummy-host interventions only
    expect_error(suppressMessages(get_OM_xml(om, dh_net(), sched_num)), "end_date is needed")
    expect_no_error(get_OM_xml(om, bi(halflife_attrition = 730, shape_attrition = 2),
                               sched_num, entomology = FALSE))
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
        b = suppressMessages(define_OM_vector_control(om_base_ento(), net))
        b = suppressMessages(deploy_OM_vector_control(b, nm, sched_num, end_date))
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

# ---------------------------------------------------------------------------
# 6. Interventions without a model (om_vector_control), entomology in baseList
# ---------------------------------------------------------------------------
test_that("om_vector_control takes built-in parameterisations and validates early", {
    p = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host", verbose = FALSE)
    ig2 = om_vector_control(p, halflife_attrition = 730, shape_attrition = 2)
    expect_s3_class(ig2, "om_vector_control")
    expect_equal(ig2$mode, "dummy_host")
    expect_equal(ig2$efficacy, p$efficacy)
    expect_equal(ig2$halflife_insecticide, p$halflife_insecticide)
    expect_equal(ig2$name, p$name)
    std = om_vector_control(eff, halflife_attrition = 730, shape_attrition = 2)
    expect_equal(std$mode, "standard")
    expect_error(om_vector_control(eff), "L")                      # Weibull without half-life
    expect_error(om_vector_control(eff, 730, 2, fit_years = -1), "duration_years")
})

test_that("GVI decays are fitted over 3 years for ITNs and 1 year for IRS", {
    hl = c(Deterrency = 4, PrePrandial = 6)
    spec = om_vector_control(eff, halflife_attrition = 730, shape_attrition = 2,
                             halflife_insecticide = hl)
    obj = function(years) suppressMessages(build_intervention(
        efficacy = eff, halflife_attrition = 730, shape_attrition = 2,
        halflife_insecticide = hl, duration_years = years, model_p = model_p))
    g = function(x, type) suppressWarnings(suppressMessages(get_OM_GVI_snippet(x, om, type)))
    expect_equal(g(spec, "ITN")[[1]]$deterrency$decay, g(obj(3), "ITN")[[1]]$deterrency$decay)
    expect_equal(g(spec, "IRS")[[1]]$deterrency$decay, g(obj(1), "IRS")[[1]]$deterrency$decay)
    spec5 = om_vector_control(eff, halflife_attrition = 730, shape_attrition = 2,
                              halflife_insecticide = hl, fit_years = 5)
    expect_equal(g(spec5, "ITN")[[1]]$deterrency$decay, g(obj(5), "ITN")[[1]]$deterrency$decay)
})

test_that("host-seeking killing of a specification covers any schedule", {
    spec = om_vector_control(c(Deterrency = 0.3, HostSeekingKilling = 0.6, PostPrandial = 0.4),
                             halflife_attrition = 365, shape_attrition = 2)
    long = data.frame(date = c("2021-01-01", "2025-01-01"), coverage = c("@c1@", "@c2@"))
    cov  = data.frame(c1 = c(0.8, 0.5), c2 = 0.6)
    v = suppressMessages(om_vector_control_values(spec, om, long, "2030-01-01", cov, name = "N"))
    # Same as an intervention object built over exactly the period
    n_days = as.numeric(as.Date("2030-01-01") - as.Date("2021-01-01"))
    legacy = suppressMessages(build_intervention(spec$efficacy, halflife_attrition = 365,
        shape_attrition = 2, duration_years = n_days / 365, model_p = model_p))
    v_legacy = suppressMessages(om_vector_control_values(legacy, om, long, "2030-01-01",
                                                         cov, name = "N"))
    expect_equal(v, v_legacy)
    expect_no_error(suppressMessages(get_OM_xml(om, spec, long, "2030-01-01")))
})

test_that("define_OM_entomology keeps the entomology used by the interventions", {
    skip_if_not_installed("openMalariaUtilities")
    b = om_base_ento()
    expect_identical(b$anophelesmodel2$ento, om)
    an = by_name(b$entomology$vector, "anopheles")
    expect_equal(unname(sapply(an, `[[`, "mosquito")), om$shares$mosquito)
    expect_equal(b$entomology$scaledAnnualEIR, "@EIR@")
    # One seasonality per population
    seas = setNames(lapply(seq_along(om$shares$mosquito), function(j) rep(j, 12)),
                    om$shares$mosquito)
    b2 = suppressMessages(define_OM_entomology(om_base(), om, seas, scaledAnnualEIR = 20))
    an2 = by_name(b2$entomology$vector, "anopheles")
    expect_equal(an2[[2]]$seasonality$monthlyValues[[2]][[1]], 2)
    expect_error(define_OM_entomology(om_base(), om, paste0("@m", 1:12, "@")), "placeholders")
    expect_error(define_OM_entomology(om_base(), om, seas[1]), "one element per population")

    # The whole route: entomology, then interventions without a model
    ig2 = om_vector_control(get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                                           verbose = FALSE),
                            halflife_attrition = 730, shape_attrition = 2)
    b = suppressMessages(define_OM_vector_control(b, ig2, name = "IG2"))
    b = suppressMessages(deploy_OM_vector_control(b, "IG2", sched_num, "2027-01-01"))
    expect_length(by_name(b$interventions$vectorPop, "intervention"), 2)
})

# ---------------------------------------------------------------------------
# 7. Campaign schedules
# ---------------------------------------------------------------------------
test_that("om_campaigns builds regular or given schedules", {
    s = om_campaigns("2015-01-25", "2018-01-25", coverage = "@histITNcov@", by_year = TRUE)
    expect_equal(s$date, c("2015-01-25", "2016-01-25", "2017-01-25", "2018-01-25"))
    expect_equal(s$coverage, paste0("@histITNcov", 2015:2018, "@"))
    s = om_campaigns("2019-01-25", "2021-07-25", every = "6 months", coverage = 0.6)
    expect_equal(s$date, c("2019-01-25", "2019-07-25", "2020-01-25", "2020-07-25",
                           "2021-01-25", "2021-07-25"))
    expect_equal(s$coverage, rep(0.6, 6))
    s = om_campaigns(dates = c("2021-01-01", "2024-01-01"), coverage = c(0.8, "@c2@"))
    expect_equal(s$coverage, c("0.8", "@c2@"))
    expect_error(om_campaigns("2015-01-01", "2016-01-01", every = "6 months",
                              coverage = "@c@", by_year = TRUE), "one campaign per year")
    expect_error(om_campaigns("2015-01-01", "2016-01-01", coverage = 0.5, by_year = TRUE),
                 "placeholder")
    expect_error(om_campaigns("2015-01-01", "2017-01-01", coverage = c(0.5, 0.6)), "one per")
    expect_error(om_campaigns(dates = c("2024-01-01", "2021-01-01"), coverage = 1),
                 "increasing")
    expect_error(om_campaigns("2015-01-01", coverage = 1), "from and to")
})

test_that("a placeholder repeated over campaigns gives one coverage column", {
    spec = om_vector_control(c(Deterrency = 0.3, HostSeekingKilling = 0.6, PostPrandial = 0.4),
                             halflife_attrition = 365, shape_attrition = 2)
    same = om_campaigns("2021-01-01", "2023-01-01", coverage = "@cont@")
    v = suppressMessages(om_vector_control_values(spec, om, same, "2024-01-01",
                                                  data.frame(cont = c(0.5, 0.8)), name = "N"))
    expect_equal(sum(names(v) == "cont"), 1)
    expect_false(any(grepl("^cont\\.", names(v))))
    # Same values as three placeholders with the same coverages
    three = om_campaigns(dates = same$date, coverage = c("@a@", "@b@", "@c@"))
    v3 = suppressMessages(om_vector_control_values(spec, om, three, "2024-01-01",
        data.frame(a = c(0.5, 0.8), b = c(0.5, 0.8), c = c(0.5, 0.8)), name = "N"))
    hsk = grep("_hsk_", names(v), value = TRUE)
    expect_equal(v[hsk], v3[hsk])
})

test_that("a list keeps the decay function shared by its nets (e.g. a step)", {
    p = function(eht) get_ITN_params("Interceptor G2", eht, verbose = FALSE)
    hist = list(
        martin  = om_vector_control(p("Martin"),  halflife_attrition = 365,
                                    decay_attrition = "step", insecticide_decay = FALSE),
        assenga = om_vector_control(p("Assenga"), halflife_attrition = 365,
                                    decay_attrition = "step", insecticide_decay = FALSE))
    b = suppressMessages(AnophelesModel2:::om_vc_components(hist, om, "ITN", "histITN"))
    decay = b$components[[1]]$GVI$decay
    expect_equal(decay, list(L = "@histITN_deterrency_L@", "function" = "step"))   # no k
    sched = om_campaigns("2015-01-25", "2018-01-25", coverage = "@histITNcov@")
    v = suppressMessages(om_vector_control_values(hist, om, sched, "2019-01-25",
        data.frame(histITNcov = 0.6), name = "histITN"))
    expect_equal(v$histITN_deterrency_L, c(1, 1))      # 365 days, in years
    expect_false("histITN_deterrency_k" %in% names(v))
    # Different decay functions: Weibull for all (step fitted)
    mixed = list(step = hist$martin,
                 weib = om_vector_control(p("Martin"), halflife_attrition = 730,
                                          shape_attrition = 2, insecticide_decay = FALSE))
    b = suppressMessages(AnophelesModel2:::om_vc_components(mixed, om, "ITN", "mix"))
    expect_equal(b$components[[1]]$GVI$decay[["function"]], "weibull")
})

test_that("deploying a member of a list points to the list's name", {
    skip_if_not_installed("openMalariaUtilities")
    nets = list(martin = om_vector_control(eff, 730, 2), assenga = om_vector_control(eff, 365, 2))
    b = suppressMessages(define_OM_vector_control(om_base_ento(), nets, name = "futITN"))
    expect_error(deploy_OM_vector_control(b, "martin", sched_num),
                 "one of the interventions of the list defined as 'futITN'.*deploy 'futITN'")
    expect_no_error(suppressMessages(deploy_OM_vector_control(b, "futITN", sched_num)))
})
