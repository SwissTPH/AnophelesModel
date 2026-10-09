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
    expect_named(om, c("mosquitoParameters", "seasonalityParameters", "shares"))
    expect_named(om$mosquitoParameters, gss)

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

test_that("unsupported cases give clear errors or warnings", {
    dh = suppressMessages(build_intervention(
        efficacy = c(Deterrency = 0.3, HostSeekingKilling = 0.6, PostPrandial = 0.4),
        halflife_attrition = 365, shape_attrition = 2, duration_years = 1,
        model_p = model_p))
    expect_error(get_OM_GVI_snippet(dh, om, "ITN"), "Dummy-host")

    neg = bi(decay_attrition = "none", halflife_insecticide = c(Deterrency = -5))
    expect_error(suppressWarnings(get_OM_GVI_snippet(neg, om, "ITN")), "Negative")

    irs_attr = bi(halflife_attrition = 365, shape_attrition = 2)
    expect_warning(get_OM_GVI_snippet(irs_attr, om, "IRS"), "IRS")
})

# ---------------------------------------------------------------------------
# 3. Compatibility with openMalariaUtilities (if installed)
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
