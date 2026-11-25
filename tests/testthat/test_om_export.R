library(AnophelesModel)
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
    expect_equal(om$irs_exposure, "indoor_resting")             # the default
})

test_that("IRS can reach the mosquitoes biting indoors instead of resting indoors", {
    om  = suppressMessages(get_OM_ento_snippet(gss, activity = activity,
                                               irs_exposure = "indoor_biting"))
    exposure = calculate_exposure(activity, def_vector_params(gss, verbose = FALSE))
    in_bed = exposure$Exposure_Indoor_whileinbed
    indoor = exposure$Exposure_Indoor_total
    expect_lte(in_bed, indoor)                       # bites in bed are bites indoors
    sh = om$shares
    expect_equal(om$irs_exposure, "indoor_biting")
    # indoor1 = in bed (ITN and IRS), indoor2 = indoors outside bed (IRS only)
    expect_equal(sh$share, c(in_bed, indoor - in_bed, 1 - indoor))
    expect_equal(sh$in_bed,         c(TRUE, FALSE, FALSE))
    expect_equal(sh$indoor_resting, c(TRUE, TRUE, FALSE))
    # The IRS export acts on indoor1 and indoor2, ITNs on indoor1 only
    irs = suppressWarnings(get_OM_GVI_snippet(
        bi(decay_attrition = "none", insecticide_decay = FALSE), om, "IRS"))[[1]]
    expect_equal(unname(sapply(irs$deterrency$anophelesParams, `[[`, "propActive")),
                 c(1, 1, 0))
    itn = get_OM_GVI_snippet(bi(halflife_attrition = 730, shape_attrition = 2), om, "ITN")[[1]]
    expect_equal(unname(sapply(itn$deterrency$anophelesParams, `[[`, "propActive")),
                 c(1, 0, 0))
    # Default unchanged; invalid choice rejected
    expect_equal(suppressMessages(get_OM_ento_snippet(gss, activity = activity))$irs_exposure,
                 "indoor_resting")
    expect_error(get_OM_ento_snippet(gss, activity = activity, irs_exposure = "outdoor"))
})

test_that("activity patterns: one per taxon with list(), c() is an error", {
    a1 = def_activity_patterns("default_Anopheles_gambiae")
    a2 = def_activity_patterns(list(HBI = 35, HBO = 166, humans_indoors = 4,
                                    humans_in_bed = 24))
    taxa = c(gss, "Anopheles funestus")
    # c() merges the patterns (duplicated entries): the second taxon would
    # silently get the first one's pattern
    expect_error(get_OM_ento_snippet(taxa, activity = c(a1, a2)), "combined with c\\(\\)")
    expect_error(calculate_exposure(c(a1, a2), def_vector_params(gss, verbose = FALSE)),
                 "combined with c\\(\\)")
    ok = suppressMessages(get_OM_ento_snippet(taxa, activity = list(a1, a2)))
    expect_equal(nrow(ok$shares), 6)
    expect_error(get_OM_ento_snippet(taxa, activity = list(a1)), "one pattern per taxon")
    expect_error(get_OM_ento_snippet(taxa, activity = list(a1, list(HBI = 1))),
                 "activity\\[\\[2\\]\\] must be an activity pattern")
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

test_that("populations with a share of 0 are not written", {
    taxa = c("Anopheles funestus", gss)
    sp = c("Anopheles funestus" = 1, "Anopheles gambiae ss/coluzzii" = 0)
    expect_message(om0 <- get_OM_ento_snippet(taxa, activity = activity, species_shares = sp),
                   "share of 0 not written.*gambiae")
    expect_equal(unique(om0$shares$taxon), "Anopheles funestus")
    for (part in c("mosquitoParameters", "seasonalityParameters", "model_params"))
        expect_named(om0[[part]], om0$shares$mosquito, info = part)
    expect_true(all(as.numeric(sapply(om0$seasonalityParameters, `[[`, "annualEIR")) > 0))
    expect_false(any(grepl("gambiae", get_OM_xml(om0)$entomology)))
    expect_error(suppressMessages(get_OM_ento_snippet(
        "Anopheles funestus", species_shares = c("Anopheles funestus" = 0))), "sum to 1")
    # With placeholders the population stays: om_ento_values() warns about a 0
    om_ph = suppressMessages(get_OM_ento_snippet(taxa, activity = activity))
    expect_warning(v <- om_ento_values(om_ph, sp), "gambiae.*share of 0")
    expect_equal(v[[AnophelesModel:::om_contrib_placeholder(
        paste0(gss, "_indoor1"))]], 0)
    expect_no_warning(om_ento_values(om_ph, c("Anopheles funestus" = 0.5,
                                              "Anopheles gambiae ss/coluzzii" = 0.5)))
})

test_that("two populations cannot share a placeholder name", {
    row = vec_ent_param[vec_ent_param$species_name == gss & vec_ent_param$level == "species", ]
    a = row; a$species_name = "Anopheles foo bar"
    b = row; b$species_name = "Anopheles foo/bar"
    tab = rbind(vec_ent_param, a, b)
    expect_error(suppressMessages(get_OM_ento_snippet(c("Anopheles foo bar", "Anopheles foo/bar"),
                                                      bionomics_table = tab)),
                 "same placeholder name \\(contrib_Anopheles_foo_bar\\)")
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
        efficacy = c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.6, PostPrandial = 0.4),
        halflife_attrition = 365, shape_attrition = 2, duration_years = 1,
        model_p = model_p))
    # Host-seeking killing is not in the GVI: a warning points to the routes
    # that write it
    expect_warning(gvi <- get_OM_GVI_snippet(dh, om, "ITN"),
                   "Host-seeking killing is not part of it.*define_OM_vector_control")
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
# 3. Host-seeking killing (dummy host) as added non-human hosts
# ---------------------------------------------------------------------------
dh_net = function(hsk = 0.6, halflife_attrition = 365, shape_attrition = 2, ...)
    suppressMessages(build_intervention(
        efficacy = c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = hsk, PostPrandial = 0.4),
        halflife_attrition = halflife_attrition, shape_attrition = shape_attrition,
        duration_years = 2, model_p = model_p, ...))
sched_num = data.frame(date = c("2021-01-01", "2022-01-01"), coverage = c(0.8, 0.5))
sched_ph  = data.frame(date = c("2021-01-01", "2022-01-01"),
                       coverage = c("@cov2021@", "@cov2022@"))
end_date  = "2023-01-01"
by_name = function(l, nm) l[names(l) == nm]
# Human availability of population j (the dummy hosts are relative to it)
alpha_h = function(j) om$model_params[[j]]$host_params$alphai[1]
dummy_rec = function(x, name = "N")
    suppressMessages(AnophelesModel:::om_vc_components(x, om, "ITN", name, schema = 49))$rec
# Availability of the dummy host of campaign k for population j, rebuilt from
# the written lists at host ages `days`, as OpenMalaria computes it: value x,
# for each modification deployed `offset` days after the host,
# 1 - initial x e(age - offset) from then on (e = 1 - f for an increasing
# decay, f otherwise; 0 before the deployment)
om_rebuild = function(h, k, j, days) {
    host = h$add[[k]]
    v = as.numeric(by_name(host$description, "anopheles")[[j]]$mosqRelativeAvailabilityHuman$value)
    for (m in Filter(function(m) m$nonHumanHostsName == host$name, h$modify)) {
        d = m$decay
        offset = as.numeric(as.Date(m$timed$deploy$time) - as.Date(host$timed$deploy$time))
        a = pmax(days - offset, 0)
        f = decay_function(L = if (!is.null(d$L)) 365 * as.numeric(d$L),
                           kappa = if (!is.null(d$k)) as.numeric(d$k),
                           datej = a, decay = d[["function"]])
        e = if (identical(d$increasing, "true")) 1 - f else f
        r = as.numeric(by_name(m$description, "anopheles")[[j]]$availabilityReduction$initial)
        v = v * (1 - ifelse(days >= offset, r * e, 0))
    }
    v
}

test_that("the dummy-host export has no work-in-progress warning (run in OpenMalaria 49)", {
    expect_no_warning(suppressMessages(get_OM_xml(om, dh_net(), sched_num, schema = 49)))
    # get_OM_GVI_snippet() only warns that host-seeking killing is not in the GVI
    w = character(0)
    withCallingHandlers(suppressMessages(get_OM_GVI_snippet(dh_net(), om, "ITN")),
                        warning = function(x) {
                            w <<- c(w, conditionMessage(x))
                            invokeRestart("muffleWarning")
                        })
    expect_length(w, 1)
    expect_match(w, "Host-seeking killing is not part of it")
})

test_that("dummy hosts follow the decay of host-seeking killing, as the GVI effects", {
    days = 0:729
    sch  = AnophelesModel:::om_schedule(sched_num, end_date)
    j    = which(om$shares$in_bed)[1]
    # A single OpenMalaria decay: exact
    exact = list(
        attrition   = list(),
        exponential = list(decay_attrition = "none", component_decay = list(
                          KillingDuringHostSeekingAbsolute = list(decay = "exponential", L = 200))))
    # Attrition x insecticide decay: the Weibull fitted to their product
    fitted = list(
        linear = list(halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 1.5)),
        rising = list(halflife_insecticide = c(KillingDuringHostSeekingAbsolute = -2)),
        step   = list(decay_attrition = "step", halflife_attrition = 400,
                      halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 3)))
    for (case in c(names(exact), names(fitted))) {
        net = suppressWarnings(do.call(dh_net, c(exact, fitted)[[case]]))[[1]]
        h   = AnophelesModel:::om_dummy_hosts(suppressWarnings(dummy_rec(net)), sch)
        # One decay modification per host
        expect_equal(sum(grepl("_decay$", sapply(h$modify, `[[`, "name"))), 2, info = case)
        # The package: KillingDuringHostSeekingAbsolute x insecticide decay x attrition, per
        # covered person, relative to the human availability
        exact_g = net$effects$alphai[, 4] * net$effects$survival / alpha_h(j)
        g = if (case %in% names(exact)) exact_g else {
            fd = net$fitted_decay[net$fitted_decay$component == "KillingDuringHostSeekingAbsolute", ]
            0.6 * decay_function(fd$L, fd$kappa, days, "weibull") / alpha_h(j)
        }
        # Campaign 1 (coverage 0.8), its nets replaced from campaign 2 (365
        # days later, coverage 0.5) in a random half of the population
        expect_equal(om_rebuild(h, 1, j, days), 0.8 * g * ifelse(days >= 365, 0.5, 1),
                     tolerance = 1e-5, info = case)
        expect_equal(om_rebuild(h, 2, j, days), 0.5 * g, tolerance = 1e-5, info = case)
        # A smooth product is close to the exact curve: on average within 2%
        # of the initial value, at most 10% on any day (largest near the start)
        if (case == "linear") {
            expect_lt(sqrt(mean((g - exact_g)^2)), 0.02 * exact_g[1])
            expect_lt(max(abs(g - exact_g)), 0.1 * exact_g[1])
        }
    }
    # A rising insecticide decay without attrition has no OpenMalaria shape
    expect_error(suppressWarnings(dummy_rec(dh_net(decay_attrition = "none",
        halflife_insecticide = c(KillingDuringHostSeekingAbsolute = -2))[[1]])), "Negative halflife_insecticide")
})

test_that("campaigns replace nets: the expected GVI effect, not stacked cohorts", {
    # Two campaigns at 100%, no decay: one net's worth, not two
    net = dh_net(decay_attrition = "none", insecticide_decay = FALSE)[[1]]
    # No attrition on purpose: the usual warning for such an ITN
    expect_warning(rec <- dummy_rec(net), "ITN without attrition")
    s = data.frame(date = c("2021-01-01", "2022-01-01"), coverage = c(1, 1))
    h = AnophelesModel:::om_dummy_hosts(rec, AnophelesModel:::om_schedule(s))
    j = which(om$shares$in_bed)[1]
    total = function(t) om_rebuild(h, 1, j, t) +
        ifelse(t >= 365, om_rebuild(h, 2, j, pmax(t - 365, 0)), 0)
    one = 0.6 / alpha_h(j)
    expect_equal(total(c(0, 200, 365, 600)), rep(one, 4), tolerance = 1e-6)
    # Three campaigns: weights c_j x prod (1 - c_k) of the later ones
    s3 = data.frame(date = c("2021-01-01", "2022-01-01", "2023-01-01"),
                    coverage = c(0.8, 0.5, "@c3@"))
    h3 = AnophelesModel:::om_dummy_hosts(rec, AnophelesModel:::om_schedule(s3))
    rep_mods = Filter(function(m) grepl("_replaced_", m$name), h3$modify)
    expect_equal(unname(sapply(rep_mods, `[[`, "name")),
                 c("N_dummy_1_replaced_2", "N_dummy_1_replaced_3", "N_dummy_2_replaced_3"))
    expect_equal(rep_mods[[1]]$decay, list("function" = "constant"))
    expect_equal(by_name(rep_mods[[2]]$description, "anopheles")[[1]]$availabilityReduction,
                 list(initial = "@c3@"))     # the coverage placeholder of campaign 3
    expect_equal(rep_mods[[2]]$timed$deploy$time, "2023-01-01")
})

test_that("dummy hosts: one host per campaign with its decay, in-bed populations", {
    sch = AnophelesModel:::om_schedule(sched_num, end_date)
    net = dh_net(halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 1.5))
    h = AnophelesModel:::om_dummy_hosts(dummy_rec(net), sch)
    expect_equal(unname(sapply(h$add, `[[`, "name")), c("N_dummy_1", "N_dummy_2"))
    expect_equal(h$add[[1]]$timed$deploy, list(time = "2021-01-01", lifespan = "730d"))
    expect_equal(h$add[[2]]$timed$deploy, list(time = "2022-01-01", lifespan = "365d"))
    an = by_name(h$add[[1]]$description, "anopheles")
    expect_length(an, nrow(om$shares))
    expect_equal(an[[which(!om$shares$in_bed)[1]]]$mosqRelativeAvailabilityHuman$value, "0")
    expect_equal(an[[1]]$mosqProbBiting$value, "0")             # mosquitoes die
    expect_equal(an[[1]]$hostFecundityFactor$value, "1")
    expect_equal(unname(sapply(h$modify, `[[`, "name")),
                 c("N_dummy_1_decay", "N_dummy_2_decay", "N_dummy_1_replaced_2"))
    expect_equal(h$modify[[2]]$nonHumanHostsName, "N_dummy_2")
    expect_equal(h$modify[[2]]$timed$deploy$time, "2022-01-01")
    # Attrition x linear insecticide decay: the fitted Weibull of the net, as
    # the GVI effects (the same function writes both)
    fd = net[[1]]$fitted_decay[net[[1]]$fitted_decay$component == "KillingDuringHostSeekingAbsolute", ]
    expect_equal(h$modify[[1]]$decay,
                 list("function" = "weibull", L = as.character(signif(fd$L / 365, 6)),
                      k = as.character(signif(fd$kappa, 6)), increasing = "true"))
    expect_equal(by_name(h$modify[[1]]$description, "anopheles")[[1]]$availabilityReduction,
                 list(initial = "1"))
    # Without end_date: long-lived hosts; without insecticide decay: the
    # attrition, exactly
    h2 = AnophelesModel:::om_dummy_hosts(dummy_rec(dh_net()),
                                          AnophelesModel:::om_schedule(sched_num))
    expect_equal(h2$add[[1]]$timed$deploy$lifespan, "100y")
    expect_equal(unname(sapply(h2$modify, `[[`, "name")),
                 c("N_dummy_1_decay", "N_dummy_2_decay", "N_dummy_1_replaced_2"))
    expect_equal(h2$modify[[1]]$decay,
                 list("function" = "weibull", L = "1", k = "2", increasing = "true"))
    # No decay at all: no decay modification
    h3 = AnophelesModel:::om_dummy_hosts(suppressWarnings(dummy_rec(
        dh_net(decay_attrition = "none", insecticide_decay = FALSE)[[1]])),
        AnophelesModel:::om_schedule(sched_num))
    expect_equal(unname(sapply(h3$modify, `[[`, "name")), "N_dummy_1_replaced_2")
})

test_that("schedules are validated", {
    s = AnophelesModel:::om_schedule
    expect_error(s(data.frame(date = "2021-01-01")), "columns date and coverage")
    expect_error(s(data.frame(date = c("2022-01-01", "2021-01-01"), coverage = 1)),
                 "increasing")
    expect_error(s(data.frame(date = "2021-01-01", coverage = 1.2)), "coverage")
    expect_error(s(data.frame(date = "2021-01-01", coverage = "cov")), "coverage")
    expect_error(s(sched_num, "2021-06-01"), "after the last campaign")
})

test_that("scenario values: per net and row, in-bed populations only", {
    nets = list(A = dh_net(0.6), B = dh_net(1.2))      # same decays: fixed decay allowed
    xml  = suppressMessages(get_OM_xml(om, nets, sched_ph, end_date, name = "ITN",
                                       entomology = FALSE, schema = 49))
    scens = data.frame(net = c("A", "A", "B", "B"), cov2021 = c(0.8, 0.4, 0.8, 0.4),
                       cov2022 = 0.5, seed = 1:4)
    v = om_scenario_values(scens, xml, net = "net")
    expect_equal(v[names(scens)], scens)                 # the user's columns are kept
    expect_true(all(c("ITN_deterrency", "ITN_postprandial",
                      "ITN_dummy_1_pop1", "ITN_dummy_2_pop1") %in% names(v)))
    expect_false(any(grepl("_L$|_k$", names(v))))        # fixed decay: no decay columns
    expect_false(any(grepl("preprandial", names(v))))
    expect_equal(v$ITN_deterrency, rep(0.3, 4))
    # Dummy hosts only for in-bed populations: coverage x HSK / human availability
    pops = which(om$shares$in_bed)
    expect_setequal(grep("_dummy_1_", names(v), value = TRUE),
                    paste0("ITN_dummy_1_pop", pops))
    hsk = c(0.6, 0.6, 1.2, 1.2)
    expect_equal(v$ITN_dummy_1_pop1, scens$cov2021 * hsk / alpha_h(1))
    expect_equal(v$ITN_dummy_2_pop1, 0.5 * hsk / alpha_h(1))

    # A single intervention with placeholder coverages: dummy-host columns only
    xml1 = suppressMessages(get_OM_xml(om, nets$A, sched_ph, end_date, name = "ITN",
                                       entomology = FALSE, schema = 49))
    v1 = om_scenario_values(scens[1:2, c("cov2021", "cov2022")], xml1)
    expect_equal(v1$ITN_dummy_1_pop1, v$ITN_dummy_1_pop1[1:2])
    expect_false("ITN_deterrency" %in% names(v1))
    expect_error(om_scenario_values(scens["seed"], xml1), "coverage column")
    expect_error(om_scenario_values(scens, xml), "net: give the column")
    expect_error(om_scenario_values(transform(scens, net = "Z"), xml, net = "net"),
                 "Unknown net")
    expect_error(om_scenario_values(transform(scens, h = 2), xml, net = "net",
                                    halflife = "h"), "fixed decays")
    expect_error(suppressMessages(get_OM_xml(om, list(A = nets$A, S = bi(
        halflife_attrition = 730, shape_attrition = 2)), sched_ph, end_date)), "same model")
    # Several nets with different decays need decay = "placeholder"
    expect_error(suppressMessages(get_OM_xml(om, list(A = nets$A, C = dh_net(
        halflife_insecticide = c(Deterrency = 3))), sched_ph, end_date, schema = 49)),
        "decay = \"placeholder\"")
    expect_error(suppressMessages(get_OM_xml(om, list(A = nets$A, C = dh_net(
        halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 3))), sched_ph, end_date, schema = 49)),
        "decay = \"placeholder\"")
    # Dummy hosts need schema >= 46
    expect_error(suppressMessages(get_OM_xml(om, nets$A, sched_ph)), "schema >= 46")
})

test_that("dummy-host decay as placeholders, like the other effects", {
    nets = list(A = dh_net(0.6, halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 2)),
                B = dh_net(1.2, halflife_attrition = 730,
                           halflife_insecticide = c(KillingDuringHostSeekingAbsolute = -3)))
    xml = suppressMessages(get_OM_xml(om, nets, sched_ph, name = "ITN", decay = "placeholder",
                                      entomology = FALSE, schema = 49))
    expect_match(xml$modify_hosts,
                 '<decay function="weibull" L="@ITN_hsk_L@" k="@ITN_hsk_k@" increasing="true"/>',
                 fixed = TRUE)
    expect_false(grepl("_initial@|attrition_L|_insecticide", xml$modify_hosts))
    v = om_scenario_values(data.frame(net = c("A", "B"), cov2021 = 0.8, cov2022 = 0.5),
                           xml, net = "net")
    # Each net's fitted Weibull (attrition x insecticide decay), in years
    for (i in 1:2) {
        fd = nets[[i]][[1]]$fitted_decay
        fd = fd[fd$component == "KillingDuringHostSeekingAbsolute", ]
        expect_equal(v$ITN_hsk_L[i], fd$L / 365, tolerance = 1e-5, info = i)
        expect_equal(v$ITN_hsk_k[i], fd$kappa, tolerance = 1e-5, info = i)
    }
    # Different decay functions: a Weibull for all (the step fitted), as the GVI
    mixed = list(A = dh_net(0.6), S = dh_net(0.6, decay_attrition = "step",
                                             halflife_attrition = 400))
    xm = suppressMessages(get_OM_xml(om, mixed, sched_ph, name = "ITN",
                                     decay = "placeholder", entomology = FALSE, schema = 49))
    expect_match(xm$modify_hosts, 'L="@ITN_hsk_L@" k="@ITN_hsk_k@"', fixed = TRUE)
    # Shared step: kept as a step, without shape
    steps = list(A = dh_net(0.6, decay_attrition = "step", halflife_attrition = 400),
                 B = dh_net(1.2, decay_attrition = "step", halflife_attrition = 300))
    xs = suppressMessages(get_OM_xml(om, steps, sched_ph, name = "ITN",
                                     decay = "placeholder", entomology = FALSE, schema = 49))
    expect_match(xs$modify_hosts, '<decay function="step" L="@ITN_hsk_L@" increasing="true"/>',
                 fixed = TRUE)
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
om_base = function(version = 44L) list(
    expName = "test", OMVersion = version, demography = list(), monitoring = list(),
    interventions = list(), healthSystem = list(), entomology = list(), model = list())
# baseList with the entomology of `om` (define_OM_vector_control() uses it)
om_base_ento = function(version = 44L)
    suppressMessages(define_OM_entomology(om_base(version), om, rep(1, 12)))

test_that("define / deploy a dummy-host net with numeric coverages", {
    skip_if_not_installed("openMalariaUtilities")
    net = dh_net(name = "IG2")
    expect_error(suppressMessages(define_OM_vector_control(om_base_ento(), net)),
                 "schema >= 46")
    expect_message(b <- define_OM_vector_control(om_base_ento(49L), net),
                   "added by deploy_OM_vector_control")
    # GVI without pre-prandial killing, merged (identical decays); no dummy hosts yet
    ids = sapply(by_name(b$interventions$human, "component"), `[[`, "id")
    expect_equal(unname(ids), "IG2")
    expect_null(b$interventions$addNonHumanHosts)
    expect_equal(b$anophelesmodel$vc$IG2$mode, "dummy_host")
    expect_error(suppressMessages(define_OM_vector_control(b, net)), "already")
    expect_error(define_OM_vector_control(om_base(49L), net), "define_OM_entomology")

    # Deploy: restrictions are rejected, then the dummy hosts are written
    for (args in list(list(subpop = "x"), list(cumulative = TRUE), list(minAge = 5),
                      list(maxAge = 10)))
        expect_error(do.call(deploy_OM_vector_control,
                             c(list(b, "IG2", sched_num, end_date), args)),
                     "not supported for dummy-host")
    expect_message(b <- deploy_OM_vector_control(b, "IG2", sched_num, end_date),
                   "added non-human hosts")

    # One dummy host per campaign, deployed at its date, 0 for populations not in bed
    hosts = by_name(b$interventions$addNonHumanHosts, "nonHumanHosts")
    expect_equal(unname(sapply(hosts, `[[`, "name")), c("IG2_dummy_1", "IG2_dummy_2"))
    expect_equal(hosts[[2]]$timed$deploy$time, "2022-01-01")
    an = by_name(hosts[[1]]$description, "anopheles")
    expect_equal(an[[3]]$mosqRelativeAvailabilityHuman$value, "0")          # outdoor
    expect_equal(as.numeric(an[[1]]$mosqRelativeAvailabilityHuman$value),
                 0.8 * 0.6 / alpha_h(1), tolerance = 1e-5)
    # Their decay (attrition only here: exact)
    mods = by_name(b$interventions$nonHumanHostsModifications, "intervention")
    expect_equal(unname(sapply(mods, `[[`, "nonHumanHostsName")),
                 c("IG2_dummy_1", "IG2_dummy_2", "IG2_dummy_1"))     # last: replaced by campaign 2
    expect_equal(by_name(mods[[3]]$description, "anopheles")[[1]]$availabilityReduction$initial,
                 "0.5")
    expect_equal(mods[[1]]$decay,
                 list("function" = "weibull", L = "1", k = "2", increasing = "true"))

    dep = by_name(b$interventions$human, "deployment")
    expect_length(dep, 1)
    deploys = by_name(dep[[1]]$timed, "deploy")
    expect_equal(unname(sapply(deploys, `[[`, "coverage")), c("0.8", "0.5"))
    # A dummy-host intervention is deployed once, with the whole schedule
    expect_error(deploy_OM_vector_control(b, "IG2", sched_num, end_date), "already deployed")
    expect_error(deploy_OM_vector_control(b, "other", sched_num), "define_OM_vector_control")
    # end_date is optional (long-lived hosts), any schedule length
    b2 = suppressMessages(define_OM_vector_control(om_base_ento(49L), dh_net(name = "S")))
    b2 = suppressMessages(deploy_OM_vector_control(b2, "S", sched_num))
    expect_equal(by_name(b2$interventions$addNonHumanHosts, "nonHumanHosts")[[1]]$timed$deploy$lifespan,
                 "100y")
})

test_that("components defined after a deploy are written before the deployments", {
    skip_if_not_installed("openMalariaUtilities")
    # define A, deploy A, define B, deploy B: in <human>, all <component>s must
    # precede the <deployment>s (xs:sequence), else the XML is invalid
    b = om_base_ento(49L)
    for (nm in c("A", "B")) {
        b = suppressMessages(define_OM_vector_control(b, dh_net(name = nm)))
        b = suppressMessages(deploy_OM_vector_control(b, nm, sched_num, end_date))
    }
    human = b$interventions$human
    expect_equal(names(human), c("component", "component", "deployment", "deployment"))
    expect_equal(unname(sapply(by_name(human, "component"), `[[`, "id")), c("A", "B"))
    expect_equal(unname(sapply(by_name(human, "deployment"), `[[`, "name")),
                 c("A", "B"))
    # The same through add_OM_GVI() after a deploy (a standard net: the GVI
    # of a dummy-host net warns that it lacks host-seeking killing)
    gvi = suppressMessages(get_OM_GVI_snippet(
        bi(halflife_attrition = 730, shape_attrition = 2), om, "ITN"))
    b = suppressMessages(add_OM_GVI(b, gvi))
    human = b$interventions$human
    n_comp = sum(names(human) == "component")
    expect_gt(n_comp, 2)
    expect_equal(names(human), c(rep("component", n_comp), rep("deployment", 2)))
})

test_that("define / deploy a template of nets: one deployment with all components", {
    skip_if_not_installed("openMalariaUtilities")
    nets = list(A = dh_net(0.6, halflife_insecticide = c(Deterrency = 3)), B = dh_net(1.2))
    b = suppressMessages(define_OM_vector_control(om_base_ento(49L), nets, name = "ITN",
                                                  decay = "placeholder"))
    comps = by_name(b$interventions$human, "component")
    expect_equal(unname(sapply(comps, `[[`, "id")),
                 c("ITN-deterrency", "ITN-postprandialKillingEffect"))
    expect_equal(comps[[1]]$GVI$decay,
                 list(L = "@ITN_deterrency_L@", "function" = "weibull", k = "@ITN_deterrency_k@"))

    b = suppressMessages(deploy_OM_vector_control(b, "ITN", sched_ph, end_date))
    an = by_name(by_name(b$interventions$addNonHumanHosts, "nonHumanHosts")[[2]]$description,
                 "anopheles")
    expect_equal(an[[1]]$mosqRelativeAvailabilityHuman$value, "@ITN_dummy_2_pop1@")
    dep = by_name(b$interventions$human, "deployment")[[1]]
    expect_equal(unname(sapply(by_name(dep, "component"), `[[`, "id")),
                 c("ITN-deterrency", "ITN-postprandialKillingEffect"))
    expect_equal(unname(sapply(by_name(dep$timed, "deploy"), `[[`, "coverage")),
                 c("@cov2021@", "@cov2022@"))
    # Every placeholder of the interventions has a column: the user's coverages
    # and the values filled by om_scenario_values() (the entomology has its
    # own: EIR, contributions, seasonality)
    scens = data.frame(net = c("A", "B"), cov2021 = 0.8, cov2022 = 0.5)
    v = suppressMessages(om_scenario_values(scens, b, "ITN", net = "net"))
    strings = unlist(b$interventions)
    all_ph = unique(gsub("@", "", unlist(regmatches(strings, gregexpr("@[^@]+@", strings)))))
    expect_setequal(all_ph, setdiff(names(v), "net"))
})

test_that("standard model: pre-prandial component and no host-seeking killing", {
    skip_if_not_installed("openMalariaUtilities")
    b = suppressMessages(define_OM_vector_control(
        om_base_ento(), list(A = bi(halflife_attrition = 730, shape_attrition = 2),
                             B = bi(halflife_attrition = 365, shape_attrition = 1.5)),
        name = "ITN", decay = "placeholder"))
    expect_true("ITN-preprandialKillingEffect" %in%
                sapply(by_name(b$interventions$human, "component"), `[[`, "id"))
    expect_null(b$interventions$addNonHumanHosts)

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
to_xml = AnophelesModel:::om_list_to_xml

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
    expect_null(xml$add_hosts)
    expect_null(xml$modify_hosts)
    # One merged component (identical decays), deployed once per campaign
    expect_equal(lengths(regmatches(xml$define, gregexpr("<component ", xml$define))), 1)
    expect_match(xml$define, '<component id="ITN" name="ITN">', fixed = TRUE)
    expect_match(xml$define, '<decay L="2" function="weibull" k="2"/>', fixed = TRUE)
    expect_match(xml$deploy, '<deployment name="ITN">', fixed = TRUE)
    expect_match(xml$deploy, '<component id="ITN"/>', fixed = TRUE)
    expect_match(xml$deploy, '<deploy coverage="0.8" time="2021-01-01"/>', fixed = TRUE)
    expect_match(xml$deploy, '<deploy coverage="0.5" time="2022-01-01"/>', fixed = TRUE)
    expect_output(print(xml), "after all <component>")

    sub = get_OM_xml(om, itn, sched_num, end_date, subpop = "eligible", minAge = 5,
                     maxAge = 15)
    expect_match(sub$deploy, '<restrictToSubPop id="ITN-eligible"/>', fixed = TRUE)
    expect_match(sub$deploy, 'minAge="5" maxAge="15"', fixed = TRUE)
    ph = get_OM_xml(om, itn, sched_num, minAge = 0, maxAge = "@maxAge@", entomology = FALSE)
    expect_match(ph$deploy, 'minAge="0" maxAge="@maxAge@"', fixed = TRUE)
    # Age limits: both or neither, valid values
    expect_error(get_OM_xml(om, itn, sched_num, minAge = 5), "both minAge and maxAge")
    expect_error(get_OM_xml(om, itn, sched_num, maxAge = 5), "both minAge and maxAge")
    expect_error(get_OM_xml(om, itn, sched_num, minAge = 5, maxAge = 5), "smaller")
    expect_error(get_OM_xml(om, itn, sched_num, minAge = -1, maxAge = 5), "minAge must")
    expect_error(get_OM_xml(om, itn, sched_num, minAge = 0, maxAge = "five"), "maxAge must")
})

test_that("dummy-host intervention: no pre-prandial killing, dummy-host snippets", {
    xml = suppressMessages(get_OM_xml(om, dh_net(name = "IG2"), sched_ph, end_date,
                                      schema = 49))
    expect_match(xml$define, '<preprandialKillingEffect value="0"/>', fixed = TRUE)
    expect_match(xml$add_hosts, '<nonHumanHosts name="IG2_dummy_2">', fixed = TRUE)
    expect_match(xml$add_hosts, '<mosqRelativeAvailabilityHuman value="@IG2_dummy_1_pop1@"/>',
                 fixed = TRUE)
    expect_match(xml$add_hosts, '<deploy time="2022-01-01" lifespan="365d"/>', fixed = TRUE)
    expect_match(xml$modify_hosts,
                 '<intervention name="IG2_dummy_1_decay" nonHumanHostsName="IG2_dummy_1">',
                 fixed = TRUE)
    expect_match(xml$modify_hosts,
                 '<decay function="weibull" L="1" k="2" increasing="true"/>', fixed = TRUE)
    expect_match(xml$deploy, 'coverage="@cov2021@"', fixed = TRUE)
    expect_output(print(xml), "<addNonHumanHosts>")
    expect_error(suppressMessages(get_OM_xml(om, dh_net(), sched_ph, end_date, schema = 49,
                                             subpop = "x")), "not supported")
    expect_message(get_OM_xml(om, dh_net(), sched_num, end_date, schema = 49),
                   "added non-human hosts")
    expect_error(suppressMessages(get_OM_xml(om, dh_net(), sched_num, schema = 45)),
                 "schema >= 46")
})

test_that("om_scenario_values fills every placeholder of the snippets", {
    nets = list(A = dh_net(0.6, halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 2)),
                B = dh_net(1.2, halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 2)))
    scens = data.frame(net = c("A", "B"), cov2021 = 0.8, cov2022 = 0.5)
    for (decay in c("fixed", "placeholder")) {
        # Entomology placeholders (EIR, seasonality) are not intervention values
        xml = suppressMessages(get_OM_xml(om, nets, sched_ph, end_date, name = "ITN",
                                          entomology = FALSE, decay = decay, schema = 49))
        text = paste(unlist(xml), collapse = "\n")
        ph = unique(gsub("@", "", regmatches(text, gregexpr("@[^@]+@", text))[[1]]))
        v = suppressMessages(om_scenario_values(scens, xml, net = "net"))
        expect_setequal(ph, setdiff(names(v), "net"))
        expect_equal(any(grepl("_L$", names(v))), decay == "placeholder", info = decay)
    }
    # Nothing varies: no placeholders to fill
    xml = suppressMessages(get_OM_xml(om, nets$A, sched_num, end_date, entomology = FALSE,
                                      schema = 49))
    expect_false(grepl("@", paste(unlist(xml), collapse = "")))
    expect_message(om_scenario_values(scens, xml), "no placeholders")
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
    expect_no_error(get_OM_xml(om, bi(halflife_attrition = 730, shape_attrition = 2),
                               sched_num, entomology = FALSE))
    # Schema 49: no propInfected / propInfectious, otherwise the same
    e49 = get_OM_xml(om, schema = 49)$entomology
    expect_false(grepl("propInf", e49))
    expect_match(e49, paste0('<anopheles mosquito="', om$shares$mosquito[1], '">'), fixed = TRUE)
    expect_equal(gsub(' propInfected="0.078" propInfectious="0.021"', "", e), e49)
    expect_error(get_OM_xml(om, schema = 40), "schema")
})

test_that("snippets are well-formed XML", {
    skip_if_not_installed("xml2")
    xml = suppressMessages(get_OM_xml(om, dh_net(name = "IG2"), sched_num, end_date,
                                      schema = 49))
    expect_no_error(xml2::read_xml(xml$entomology))
    expect_no_error(xml2::read_xml(paste0("<human>", xml$define, xml$deploy, "</human>")))
    cont = get_OM_xml(om, bi(halflife_attrition = 730, shape_attrition = 2), entomology = FALSE,
                      om_continuous(c(0.25, 6), 0.8, begin = "2021-01-01"))
    expect_no_error(xml2::read_xml(paste0("<human>", cont$define, cont$deploy, "</human>")))
    expect_no_error(xml2::read_xml(paste0("<addNonHumanHosts>", xml$add_hosts,
                                          "</addNonHumanHosts>")))
    expect_no_error(xml2::read_xml(paste0("<nonHumanHostsModifications>", xml$modify_hosts,
                                          "</nonHumanHostsModifications>")))
})

test_that("the OMU entomology matches the text one, custom animal hosts included", {
    skip_if_not_installed("openMalariaUtilities")
    # Deliberately distinct animal host parameters (defineEntomology() alone
    # writes 0.95 / 0.95 / 0.99)
    ht = as.data.frame(def_host_params(verbose = FALSE), stringsAsFactors = FALSE)
    ht[ht$host == "animal", c("PBi", "PCi", "PDi")] = list(0.71, 0.72, 0.73)
    om_c = suppressMessages(get_OM_ento_snippet(
        gss, activity = activity, hosts_p = def_host_params(ht, verbose = FALSE)))
    # Numeric seasonality: defineEntomology() rejects monthly placeholders
    # with R >= 4.3 (an openMalariaUtilities issue)
    om_num_seas = om_c
    om_num_seas$seasonalityParameters = lapply(om_c$seasonalityParameters, function(s) {
        s$seasonality = rep(1, 12); s })
    raw = openMalariaUtilities::defineEntomology(
        om_base(), om_num_seas$seasonalityParameters, om_num_seas$mosquitoParameters)
    expect_false(isTRUE(by_name(raw$entomology$vector, "anopheles")[[1]]$nonHumanHosts$
                            mosqProbBiting$value == "0.71"))      # OMU's fixed values
    for (version in c(44L, 49L)) {
        b = suppressMessages(define_OM_entomology(om_base(version), om_c, rep(1, 12)))
        omu  = b$entomology$vector
        ours = AnophelesModel:::om_vector_list(om_num_seas, version)
        expect_equal(names(ours), names(omu))
        for (i in seq_along(ours))
            expect_equal(to_xml(names(ours)[i], ours[[i]]), to_xml(names(omu)[i], omu[[i]]),
                         info = paste(version, i))
        nhh = by_name(omu, "anopheles")[[1]]$nonHumanHosts
        expect_equal(c(nhh$mosqProbBiting$value, nhh$mosqProbFindRestSite$value,
                       nhh$mosqProbResting$value), c("0.71", "0.72", "0.73"))
    }
    # The models of the populations use the same animal values
    expect_true(0.71 %in% om_c$model_params[[1]]$host_params$PBi)
})

test_that("text snippets match what define / deploy write into a baseList", {
    skip_if_not_installed("openMalariaUtilities")
    for (net in list(dh_net(name = "IG2"),
                     bi(halflife_attrition = 730, shape_attrition = 2, name = "ITN",
                        halflife_insecticide = c(Deterrency = 4)))) {
        nm  = net[[1]]$description
        xml = suppressMessages(get_OM_xml(om, net, sched_num, end_date, schema = 49))
        b = suppressMessages(define_OM_vector_control(om_base_ento(49L), net))
        b = suppressMessages(deploy_OM_vector_control(b, nm, sched_num, end_date))
        human = b$interventions$human
        text_of = function(l, tag) if (length(l))
            paste(unlist(lapply(l, function(x) to_xml(tag, x))), collapse = "\n")
        expect_equal(xml$define, text_of(by_name(human, "component"), "component"), info = nm)
        expect_equal(xml$deploy, text_of(by_name(human, "deployment"), "deployment"), info = nm)
        expect_equal(xml$add_hosts, text_of(b$interventions$addNonHumanHosts, "nonHumanHosts"),
                     info = nm)
        expect_equal(xml$modify_hosts,
                     text_of(b$interventions$nonHumanHostsModifications, "intervention"),
                     info = nm)
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

test_that("dummy hosts of a specification and of an intervention object agree", {
    spec = om_vector_control(c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.6, PostPrandial = 0.4),
                             halflife_attrition = 365, shape_attrition = 2,
                             halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 2))
    long = data.frame(date = c("2021-01-01", "2025-01-01"), coverage = c("@c1@", "@c2@"))
    cov  = data.frame(c1 = c(0.8, 0.5), c2 = 0.6)
    xml_of = function(x) suppressMessages(get_OM_xml(om, x, long, "2030-01-01", name = "N",
                                                     entomology = FALSE, schema = 49))
    # Same fitting window (3 years for ITNs): the decay of host-seeking
    # killing is fitted over it, as for the GVI effects
    legacy = suppressMessages(build_intervention(spec$efficacy, halflife_attrition = 365,
        shape_attrition = 2, halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 2),
        duration_years = 3, model_p = model_p))
    expect_equal(xml_of(spec)$add_hosts, xml_of(legacy)$add_hosts)
    expect_equal(xml_of(spec)$modify_hosts, xml_of(legacy)$modify_hosts)
    expect_equal(om_scenario_values(cov, xml_of(spec)), om_scenario_values(cov, xml_of(legacy)))
})

test_that("define_OM_entomology keeps the entomology used by the interventions", {
    skip_if_not_installed("openMalariaUtilities")
    b = om_base_ento()
    expect_identical(b$anophelesmodel$ento, om)
    an = by_name(b$entomology$vector, "anopheles")
    expect_equal(unname(sapply(an, `[[`, "mosquito")), om$shares$mosquito)
    expect_equal(b$entomology$scaledAnnualEIR, "@EIR@")
    # One seasonality per population
    seas = setNames(lapply(seq_along(om$shares$mosquito), function(j) rep(j, 12)),
                    om$shares$mosquito)
    b2 = suppressMessages(define_OM_entomology(om_base(), om, seas, scaledAnnualEIR = 20))
    an2 = by_name(b2$entomology$vector, "anopheles")
    expect_equal(an2[[2]]$seasonality$monthlyValues[[2]][[1]], 2)
    expect_error(define_OM_entomology(om_base(), om, seas[1]), "one element per population")
    # Placeholder seasonality (default @m1@..@m12@): written by a recent
    # openMalariaUtilities, a clear error with older versions (R >= 4.3)
    res = tryCatch(suppressMessages(define_OM_entomology(om_base(), om)),
                   error = function(e) e)
    if (inherits(res, "error")) {
        expect_match(conditionMessage(res), "older versions reject placeholders")
    } else {
        expect_true(all(paste0("@m", 1:12, "@") %in% unlist(res$entomology$vector)))
    }
    # Invalid seasonalities, checked before openMalariaUtilities
    for (bad in list(c(paste0("@m", 1:11, "@"), "m12"), rep(-1, 12), rep(1, 11),
                     paste0("@m", 1:11, "@")))
        expect_error(define_OM_entomology(om_base(), om, bad), "seasonality must be")
    expect_false(is.null(an[[1]]$propInfected))
    # Schema 49: without propInfected / propInfectious
    an49 = by_name(om_base_ento(49L)$entomology$vector, "anopheles")
    expect_null(an49[[1]]$propInfected)
    expect_null(an49[[1]]$propInfectious)

    # The whole route: entomology, then interventions without a model
    ig2 = om_vector_control(get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                                           verbose = FALSE),
                            halflife_attrition = 730, shape_attrition = 2)
    b = suppressMessages(define_OM_vector_control(om_base_ento(49L), ig2, name = "IG2"))
    b = suppressMessages(deploy_OM_vector_control(b, "IG2", sched_num, "2027-01-01"))
    expect_length(by_name(b$interventions$addNonHumanHosts, "nonHumanHosts"), 2)
    # One decay per host, one replacement
    expect_length(by_name(b$interventions$nonHumanHostsModifications, "intervention"), 3)
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

test_that("the end_date of a schedule is used everywhere, and must agree with an argument", {
    spec = om_vector_control(c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.6, PostPrandial = 0.4),
                             halflife_attrition = 365, shape_attrition = 2)
    s_end = om_campaigns(dates = sched_ph$date, coverage = sched_ph$coverage,
                         end_date = end_date)
    expect_equal(attr(s_end, "end_date"), end_date)
    cov = data.frame(cov2021 = 0.8, cov2022 = 0.5)
    x_of = function(...) suppressMessages(get_OM_xml(om, spec, ..., entomology = FALSE,
                                                     schema = 49))
    x_arg = x_of(sched_ph, end_date)
    x_end = x_of(s_end)
    expect_equal(x_end$add_hosts, x_arg$add_hosts)
    expect_match(x_end$add_hosts, 'lifespan="730d"', fixed = TRUE)
    expect_equal(om_scenario_values(cov, x_end), om_scenario_values(cov, x_arg))
    expect_error(x_of(s_end, "2024-01-01"), "differs")
    expect_match(x_of(sched_ph)$add_hosts, 'lifespan="100y"', fixed = TRUE)   # optional
    expect_error(om_campaigns(dates = sched_ph$date, coverage = 1, end_date = "2021-06-01"),
                 "after the last campaign")
    # Standard interventions do not need it
    expect_no_error(get_OM_xml(om, list(a = om_vector_control(eff, 730, 2),
                                        b = om_vector_control(eff, 365, 2)),
                               sched_ph, decay = "placeholder"))
})

test_that("continuous deployment at target ages", {
    itn  = bi(halflife_attrition = 730, shape_attrition = 2, name = "ITN")
    cont = om_continuous(age = c(0.25, 6), coverage = c(0.8, "@schoolcov@"),
                         begin = "2021-01-01")
    expect_s3_class(cont, "om_continuous")
    xml = get_OM_xml(om, itn, cont, entomology = FALSE)
    expect_match(xml$deploy, "<continuous>", fixed = TRUE)
    expect_false(grepl("<timed>", xml$deploy))
    expect_match(xml$deploy, '<deploy coverage="0.8" targetAgeYrs="0.25" begin="2021-01-01"/>',
                 fixed = TRUE)
    expect_match(xml$deploy, '<deploy coverage="@schoolcov@" targetAgeYrs="6" begin="2021-01-01"/>',
                 fixed = TRUE)
    expect_match(xml$deploy, '<component id="ITN"/>', fixed = TRUE)
    # Without dates: always active (no begin / end attributes)
    xml0 = get_OM_xml(om, itn, om_continuous(0.25, 0.7), entomology = FALSE)
    expect_match(xml0$deploy, '<deploy coverage="0.7" targetAgeYrs="0.25"/>', fixed = TRUE)
    # Sub-population, before the deploys
    sub = get_OM_xml(om, itn, cont, subpop = "eligible", entomology = FALSE)
    expect_match(sub$deploy, '<continuous>\\s*<restrictToSubPop id="ITN-eligible"/>')
    # Campaign options do not apply; dummy host not supported
    expect_error(get_OM_xml(om, itn, cont, cumulative = TRUE), "for campaigns")
    expect_error(get_OM_xml(om, itn, cont, minAge = 0, maxAge = 5), "for campaigns")
    expect_error(get_OM_xml(om, itn, cont, end_date = "2030-01-01"), "end_date is for campaigns")
    expect_error(suppressMessages(get_OM_xml(om, dh_net(), cont, schema = 49)),
                 "not supported for dummy-host")
    # Validation
    expect_error(om_continuous(age = -1, coverage = 0.5), "age must")
    expect_error(om_continuous(age = 120, coverage = 0.5), "age must")
    expect_error(om_continuous(age = 1, coverage = 1.5), "coverage must")
    expect_error(om_continuous(age = c(1, 2), coverage = c(0.5, 0.6, 0.7)), "one per target age")
    expect_error(om_continuous(age = 1, coverage = 0.5, begin = "2021-13-01"), "begin must")
    expect_error(om_continuous(age = 1, coverage = 0.5, begin = "2025-01-01",
                               end = "2021-01-01"), "after begin")
    expect_error(om_continuous(age = numeric(0), coverage = 0.5), "at least one")
})

test_that("continuous and campaign deployments of the same net", {
    skip_if_not_installed("openMalariaUtilities")
    itn = bi(halflife_attrition = 730, shape_attrition = 2, name = "ITN")
    b = suppressMessages(define_OM_vector_control(om_base_ento(), itn, name = "ITN"))
    expect_message(b <- deploy_OM_vector_control(
        b, "ITN", om_continuous(0.25, "@epicov@", begin = "2021-01-01")),
        "continuous, at target ages 0.25")
    b = suppressMessages(deploy_OM_vector_control(b, "ITN", sched_num))
    dep = by_name(b$interventions$human, "deployment")
    expect_length(dep, 2)
    expect_equal(dep[[1]]$continuous$deploy,
                 list(coverage = "@epicov@", targetAgeYrs = "0.25", begin = "2021-01-01"))
    expect_length(by_name(dep[[2]]$timed, "deploy"), 2)
    # Same lists as the text route
    xml = get_OM_xml(om, itn, om_continuous(0.25, "@epicov@", begin = "2021-01-01"),
                     name = "ITN", entomology = FALSE)
    expect_equal(xml$deploy, paste(to_xml("deployment", dep[[1]]), collapse = "\n"))
})

test_that("a placeholder repeated over campaigns gives one coverage column", {
    spec = om_vector_control(c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.6, PostPrandial = 0.4),
                             halflife_attrition = 365, shape_attrition = 2)
    same = om_campaigns("2021-01-01", "2023-01-01", coverage = "@cont@")
    xml_of = function(s) suppressMessages(get_OM_xml(om, spec, s, "2024-01-01", name = "N",
                                                     entomology = FALSE, schema = 49))
    v = om_scenario_values(data.frame(cont = c(0.5, 0.8)), xml_of(same))
    # Same values as three placeholders with the same coverages
    three = om_campaigns(dates = same$date, coverage = c("@a@", "@b@", "@c@"))
    v3 = om_scenario_values(data.frame(a = c(0.5, 0.8), b = c(0.5, 0.8), c = c(0.5, 0.8)),
                            xml_of(three))
    dummy = grep("_dummy_", names(v), value = TRUE)
    expect_length(dummy, 3 * sum(om$shares$in_bed))
    expect_equal(v[dummy], v3[dummy])
})

test_that("a list keeps the decay function shared by its nets (e.g. a step)", {
    p = function(eht) get_ITN_params("Interceptor G2", eht, verbose = FALSE)
    hist = list(
        martin  = om_vector_control(p("Martin"),  halflife_attrition = 365,
                                    decay_attrition = "step", insecticide_decay = FALSE),
        assenga = om_vector_control(p("Assenga"), halflife_attrition = 365,
                                    decay_attrition = "step", insecticide_decay = FALSE))
    comps = function(x, decay) suppressMessages(AnophelesModel:::om_vc_components(
        x, om, "ITN", "histITN", decay))$components
    # Same decay for both nets: fixed values, or placeholders for its parameters
    expect_equal(comps(hist, "fixed")[[1]]$GVI$decay, list(L = "1", "function" = "step"))
    expect_equal(comps(hist, "placeholder")[[1]]$GVI$decay,
                 list(L = "@histITN_deterrency_L@", "function" = "step"))   # no k
    sched = om_campaigns("2015-01-25", "2018-01-25", coverage = "@histITNcov@")
    xml = suppressMessages(get_OM_xml(om, hist, sched, name = "histITN",
                                      decay = "placeholder", entomology = FALSE))
    v = om_scenario_values(data.frame(net = c("martin", "assenga"), histITNcov = 0.6),
                           xml, net = "net")
    expect_equal(v$histITN_deterrency_L, c(1, 1))      # 365 days, in years
    expect_false("histITN_deterrency_k" %in% names(v))
    # Different decay functions: Weibull for all (step fitted)
    mixed = list(step = hist$martin,
                 weib = om_vector_control(p("Martin"), halflife_attrition = 730,
                                          shape_attrition = 2, insecticide_decay = FALSE))
    expect_equal(comps(mixed, "placeholder")[[1]]$GVI$decay[["function"]], "weibull")
    expect_error(comps(mixed, "fixed"), "different")
})

test_that("om_scenario_values: attrition from the table, combined per effect", {
    hl = c(Deterrency = 4, PrePrandial = 6)              # insecticide decay differs per effect
    spec = function(days) om_vector_control(eff, halflife_attrition = days,
                                            shape_attrition = 2, halflife_insecticide = hl)
    xml = suppressMessages(get_OM_xml(om, spec(365), sched_num, name = "N",
                                      decay = "placeholder", entomology = FALSE))
    scens = data.frame(att = c(2, 3, 2), k = 2)
    v = suppressMessages(om_scenario_values(scens, xml, halflife = "att", kappa = "k"))
    # Per effect: the attrition of the row x the effect's insecticide decay, fitted
    built = AnophelesModel:::om_build_spec(spec(730), model_p, 3)
    for (comp in c("Deterrency", "PrePrandial", "PostPrandial")) {
        w = suppressMessages(AnophelesModel:::om_template_decay_values(built, comp, "weibull"))
        lab = paste0("N_", c(Deterrency = "deterrency", PrePrandial = "preprandial",
                             PostPrandial = "postprandial")[[comp]])
        expect_equal(v[[paste0(lab, "_L")]][1], unname(w["L"]), info = comp)
        expect_equal(v[[paste0(lab, "_k")]][1], unname(w["k"]), info = comp)
    }
    expect_false(isTRUE(all.equal(v$N_deterrency_L, v$N_preprandial_L)))   # differ per effect
    expect_equal(unname(unlist(v[1, ])), unname(unlist(v[3, ])))           # same row values
    expect_gt(v$N_postprandial_L[2], v$N_postprandial_L[1])               # longer attrition
    # Days give the same as years
    v_days = suppressMessages(om_scenario_values(transform(scens, att = att * 365), xml,
                                                 halflife = "att", kappa = "k",
                                                 decay_unit = "days"))
    expect_equal(v_days[-1], v[-1])
    # One attrition column only, and specifications are needed
    expect_error(om_scenario_values(scens, xml, halflife = c(Deterrency = "att",
                                                             PrePrandial = "att")),
                 "one attrition column")
    legacy = suppressMessages(get_OM_xml(om, bi(halflife_attrition = 365, shape_attrition = 2),
                                         sched_num, decay = "placeholder", entomology = FALSE))
    expect_error(om_scenario_values(scens, legacy, halflife = "att"), "om_vector_control")
    expect_error(om_scenario_values(scens, xml, halflife = "nope"), "no column nope")
})

test_that("om_scenario_values: final decays per effect, written as given", {
    xml = suppressMessages(get_OM_xml(om, om_vector_control(eff, 365, 2), sched_num, name = "N",
                                      decay = "placeholder", entomology = FALSE))
    scens = data.frame(hl_det = 1, hl_pre = 2, hl_post = 3, k = 1.5)
    v = om_scenario_values(scens, xml, combine = FALSE, kappa = "k",
        halflife = c(Deterrency = "hl_det", PrePrandial = "hl_pre", PostPrandial = "hl_post"))
    expect_equal(c(v$N_deterrency_L, v$N_preprandial_L, v$N_postprandial_L), c(1, 2, 3))
    expect_equal(v$N_deterrency_k, 1.5)
    # One column for all effects
    v1 = om_scenario_values(scens, xml, combine = FALSE, halflife = "hl_pre", kappa = "k")
    expect_equal(c(v1$N_deterrency_L, v1$N_postprandial_L), c(2, 2))
    expect_error(om_scenario_values(scens, xml, combine = FALSE,
                                    halflife = c(Typo = "hl_det")), "Unknown effect")

    # Host-seeking killing: an effect like the others (the dummy hosts' decay)
    dh = function(days = 365, k = 2)
        om_vector_control(c(Deterrency = 0.3, KillingDuringHostSeekingAbsolute = 0.6, PostPrandial = 0.4),
                          halflife_attrition = days, shape_attrition = k,
                          halflife_insecticide = c(KillingDuringHostSeekingAbsolute = 2))
    xml_dh = suppressMessages(get_OM_xml(om, dh(), sched_ph, end_date, name = "D",
                                         decay = "placeholder", entomology = FALSE,
                                         schema = 49))
    s = data.frame(cov2021 = 0.8, cov2022 = 0.5, hl = 1.5, k = 3)
    v_final = om_scenario_values(s, xml_dh, combine = FALSE, halflife = "hl", kappa = "k")
    expect_equal(c(v_final$D_hsk_L, v_final$D_hsk_k), c(1.5, 3))
    expect_equal(c(v_final$D_deterrency_L, v_final$D_deterrency_k), c(1.5, 3))
    expect_equal(v_final$D_dummy_1_pop1, 0.8 * 0.6 / alpha_h(1))
    # Per effect: host-seeking killing alone
    v_hsk = om_scenario_values(s, xml_dh, combine = FALSE,
                               halflife = c(KillingDuringHostSeekingAbsolute = "hl"))
    expect_equal(v_hsk$D_hsk_L, 1.5)
    expect_false(isTRUE(all.equal(v_hsk$D_deterrency_L, 1.5)))
    # Half-life only: the shape of the net's exported (fitted) decay is kept
    built = function(days, k) AnophelesModel:::om_build_spec(dh(days, k), model_p, 3)
    w_net = suppressMessages(AnophelesModel:::om_template_decay_values(
        built(365, 2), "KillingDuringHostSeekingAbsolute", "weibull"))
    v_hl = suppressMessages(om_scenario_values(s, xml_dh, combine = FALSE, halflife = "hl"))
    expect_equal(c(v_hl$D_hsk_L, v_hl$D_hsk_k), c(1.5, unname(w_net["k"])))
    # combine = TRUE: the row's attrition x the insecticide decay of
    # host-seeking killing, fitted, as for the GVI effects
    v_comb = suppressMessages(om_scenario_values(s, xml_dh, halflife = "hl", kappa = "k"))
    w = suppressMessages(AnophelesModel:::om_template_decay_values(
        built(1.5 * 365, 3), "KillingDuringHostSeekingAbsolute", "weibull"))
    expect_equal(c(v_comb$D_hsk_L, v_comb$D_hsk_k), unname(w[c("L", "k")]))
    expect_lt(v_comb$D_hsk_L, 1.5)                    # faster than the attrition alone
    expect_equal(c(v_comb$D_deterrency_L, v_comb$D_deterrency_k), c(1.5, 3))
    expect_equal(v_comb$D_dummy_2_pop1, v_final$D_dummy_2_pop1)
})

test_that("om_scenario_values: attrition half-life and shape are replaced independently", {
    spec = om_vector_control(eff, halflife_attrition = 730, shape_attrition = 2)
    xml = suppressMessages(get_OM_xml(om, spec, sched_num, name = "N", decay = "placeholder",
                                      entomology = FALSE))
    # Shape only: the half-life (2 years) is kept, shapes 1 and 3 give different decays
    v = suppressMessages(om_scenario_values(data.frame(k = c(1, 3)), xml, kappa = "k"))
    expect_equal(v$N_deterrency_k, c(1, 3))
    expect_equal(v$N_deterrency_L, c(2, 2))
    expect_false(isTRUE(all.equal(v[1, ], v[2, ], check.attributes = FALSE)))
    # Half-life only: the shape is kept
    v2 = suppressMessages(om_scenario_values(data.frame(h = c(1, 3)), xml, halflife = "h"))
    expect_equal(v2$N_deterrency_L, c(1, 3))
    expect_equal(v2$N_deterrency_k, c(2, 2))
    # Invalid values name the rows
    expect_error(om_scenario_values(data.frame(k = c(1, -1)), xml, kappa = "k"),
                 "Column k: rows 2 must be positive")
    expect_error(om_scenario_values(data.frame(h = c(NA, 1)), xml, halflife = "h"),
                 "Column h: rows 1 must be positive")
    # A shape override also needs specifications
    legacy = suppressMessages(get_OM_xml(om, bi(halflife_attrition = 730, shape_attrition = 2),
                                         sched_num, decay = "placeholder", entomology = FALSE))
    expect_error(om_scenario_values(data.frame(k = 1), legacy, kappa = "k"), "om_vector_control")
})

test_that("om_scenario_values: final decays replace only the supplied parameters", {
    spec = om_vector_control(eff, halflife_attrition = 730, shape_attrition = 2,
                             halflife_insecticide = c(Deterrency = 4))
    xml = suppressMessages(get_OM_xml(om, spec, sched_num, name = "N", decay = "placeholder",
                                      entomology = FALSE))
    own = suppressMessages(om_scenario_values(data.frame(x = 1), xml))
    # Half-life of one effect: its shape kept, the other effects their own decays
    v = suppressMessages(om_scenario_values(data.frame(h = 1.5), xml, combine = FALSE,
                                            halflife = c(PrePrandial = "h")))
    expect_equal(c(v$N_preprandial_L, v$N_preprandial_k), c(1.5, 2))
    expect_equal(c(v$N_deterrency_L, v$N_deterrency_k), c(own$N_deterrency_L, own$N_deterrency_k))
    # Shape of one effect: its half-life kept
    v = suppressMessages(om_scenario_values(data.frame(k = 3), xml, combine = FALSE,
                                            kappa = c(PostPrandial = "k")))
    expect_equal(c(v$N_postprandial_L, v$N_postprandial_k), c(2, 3))
    # Days and years give the same
    vd = suppressMessages(om_scenario_values(data.frame(h = 1.5 * 365), xml, combine = FALSE,
                                             halflife = c(PrePrandial = "h"), decay_unit = "days"))
    expect_equal(vd$N_preprandial_L, 1.5)
    # Invalid values: rows and effect named
    expect_error(suppressMessages(om_scenario_values(
        data.frame(h = c(1, 0)), xml, combine = FALSE, halflife = c(PrePrandial = "h"))),
        "rows 2, PrePrandial: the half-life")
    expect_error(suppressMessages(om_scenario_values(
        data.frame(k = -1), xml, combine = FALSE, kappa = "k")),
        "rows 1, Deterrency: the shape")
    expect_error(suppressMessages(om_scenario_values(
        data.frame(h = NA), xml, combine = FALSE, halflife = c(PostPrandial = "h"))),
        "rows 1, PostPrandial: the half-life")
})

test_that("om_scenario_values rejects decay columns that would have no effect", {
    # Fixed decays, nothing else to fill: still an error (was a message)
    one = suppressMessages(get_OM_xml(om, om_vector_control(eff, 730, 2), sched_num,
                                      name = "N", entomology = FALSE))
    expect_error(om_scenario_values(data.frame(h = 2), one, halflife = "h"), "fixed decays")
    # Attrition columns on a net without attrition, kappa on a shape-less one
    irs = om_vector_control(eff, decay_attrition = "none",
                            halflife_insecticide = c(Deterrency = 1))
    x_irs = suppressWarnings(suppressMessages(get_OM_xml(
        om, irs, sched_num, name = "I", intervention_type = "IRS",
        decay = "placeholder", entomology = FALSE)))
    expect_error(om_scenario_values(data.frame(h = 2), x_irs, halflife = "h"),
                 "has no attrition")
    step = om_vector_control(eff, halflife_attrition = 365, decay_attrition = "step")
    x_step = suppressMessages(get_OM_xml(om, step, sched_num, name = "S",
                                         decay = "placeholder", entomology = FALSE))
    expect_error(om_scenario_values(data.frame(k = 2), x_step, kappa = "k"),
                 "has no shape parameter")
    expect_no_error(suppressMessages(om_scenario_values(data.frame(h = 2), x_step,
                                                        halflife = "h")))
    # Factor columns are read as their values, not their level codes
    spec = om_vector_control(eff, halflife_attrition = 730, shape_attrition = 2)
    x = suppressMessages(get_OM_xml(om, spec, sched_num, name = "F", decay = "placeholder",
                                    entomology = FALSE))
    as_factor = suppressMessages(om_scenario_values(data.frame(h = factor(c("3", "1"))), x,
                                                    halflife = "h"))
    as_number = suppressMessages(om_scenario_values(data.frame(h = c(3, 1)), x,
                                                    halflife = "h"))
    expect_equal(as_factor$F_deterrency_L, as_number$F_deterrency_L)
    # An undeployed dummy-host intervention is an error, also with fixed decays
    skip_if_not_installed("openMalariaUtilities")
    b = suppressMessages(define_OM_vector_control(om_base_ento(49L), dh_net(name = "D")))
    expect_error(om_scenario_values(data.frame(x = 1), b, "D"), "has not been deployed")
})

test_that("an entomology without the indoor/outdoor split warns that all bites are reached", {
    unsplit = suppressMessages(get_OM_ento_snippet(gss))
    net = om_vector_control(eff, 730, 2)
    expect_warning(suppressMessages(get_OM_GVI_snippet(net, unsplit, "ITN")),
                   "no indoor/outdoor split.*ITN acts on all the bites")
    expect_warning(suppressMessages(get_OM_xml(unsplit, net, sched_num)),
                   "no indoor/outdoor split")
    expect_warning(suppressMessages(get_OM_xml(unsplit, net, sched_num, decay = "placeholder")),
                   "no indoor/outdoor split")
    expect_no_warning(suppressMessages(get_OM_GVI_snippet(net, om, "ITN")))
    expect_no_warning(suppressMessages(get_OM_xml(om, net, sched_num)))
    # The entomology alone does not warn
    expect_no_warning(suppressMessages(get_OM_xml(unsplit)))
})

test_that("schedule dates must be YYYY-MM-DD everywhere", {
    expect_error(om_campaigns(dates = c("2021/01/01", "2022-01-01"), coverage = 1),
                 "dates must be dates \"YYYY-MM-DD\".*2021/01/01")
    expect_error(om_campaigns("2021-01-01x", "2023-01-01", coverage = 1), "from must be dates")
    expect_error(om_campaigns("2021-01-01", "2023-01-01", coverage = 1, end_date = "1/1/2030"),
                 "end_date must be dates")
    expect_error(AnophelesModel:::om_schedule(data.frame(date = "2021-13-01", coverage = 1)),
                 "schedule\\$date must be dates")
    expect_error(om_continuous(age = 0.25, coverage = 0.8, begin = "2021/01/01"),
                 "begin must be dates")
    # Date objects are accepted
    s = om_campaigns(dates = as.Date(c("2021-01-01", "2024-01-01")), coverage = 0.5,
                     end_date = as.Date("2027-01-01"))
    expect_equal(s$date, c("2021-01-01", "2024-01-01"))
    expect_equal(attr(s, "end_date"), "2027-01-01")
    expect_no_error(om_continuous(age = 0.25, coverage = 0.8, begin = as.Date("2021-01-01")))
})

test_that("two intervention names cannot give the same placeholders in a baseList", {
    skip_if_not_installed("openMalariaUtilities")
    net = om_vector_control(eff, 730, 2)
    b = suppressMessages(define_OM_vector_control(om_base_ento(), net, name = "IG 2"))
    expect_error(suppressMessages(define_OM_vector_control(b, net, name = "IG_2")),
                 "'IG_2' and 'IG 2' give the same placeholder names \\(@IG_2_...@\\)")
    expect_no_error(suppressMessages(define_OM_vector_control(b, net, name = "IG3")))
})

test_that("the attrition warnings are given on every export route", {
    no_attr = om_vector_control(eff, decay_attrition = "none")
    with_attr = om_vector_control(eff, 730, 2)
    xml = function(...) suppressMessages(get_OM_xml(om, schedule = sched_num,
                                                    entomology = FALSE, ...))
    expect_warning(xml(interv = no_attr), "ITN without attrition")
    expect_warning(xml(interv = no_attr, decay = "placeholder"), "ITN without attrition")
    expect_warning(xml(interv = list(a = no_attr, b = with_attr), name = "L",
                       decay = "placeholder"), "ITN without attrition")
    expect_warning(xml(interv = with_attr, intervention_type = "IRS",
                       decay = "placeholder"), "IRS with attrition")
    expect_no_warning(xml(interv = list(a = with_attr, b = with_attr), name = "L"))
})

test_that("deploying a member of a list points to the list's name", {
    skip_if_not_installed("openMalariaUtilities")
    nets = list(martin = om_vector_control(eff, 730, 2), assenga = om_vector_control(eff, 365, 2))
    b = suppressMessages(define_OM_vector_control(om_base_ento(), nets, name = "futITN",
                                                  decay = "placeholder"))
    expect_error(deploy_OM_vector_control(b, "martin", sched_num),
                 "one of the interventions of the list defined as 'futITN'.*deploy 'futITN'")
    expect_no_error(suppressMessages(deploy_OM_vector_control(b, "futITN", sched_num)))
})
