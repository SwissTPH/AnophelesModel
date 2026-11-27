library(AnophelesModel)
library(testthat)

########################################
# Tests for list_activity()
########################################
# This test checks whether all activity patterns for An. gambiae are listed.
test_that("test: check An. gambiae activities", {
    an_gambiae_activity = list_activity()

    c_sol = activity_patterns[which(activity_patterns$species ==
                                                        "Anopheles gambiae" &
                                        activity_patterns$sampling ==
                                        "HBI"), ]

    expect_equal(c_sol, an_gambiae_activity)
})

########################################
# Tests for get_ITN_params(): host-seeking killing rescaled by alpha_0
########################################
db  = ITN_parameterisations_Champagne2025
row = function(net, EHT, par) db[db$netType == net & db$EHT == EHT &
                                     db$parameter == par, ]

test_that("dummy-host HSK is rescaled by the control alpha_0 of the same arm", {
    hsk  = row("Interceptor G2", "Kibondo", "KillingDuringHostSeekingRelative")
    ctrl = row("control", "Kibondo", "alpha_0")
    abs_U = hsk$Unwashed * ctrl$Unwashed
    abs_W = hsk$Washed   * ctrl$Washed

    p = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                       verbose = FALSE)
    expect_equal(p$alpha_0, c(Unwashed = ctrl$Unwashed, Washed = ctrl$Washed))
    expect_equal(p$efficacy[["KillingDuringHostSeekingAbsolute"]], abs_U)
    expect_equal(p$halflife_insecticide[["KillingDuringHostSeekingAbsolute"]],
                 3 / (1 - abs_W / abs_U))

    pw = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                        washed = TRUE, verbose = FALSE)
    expect_equal(pw$efficacy[["KillingDuringHostSeekingAbsolute"]], abs_W)

    # Other components are not rescaled
    post = row("Interceptor G2", "Kibondo", "Postprandialkilling")
    expect_equal(p$efficacy[["PostPrandial"]], post$Unwashed)
})

test_that("rescale_hsk = FALSE returns the stored relative values, under their name", {
    hsk = row("Interceptor G2", "Kibondo", "KillingDuringHostSeekingRelative")
    p = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                       rescale_hsk = FALSE, verbose = FALSE)
    expect_null(p$alpha_0)
    expect_false("KillingDuringHostSeekingAbsolute" %in% names(p$efficacy))
    expect_equal(p$efficacy[["KillingDuringHostSeekingRelative"]], hsk$Unwashed)
    expect_equal(p$halflife_insecticide[["KillingDuringHostSeekingRelative"]],
                 hsk$halflife_insecticide)
    # The model does not take a relative value
    expect_error(build_intervention(efficacy = p$efficacy, mode = "dummy_host",
                                    duration_years = 1, model_p = build_model_obj(
                                        def_vector_params(), def_host_params())),
                 "KillingDuringHostSeekingRelative not accepted")
})

test_that("the dummy-host efficacy names are in the same order after rescaling", {
    p = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                       verbose = FALSE)
    r = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                       rescale_hsk = FALSE, verbose = FALSE)
    expect_equal(sub("Absolute$", "Relative", names(p$efficacy)), names(r$efficacy))
    expect_equal(names(p$halflife_insecticide), names(p$efficacy))
})

test_that("standard mode is unaffected by the rescaling", {
    p = get_ITN_params("Interceptor G2", "Kibondo", verbose = FALSE)
    expect_null(p$alpha_0)
    expect_false("KillingDuringHostSeekingAbsolute" %in% names(p$efficacy))
})

test_that("list_ITN_params lists pairs usable by get_ITN_params in both modes", {
    itns = list_ITN_params()
    expect_named(itns, c("netType", "EHT", "reference"))
    expect_false("control" %in% itns$netType)
    expect_false(anyDuplicated(itns[, c("netType", "EHT")]) > 0)
    expect_silent(head(itns))                      # returned, not printed
    for (i in seq_len(nrow(itns))) for (mode in c("standard", "dummy_host"))
        expect_no_error(get_ITN_params(itns$netType[i], itns$EHT[i], mode = mode,
                                       verbose = FALSE))
})

test_that("an ambiguous control alpha_0 gives a clear error", {
    fake = data.frame(netType = "control", EHT = "X", parameter = "alpha_0",
                      reference = "same ref", Unwashed = c(0.8, 1.7),
                      Washed = c(0.7, 1.6), stringsAsFactors = FALSE)
    expect_error(AnophelesModel:::control_alpha_0(fake, "X", "same ref"),
                 "unique control alpha_0")
    fake$reference = c("ref A", "ref B")
    expect_equal(AnophelesModel:::control_alpha_0(fake, "X", "ref B"),
                 c(Unwashed = 1.7, Washed = 1.6))
    # Same reference, several fits: matched on the fit
    fake$reference = "same ref"
    fake$fit = 1:2
    expect_equal(AnophelesModel:::control_alpha_0(fake, "X", "same ref", fit = 2),
                 c(Unwashed = 1.7, Washed = 1.6))
})

test_that("each net is rescaled by the control of its own fit (Sovegnon)", {
    skip_if(is.null(ITN_parameterisations_Champagne2025$fit),
            "dataset without the fit column (regenerate it)")
    # Controls of the source table (ITNcascades): treatment -1 with
    # Interceptor G2, treatment -2 with Pyrethroid-only
    a0 = function(net) get_ITN_params(net, "Sovegnon", mode = "dummy_host",
                                      verbose = FALSE)$alpha_0
    expect_equal(a0("Interceptor G2"),  c(Unwashed = 0.81, Washed = 0.73))
    expect_equal(a0("Pyrethroid-only"), c(Unwashed = 1.72, Washed = 1.66))
})

test_that("the rescaled host-seeking killing decays over the trial's own aging time", {
    db = ITN_parameterisations_Champagne2025
    expect_false(is.null(db$aged_time))
    hsk_hl = function(net, eht) get_ITN_params(net, eht, mode = "dummy_host",
                                               verbose = FALSE)$halflife_insecticide[["KillingDuringHostSeekingAbsolute"]]
    # Sovegnon: washed arm = 2 years (1.94 x 0.81 unwashed, 1.32 x 0.73 washed)
    expect_equal(unique(db$aged_time[db$EHT == "Sovegnon"]), 2)
    expect_equal(hsk_hl("Interceptor G2", "Sovegnon"),
                 2 / (1 - (1.32 * 0.73) / (1.94 * 0.81)), tolerance = 1e-10)
    # Without rescaling, the dataset's own value (also computed with 2 years)
    stored = db$halflife_insecticide[db$netType == "Interceptor G2" & db$EHT == "Sovegnon" &
                                     db$parameter == "KillingDuringHostSeekingRelative"]
    expect_equal(get_ITN_params("Interceptor G2", "Sovegnon", mode = "dummy_host",
                                rescale_hsk = FALSE, verbose = FALSE)$halflife_insecticide[[
                                    "KillingDuringHostSeekingRelative"]], stored)
    expect_equal(stored, 2 / (1 - 1.32 / 1.94), tolerance = 1e-10)
    # Other trials: 3 years
    k = db[db$netType == "Interceptor G2" & db$EHT == "Kibondo", ]
    expect_equal(unique(k$aged_time), 3)
    a0 = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host", verbose = FALSE)$alpha_0
    h  = k[k$parameter == "KillingDuringHostSeekingRelative", ]
    expect_equal(hsk_hl("Interceptor G2", "Kibondo"),
                 3 / (1 - (h$Washed * a0[["Washed"]]) / (h$Unwashed * a0[["Unwashed"]])),
                 tolerance = 1e-10)
})
