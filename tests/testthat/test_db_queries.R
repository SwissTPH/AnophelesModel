library(AnophelesModel2)
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
    hsk  = row("Interceptor G2", "Kibondo", "KillingDuringHostSeeking")
    ctrl = row("control", "Kibondo", "alpha_0")
    abs_U = hsk$Unwashed * ctrl$Unwashed
    abs_W = hsk$Washed   * ctrl$Washed

    p = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                       verbose = FALSE)
    expect_equal(p$alpha_0, c(Unwashed = ctrl$Unwashed, Washed = ctrl$Washed))
    expect_equal(p$efficacy[["HostSeekingKilling"]], abs_U)
    expect_equal(p$halflife_insecticide[["HostSeekingKilling"]],
                 3 / (1 - abs_W / abs_U))

    pw = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                        washed = TRUE, verbose = FALSE)
    expect_equal(pw$efficacy[["HostSeekingKilling"]], abs_W)

    # Other components are not rescaled
    post = row("Interceptor G2", "Kibondo", "Postprandialkilling")
    expect_equal(p$efficacy[["PostPrandial"]], post$Unwashed)
})

test_that("rescale_hsk = FALSE returns the stored values", {
    hsk = row("Interceptor G2", "Kibondo", "KillingDuringHostSeeking")
    p = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host",
                       rescale_hsk = FALSE, verbose = FALSE)
    expect_null(p$alpha_0)
    expect_equal(p$efficacy[["HostSeekingKilling"]], hsk$Unwashed)
    expect_equal(p$halflife_insecticide[["HostSeekingKilling"]],
                 hsk$halflife_insecticide)
})

test_that("standard mode is unaffected by the rescaling", {
    p = get_ITN_params("Interceptor G2", "Kibondo", verbose = FALSE)
    expect_null(p$alpha_0)
    expect_false("HostSeekingKilling" %in% names(p$efficacy))
})

test_that("an ambiguous control alpha_0 gives a clear error", {
    fake = data.frame(netType = "control", EHT = "X", parameter = "alpha_0",
                      reference = "same ref", Unwashed = c(0.8, 1.7),
                      Washed = c(0.7, 1.6), stringsAsFactors = FALSE)
    expect_error(AnophelesModel2:::control_alpha_0(fake, "X", "same ref"),
                 "unique control alpha_0")
    fake$reference = c("ref A", "ref B")
    expect_equal(AnophelesModel2:::control_alpha_0(fake, "X", "ref B"),
                 c(Unwashed = 1.7, Washed = 1.6))
})
