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
