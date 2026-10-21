library(AnophelesModel2)
library(testthat)

# v2 reproduces AnophelesModel v1 on identical inputs (same vector and host
# parameters, unit populations, same intervention effects, exposure 1,
# standard model). The v1 results are a fixture made by
# data-raw/v1_reference.R, so v1 is not needed here. Differences between the
# versions in the bundled data (bionomics) or in exposure are deliberate and
# not tested here.

fixture = test_path("fixtures", "v1_reference.rds")

test_that("v2 reproduces v1 on identical inputs", {
    skip_if_not(file.exists(fixture),
                "no v1 reference: run data-raw/v1_reference.R with AnophelesModel installed")
    ref = readRDS(fixture)
    mp  = build_model_obj(def_vector_params(vector_table = ref$vec_table, verbose = FALSE),
                          def_host_params(ref$host_table, verbose = FALSE))

    # The model without interventions
    expect_equal(mp$host_params$muvA, ref$v1$muvA, tolerance = 1e-8)
    expect_equal(unname(mp$host_params$alphai), unname(ref$v1$alphai), tolerance = 1e-8)

    # The impact of the same interventions
    for (case in names(ref$cases)) {
        iv  = suppressMessages(do.call(build_intervention,
                                       c(ref$cases[[case]], list(model_p = mp))))
        res = calculate_impact(iv, ref$cov, mp)
        expect_equal(res$avg_impact, ref$v1$impact[[case]], tolerance = 1e-8, info = case)
        expect_equal(res$avg_vc, ref$v1$vc[[case]], tolerance = 1e-8, info = case)
    }
})
