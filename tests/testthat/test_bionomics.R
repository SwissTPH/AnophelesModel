library(AnophelesModel2)
library(testthat)

# Vector bionomics table from AnophelesBionomics: taxonomy levels, aliases,
# human blood index options, validation and the whole-day tau in calc_psts().

gss = "Anopheles gambiae ss/coluzzii"

# ---------------------------------------------------------------------------
# 1. Dataset structure
# ---------------------------------------------------------------------------
test_that("vec_ent_param is a valid bionomics table with explicit levels", {
    expect_no_error(AnophelesModel2:::check_bionomics_table(vec_ent_param))
    expect_setequal(unique(vec_ent_param$level), c("species", "complex", "genus"))
    expect_equal(sum(vec_ent_param$level == "genus"), 1L)
    expect_false(anyDuplicated(vec_ent_param[, c("species_name", "level")]) > 0)
    # Default Chi is the indoor human blood index
    expect_equal(vec_ent_param$Chi, vec_ent_param$HBI_indoor)
    expect_true(gss %in% list_all_species("species")$species_name)
    expect_true("Gambiae" %in% list_all_species("complex")$species_name)
})

test_that("list_all_species returns taxa with their level", {
    all_taxa = list_all_species()
    expect_named(all_taxa, c("species_name", "level", "complex", "imputed_traits"))
    expect_equal(nrow(all_taxa), nrow(vec_ent_param))
    expect_true(all(list_all_species("complex")$level == "complex"))
})

# ---------------------------------------------------------------------------
# 2. Lookup by name and level, aliases
# ---------------------------------------------------------------------------
test_that("taxa are looked up by name and level", {
    sp = def_vector_params(gss)
    expect_equal(sp$species_name, gss)
    expect_equal(sp$level, "species")
    expect_equal(sp$complex, "Gambiae")

    cx = def_vector_params("Gambiae", level = "complex")
    expect_equal(cx$level, "complex")
    # Usual spellings of a complex are accepted silently
    expect_silent(cx2 <- def_vector_params("Anopheles gambiae complex", level = "complex"))
    expect_equal(cx2$species_name, "Gambiae")
    expect_equal(def_vector_params("Anopheles gambiae", level = "complex")$species_name,
                 "Gambiae")

    expect_equal(def_vector_params("Anopheles", level = "genus")$level, "genus")
})

test_that("gambiae s.s. / coluzzii aliases resolve with a warning", {
    for (alias in c("Anopheles gambiae", "Anopheles gambiae s.s.",
                    "Anopheles gambiae ss", "Anopheles coluzzii")) {
        expect_warning(p <- def_vector_params(alias), "merged", info = alias)
        expect_equal(p$species_name, gss, info = alias)
    }
    expect_warning(def_vector_params("Anopheles gambiae"), "level = \"complex\"")
})

test_that("unknown names and wrong levels give informative errors", {
    expect_error(def_vector_params("Gambiae"), "is a complex")
    expect_error(def_vector_params("Anopheles imaginarius"), "list_all_species")
    expect_error(def_vector_params(gss, level = "family"), "should be one of")
})

# ---------------------------------------------------------------------------
# 3. Human blood index options
# ---------------------------------------------------------------------------
test_that("hbi selects the human blood index used as Chi", {
    row = vec_ent_param[vec_ent_param$species_name == gss, ]

    ind = def_vector_params(gss)
    expect_equal(ind$Chi, row$HBI_indoor)
    expect_equal(ind$hbi_source, "indoor")

    out = def_vector_params(gss, hbi = "outdoor")
    expect_equal(out$Chi, row$HBI_outdoor)
    expect_equal(out$Chi.sd, row$HBI_outdoor.sd)

    w = def_vector_params(gss, hbi = "weighted")
    expect_equal(w$Chi, row$HBI_indoor * row$endophagy +
                     row$HBI_outdoor * (1 - row$endophagy))
    expect_true(w$Chi.sd > 0)

    expect_error(def_vector_params(gss, hbi = "both"), "should be one of")
})

test_that("the HBI choice changes the model", {
    mp = function(hbi) build_model_obj(def_vector_params(gss, hbi = hbi),
                                       def_host_params())
    expect_false(isTRUE(all.equal(mp("indoor")$host_params$alphai,
                                  mp("outdoor")$host_params$alphai)))
})

# ---------------------------------------------------------------------------
# 4. User-supplied bionomics tables
# ---------------------------------------------------------------------------
test_that("a bionomics table can be read, validated and used", {
    f = withr::local_tempfile(fileext = ".csv")
    tab = vec_ent_param
    tab$M[tab$species_name == gss] = 0.5
    utils::write.csv(tab, f, row.names = FALSE)

    read_tab = read_bionomics_table(f)
    expect_equal(nrow(read_tab), nrow(vec_ent_param))
    expect_equal(def_vector_params(gss, bionomics_table = read_tab)$M, 0.5)
})

test_that("invalid bionomics tables are rejected", {
    chk = AnophelesModel2:::check_bionomics_table
    bad = vec_ent_param; bad$level[1] = "family"
    expect_error(chk(bad), "invalid level")
    bad = rbind(vec_ent_param, vec_ent_param[1, ])
    expect_error(chk(bad), "duplicated")
    bad = vec_ent_param; bad$Chi[1] = 1.2
    expect_error(chk(bad), "proportions above 1")
    bad = vec_ent_param; bad$tau = NULL
    expect_error(chk(bad), "missing column")
})

test_that("rows of vec_ent_param can be used as custom vector tables", {
    row = vec_ent_param[vec_ent_param$level == "species" &
                            is.na(vec_ent_param$complex), ][1, ]
    expect_no_error(def_vector_params(vector_table = row, verbose = FALSE))
})

# ---------------------------------------------------------------------------
# 5. calc_psts: resting duration in whole days
# ---------------------------------------------------------------------------
test_that("calc_psts rounds tau to whole days (at least one)", {
    psts = function(tau) AnophelesModel2:::calc_psts(10, tau, c(0.5, 0.6), c(0.3, 0.2))$psts
    expect_equal(psts(1.07), psts(1))      # 1.07 days -> 1 day, no tau < 2 fallback
    expect_equal(psts(1.5),  psts(2))      # halves round up (not to even)
    expect_equal(psts(2.03), psts(2))
    expect_equal(psts(0.4),  psts(1))      # at least one day
    expect_false(isTRUE(all.equal(psts(1), psts(2))))
    expect_true(all(is.finite(psts(1))))
})

test_that("calc_psts includes the term at a zero upper summation index", {
    # ts=10, tau=6: the first sum contains only PA^4. In the inner
    # sums, l=1 has no terms; l=2:5 contribute Pdf * PA^(l-2).
    PA = c(0.5, 0.6)
    Pdf = c(0.3, 0.2)
    res = AnophelesModel2:::calc_psts(10, 6, PA, Pdf)
    expect_equal(res$sumkplus, PA^4)
    expect_equal(res$sumlv, unname(rbind(c(0, 0), Pdf, Pdf * PA,
                                         Pdf * PA^2, Pdf * PA^3)))
    expect_equal(res$psts, c(0.625, 0.5648), tolerance = 1e-12)
})

test_that("calc_psts includes inner zero-index terms when the first sum has several terms", {
    # ts=10, tau=5: first sum = PA^5 + Pdf; each inner sum has
    # one term, Pdf * PA^l for l=1:4.
    res = AnophelesModel2:::calc_psts(10, 5, 0.5, 0.3)
    expect_equal(res$sumkplus, 0.33125)
    expect_equal(res$sumlv, matrix(c(0.15, 0.075, 0.0375, 0.01875), ncol = 1))
    expect_equal(res$psts, 0.6125)
})

test_that("calc_psts leaves negative-index sums empty", {
    # ts=10, tau=9: first sum = PA, inner sums l=1:7 are empty,
    # and l=8 contributes Pdf.
    res = AnophelesModel2:::calc_psts(10, 9, 0.5, 0.3)
    expect_equal(res$sumkplus, 0.5)
    expect_equal(res$sumlv, matrix(c(rep(0, 7), 0.3), ncol = 1))
    expect_equal(res$psts, 0.8)
})

test_that("calc_psts handles incubation equal to resting duration", {
    for (tau in c(1, 10)) {
        # The first sum is PA^0 = 1 and all inner sums are empty.
        res = AnophelesModel2:::calc_psts(tau, tau, c(0.5, 0.6), c(0.3, 0.2))
        expect_equal(res$sumkplus, c(1, 1))
        expect_equal(res$sumlv, matrix(0, nrow = tau - 1, ncol = 2))
        expect_equal(res$psts, c(1, 1))
    }
})
