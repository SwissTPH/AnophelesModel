# Step 1 of 2: build the vector bionomics table of AnophelesModel2 from the
# posterior draws of AnophelesBionomics (no Stan refit).
#
# Requires AnophelesBionomics installed. Run from the AnophelesModel2 package
# root; writes data-raw/vec_ent_param_AnophelesBionomics.csv (the versioned
# reference table) and data-raw/vec_ent_param_AnophelesBionomics_source.txt.
# Then run step 2, data-raw/vec_ent_param.R, to update the package dataset.
#
# Conventions:
# - one row per taxon and level (species / complex / genus); a species alone in
#   its complex is listed once, as a species
# - posterior mean and sd per taxon; taxa without data for a trait carry the
#   values imputed by AnophelesBionomics, flagged in `imputed_traits`
# - tau = resting duration (1/q, q = fed / (fed + gravid))
# - Chi = indoor HBI by default; outdoor HBI is also stored
#   (see the `hbi` option of def_vector_params())

library(data.table)
pkg    = "AnophelesBionomics"
x      = as.data.table(readRDS(system.file("outputs", "full_bionomics_posterior.rds", package = pkg)))
compat = fread(system.file("extdata", "compatibility_repo_taxonomy_v2.csv", package = pkg))

traits = c(parous_rate = "M", sac_rate = "A0",
           indoor_HBI = "HBI_indoor", outdoor_HBI = "HBI_outdoor",
           endophagy = "endophagy", endophily = "endophily",
           resting_duration = "tau")
stopifnot(all(names(traits) %in% x$param))
x = x[param %in% names(traits)]

## 1. Taxonomy used by each trait's fit (data preparation only, no Stan)
maps = rbindlist(lapply(names(traits), function(v)
    as.data.table(AnophelesBionomics:::creation_df(v)$species_complex)[, param := v]))

## Names in the rds have the subgenus removed, e.g.
## "Anopheles (Nyssorhyncus) darlingi" -> "Anopheles darlingi": do the same here
strip_subgenus = function(s) trimws(gsub("\\s*\\([^)]*\\)", "", s))
maps[, `:=`(species = strip_subgenus(species), complex = strip_subgenus(complex))]
compat[, species := strip_subgenus(species)]
stopifnot(!any(grepl("\\(", x$name)))     # the rds itself has no parentheses



## 2. Species alone in their complex: species node + complex node stored under
##    the same name (two blocks). Keep the species node = the narrower block.
singletons = unique(maps[grepl("_complex$", complex), .(param, name = species)])
n_draws = x[, .N, by = .(param, name)][, ratio := N / min(N), by = param]
doubled = n_draws[ratio == 2, .(param, name)]
stopifnot(all(n_draws$ratio %in% c(1, 2)))
if (!fsetequal(doubled, singletons)) {
    print(fsetdiff(doubled, singletons)); print(fsetdiff(singletons, doubled))
    stop("Doubled names do not match the one-member complexes")
}
x[, block := 1L]
x[doubled, on = .(param, name), block := NA_integer_]
x[is.na(block), block := rep(1:2, each = .N / 2), by = .(param, name)]
keep = x[, .(sd = sd(value)), by = .(param, name, block)][, .SD[which.min(sd)], by = .(param, name)]
print(keep[doubled, on = .(param, name)])          # expected: block 1 kept everywhere
x = x[keep[, .(param, name, block)], on = .(param, name, block)][, block := NULL]

## 3. Posterior mean and sd per taxon and trait
summ = x[, .(mean = mean(value), sd = sd(value)), by = .(name, param)]
summ[, col := traits[param]]
tab = dcast(summ, name ~ col, value.var = c("mean", "sd"))
setnames(tab, sub("^mean_", "", names(tab)))
setnames(tab, sub("^sd_(.*)$", "\\1.sd", names(tab)))

## 4. Level, parent complex, traits without own data
tab = tab[!startsWith(name, "unlabeled ")]        # pseudo-species, not real taxa
complex_labels = unique(c(compat[complex != "", complex],
                          maps[!grepl("_complex$", complex), complex]))
tab[, level := fifelse(name == "Anopheles", "genus",
                       fifelse(name %in% complex_labels, "complex", "species"))]
tab[, complex := NA_character_]
tab[level == "species", complex := compat$complex[match(name, compat$species)]]
tab[complex == "", complex := NA_character_]

fitted = unique(rbind(maps[, .(param, name = species)], maps[, .(param, name = complex)]))
imp = CJ(name = tab[level != "genus", name], param = names(traits))[!fitted, on = .(param, name)]
imp = imp[, .(imputed_traits = paste(traits[param], collapse = ";")), by = name]
tab = imp[tab, on = "name"][is.na(imputed_traits), imputed_traits := ""]

## 5. Package conventions
print(tab[grepl("gambiae", name, ignore.case = TRUE), .(name, level)])   # check spelling
tab[level == "species" & name == "Anopheles gambiae s.s. / coluzzii",
    name := "Anopheles gambiae ss/coluzzii"]
tab[, `:=`(zeta.3 = 1, td = 0.33, ts = 10, to = 5)]
tab[, `:=`(Chi = HBI_indoor, Chi.sd = HBI_indoor.sd)]     # default: indoor HBI
setnames(tab, "name", "species_name")
setcolorder(tab, c("species_name", "level", "complex", "imputed_traits",
                   "M", "M.sd", "Chi", "Chi.sd",
                   "HBI_indoor", "HBI_indoor.sd", "HBI_outdoor", "HBI_outdoor.sd",
                   "A0", "A0.sd", "zeta.3", "td", "tau", "tau.sd", "ts", "to",
                   "endophily", "endophily.sd", "endophagy", "endophagy.sd"))

## 6. Checks and export
props = c("M", "Chi", "HBI_indoor", "HBI_outdoor", "A0", "endophily", "endophagy")
stopifnot(!anyDuplicated(tab[, .(species_name, level)]),
          !anyNA(tab[, c(props, "tau"), with = FALSE]),
          all(as.matrix(tab[, ..props]) >= 0 & as.matrix(tab[, ..props]) <= 1),
          all(tab$tau > 0))
print(tab[, .N, by = level])
print(tab[level == "species" & is.na(complex), species_name])   # species without a complex
fwrite(tab, file.path("data-raw", "vec_ent_param_AnophelesBionomics.csv"))

# Provenance, read by step 2
writeLines(c(paste("package:", pkg),
             paste("version:", as.character(utils::packageVersion(pkg))),
             paste("built:", format(Sys.Date()))),
           file.path("data-raw", "vec_ent_param_AnophelesBionomics_source.txt"))
