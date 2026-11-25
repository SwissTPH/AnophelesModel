# Step 2 of 2: build the package dataset vec_ent_param from the reference
# table data-raw/vec_ent_param_AnophelesBionomics.csv (created by step 1,
# data-raw/vec_ent_param_from_AnophelesBionomics.R). Does not need
# AnophelesBionomics.
#
# Run from the package root, then devtools::document() and devtools::test():
#   devtools::load_all(".")
#   source("data-raw/vec_ent_param.R")

csv = file.path("data-raw", "vec_ent_param_AnophelesBionomics.csv")
vec_ent_param = read_bionomics_table(csv)   # reads and validates

# Provenance written by step 1, if available
src_file = file.path("data-raw", "vec_ent_param_AnophelesBionomics_source.txt")
attr(vec_ent_param, "source") =
    if (file.exists(src_file)) readLines(src_file) else "package: AnophelesBionomics"

# Taxa used as defaults in the package, tests and vignette must exist
stopifnot("Anopheles gambiae ss/coluzzii" %in%
              vec_ent_param$species_name[vec_ent_param$level == "species"],
          "Anopheles albimanus" %in% vec_ent_param$species_name,
          sum(vec_ent_param$level == "genus") == 1L)

save(vec_ent_param, file = file.path("data", "vec_ent_param.RData"),
     compress = "xz")
message("Saved data/vec_ent_param.RData: ", nrow(vec_ent_param), " taxa (",
        paste(names(table(vec_ent_param$level)), table(vec_ent_param$level),
              sep = " ", collapse = ", "), ")")
