# Helpers for the vector bionomics table (vec_ent_param or a user-supplied
# table with the same structure): validation, reading, taxon lookup by
# name and level, and choice of the human blood index.

# Columns of a bionomics table
.BIONOMICS_TEXT_COLS = c("species_name", "level", "complex", "imputed_traits")
.BIONOMICS_REQUIRED  = c("species_name", "level", "M", "Chi", "A0", "zeta.3",
                         "td", "tau", "ts", "to", "endophily", "endophagy")
.BIONOMICS_PROPS     = c("M", "Chi", "A0", "endophily", "endophagy",
                         "HBI_indoor", "HBI_outdoor")
.TAXON_LEVELS        = c("species", "complex", "genus")

# Synonyms accepted by def_vector_params(), with the warning they trigger.
# An. gambiae s.s. and An. coluzzii are merged in the bionomics data.
.GAMBIAE_MSG = paste("An. gambiae s.s. and An. coluzzii are merged in the",
                     "bionomics data; using \"Anopheles gambiae ss/coluzzii\".")
.TAXON_ALIASES = data.frame(
    alias     = c("Anopheles gambiae s.s.", "Anopheles gambiae ss",
                  "Anopheles gambiae s.s. / coluzzii", "Anopheles coluzzii",
                  "Anopheles gambiae"),
    level     = "species",
    canonical = "Anopheles gambiae ss/coluzzii",
    message   = c(rep(.GAMBIAE_MSG, 4),
                  paste(.GAMBIAE_MSG, "For the whole Gambiae complex use",
                        "mosquito_species = \"Gambiae\", level = \"complex\".")),
    stringsAsFactors = FALSE
)


#########check_bionomics_table############
# Internal: validate a bionomics table and return it as a data.frame
# (empty complex -> NA, missing imputed_traits -> "").
check_bionomics_table = function(tab) {
    if (!is.data.frame(tab)) stop("The bionomics table must be a data frame.")
    tab = as.data.frame(tab, stringsAsFactors = FALSE)

    missing_cols = setdiff(.BIONOMICS_REQUIRED, names(tab))
    if (length(missing_cols))
        stop("Bionomics table: missing column(s) ",
             paste(missing_cols, collapse = ", "))
    if (!("complex" %in% names(tab)))        tab$complex = NA_character_
    if (!("imputed_traits" %in% names(tab))) tab$imputed_traits = ""
    tab$complex[!is.na(tab$complex) & tab$complex == ""] = NA_character_
    tab$imputed_traits[is.na(tab$imputed_traits)] = ""

    bad_level = setdiff(unique(tab$level), .TAXON_LEVELS)
    if (length(bad_level))
        stop("Bionomics table: invalid level(s) ", paste(bad_level, collapse = ", "),
             ". Valid: ", paste(.TAXON_LEVELS, collapse = ", "))
    dup = duplicated(tab[, c("species_name", "level")])
    if (any(dup))
        stop("Bionomics table: duplicated taxa (species_name, level): ",
             paste(tab$species_name[dup], tab$level[dup], sep = "/", collapse = ", "))

    num_cols = setdiff(names(tab), .BIONOMICS_TEXT_COLS)
    not_num  = num_cols[!vapply(tab[num_cols], is.numeric, logical(1))]
    if (length(not_num))
        stop("Bionomics table: non-numeric column(s) ", paste(not_num, collapse = ", "))
    req_num = intersect(.BIONOMICS_REQUIRED, num_cols)
    if (anyNA(tab[req_num]))
        stop("Bionomics table: missing values in ", paste(
            req_num[vapply(tab[req_num], anyNA, logical(1))], collapse = ", "))
    if (any(unlist(tab[num_cols]) < 0, na.rm = TRUE))
        stop("Bionomics table: negative values")
    props = intersect(.BIONOMICS_PROPS, names(tab))
    if (any(unlist(tab[props]) > 1, na.rm = TRUE))
        stop("Bionomics table: proportions above 1 in ",
             paste(props[vapply(tab[props], function(x) any(x > 1, na.rm = TRUE),
                                logical(1))], collapse = ", "))
    if (any(tab$tau <= 0)) stop("Bionomics table: tau must be positive")
    tab
}


#########read_bionomics_table############
#' @title Read a vector bionomics table from a CSV file
#'
#' @description Reads and validates a table of vector bionomics with the
#' same structure as \code{vec_ent_param}, for example a newer export from
#' the AnophelesBionomics package, so that it can be used with
#' \code{def_vector_params(bionomics_table = ...)} without reinstalling
#' AnophelesModel2.
#'
#' @param file Path to a CSV file. Required columns: \code{species_name},
#'   \code{level} (\code{"species"}, \code{"complex"} or \code{"genus"}),
#'   \code{M}, \code{Chi}, \code{A0}, \code{zeta.3}, \code{td}, \code{tau},
#'   \code{ts}, \code{to}, \code{endophily}, \code{endophagy}. Optional:
#'   \code{complex}, \code{imputed_traits}, standard deviations
#'   (\code{*.sd}) and \code{HBI_indoor}, \code{HBI_outdoor}.
#'
#' @return The validated table as a data frame.
#'
#' @seealso \code{\link{def_vector_params}}, \code{\link{vec_ent_param}}
#'
#' @importFrom utils read.csv
#' @export
read_bionomics_table = function(file) {
    tab = utils::read.csv(file, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    check_bionomics_table(tab)
}


# Internal: select one taxon (one row) from a bionomics table by name and
# level, accepting the synonyms in .TAXON_ALIASES (with a warning) and the
# usual spellings of complexes ("Anopheles gambiae complex", "Gambiae").
select_taxon = function(tab, name, level) {
    hit = which(tab$species_name == name & tab$level == level)
    if (length(hit) == 1L) return(tab[hit, , drop = FALSE])

    alias = .TAXON_ALIASES[.TAXON_ALIASES$alias == name &
                               .TAXON_ALIASES$level == level, ]
    if (nrow(alias) == 1L) {
        hit = which(tab$species_name == alias$canonical & tab$level == level)
        if (length(hit) == 1L) {
            warning(alias$message, call. = FALSE)
            return(tab[hit, , drop = FALSE])
        }
    }

    if (level == "complex") {
        # Complexes are stored under their label, e.g. "Gambiae"
        label = sub("^Anopheles ", "", sub("( complex| s\\.l\\.)$", "", name))
        label = paste0(toupper(substr(label, 1, 1)), substring(label, 2))
        hit = which(tab$species_name == label & tab$level == "complex")
        if (length(hit) == 1L) return(tab[hit, , drop = FALSE])
    }

    other = unique(tab$level[tab$species_name == name])
    if (length(other))
        stop("'", name, "' is a ", paste(other, collapse = "/"),
             " in the bionomics table, not a ", level, ": use level = \"",
             other[1], "\".", call. = FALSE)
    stop("'", name, "' (level \"", level, "\") is not in the bionomics table. ",
         "Use list_all_species() to see the available taxa.", call. = FALSE)
}


# Internal: set Chi (and Chi.sd) from the chosen human blood index.
#   "indoor"   : HBI_indoor
#   "outdoor"  : HBI_outdoor
#   "weighted" : HBI_indoor * endophagy + HBI_outdoor * (1 - endophagy); its sd
#                is approximated assuming independent estimates.
apply_hbi = function(vec_p, hbi) {
    sd_of = function(nm) if (!is.null(vec_p[[nm]])) vec_p[[nm]] else NA_real_
    need  = switch(hbi, indoor = "HBI_indoor", outdoor = "HBI_outdoor",
                   weighted = c("HBI_indoor", "HBI_outdoor", "endophagy"))
    missing_cols = need[vapply(need, function(nm) is.null(vec_p[[nm]]), logical(1))]
    if (length(missing_cols))
        stop("hbi = \"", hbi, "\" requires column(s) ",
             paste(missing_cols, collapse = ", "), call. = FALSE)

    if (hbi == "indoor") {
        vec_p$Chi = vec_p$HBI_indoor;  vec_p$Chi.sd = sd_of("HBI_indoor.sd")
    } else if (hbi == "outdoor") {
        vec_p$Chi = vec_p$HBI_outdoor; vec_p$Chi.sd = sd_of("HBI_outdoor.sd")
    } else {
        e = vec_p$endophagy; hi = vec_p$HBI_indoor; ho = vec_p$HBI_outdoor
        vec_p$Chi    = hi * e + ho * (1 - e)
        vec_p$Chi.sd = sqrt(e^2 * sd_of("HBI_indoor.sd")^2 +
                            (1 - e)^2 * sd_of("HBI_outdoor.sd")^2 +
                            (hi - ho)^2 * sd_of("endophagy.sd")^2)
    }
    vec_p$hbi_source = hbi
    vec_p
}
