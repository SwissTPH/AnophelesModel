#' @title The main usages of the package
#'
#' @description A framework for assessing the impact of malaria vector
#' control interventions, taking into account species- and/or
#' geographically-specific mosquito bionomics. Version 2 is a refactoring
#' of version 1 (see NEWS.md for the differences).
#'
#' @section Overview:
#' The package combines a database of vector bionomics (from the
#' AnophelesBionomics package, \code{\link{vec_ent_param}}), host parameters,
#' activity patterns and ITN efficacies fitted to experimental hut trials
#' (\code{\link{ITN_parameterisations_Champagne2025}}) to parameterise an
#' entomological model of the mosquito oviposition cycle. The model computes,
#' for a given \emph{Anopheles} taxon, the reduction in vectorial capacity
#' due to vector control interventions (\code{\link{build_intervention}},
#' \code{\link{calculate_impact}}, \code{\link{run_interventions}},
#' \code{\link{calculate_combined_impact}}), and can be used to compare
#' interventions across species and settings. The same parameterisations can
#' be exported to OpenMalaria scenarios (entomology and vector control; see
#' \code{\link{get_OM_ento_snippet}} and \code{\link{get_OM_xml}}).
#'
#' @author Clara Champagne, \email{clara.champagne@swisstph.ch}
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @name AnophelesModel-package
"_PACKAGE"

# Suppress R CMD check NOTEs for lazy-loaded package datasets referenced by
# name in code (ggplot2 columns are referenced with .data$)
utils::globalVariables(c(
    "vec_ent_param", "host_ent_param", "activity_patterns",
    "ITN_parameterisations_Champagne2025"
))
