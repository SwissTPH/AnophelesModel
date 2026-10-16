# Functions for querying the built-in ITN parameterisation database.

# Internal name maps per model mode
.ITN_STANDARD_MAP = c(
    Repellent           = "Deterrency",
    Preprandialkilling  = "PrePrandial",
    Postprandialkilling = "PostPrandial"
)
.ITN_DUMMYHOST_MAP = c(
    Repellent                = "Deterrency",
    KillingDuringHostSeeking = "HostSeekingKilling",
    Postprandialkilling      = "PostPrandial"
)


#########list_ITN_params############
#' @title List available ITN parameterisations
#'
#' @description Prints all net type / EHT combinations available in the
#' built-in \code{ITN_parameterisations_Champagne2025} dataset.
#'
#' Every pair has both parameterisations (standard and dummy-host model),
#' chosen with \code{mode} in \code{\link{get_ITN_params}}.
#'
#' @return Data frame with columns \code{netType}, \code{EHT} and
#' \code{reference} (also printed to console); \code{netType} and \code{EHT}
#' are the \code{net_type} and \code{EHT} arguments of
#' \code{\link{get_ITN_params}}.
#'
#' @author Clara Champagne, \email{clara.champagne@swisstph.ch}
#'
#' @examples
#' list_ITN_params()
#'
#' @export
#'
list_ITN_params = function() {
    db  = ITN_parameterisations_Champagne2025
    out = unique(db[db$netType != "control",
                    c("netType", "EHT", "reference")])
    out = out[order(out$EHT, out$netType), ]
    rownames(out) = NULL
    print(out)
    invisible(out)
}


#########get_ITN_params############
#' @title Load ITN efficacy parameters from the built-in database
#'
#' @description Returns efficacy parameters and per-component insecticide
#' half-lives for a named net type and experimental hut trial (EHT), ready
#' to be passed directly to \code{\link{build_intervention}}.
#'
#' Two model modes are supported:
#' \itemize{
#'   \item \code{"standard"} (default): three-host model with
#'     \code{Deterrency}, \code{PrePrandial}, and \code{PostPrandial}.
#'   \item \code{"dummy_host"}: four-host model with
#'     \code{Deterrency}, \code{HostSeekingKilling}, and \code{PostPrandial}.
#'     \code{Deterrency} and \code{PostPrandial} are required in both modes.
#' }
#'
#' The returned \code{halflife_insecticide} vector encodes per-component
#' insecticide decay as a \strong{linear} decay (in years). Despite the name,
#' it is the time at which the effect reaches zero, calibrated so that
#' efficacy after 3 years equals the washed value. Negative values (efficacy
#' increased with washing) are returned as fitted. To use the unwashed
#' efficacy throughout, pass \code{insecticide_decay = FALSE} to
#' \code{\link{build_intervention}}.
#'
#' In the experimental hut model, host-seeking killing is fitted
#' \strong{relative} to the availability \code{alpha_0} of the trial's control
#' arm (extra host-seeking mortality \code{KillingDuringHostSeeking * alpha_0}).
#' Since \code{alpha_0} varies widely between trials, relative values are not
#' comparable across trials. With \code{rescale_hsk = TRUE}, the returned
#' \code{HostSeekingKilling} is the \strong{absolute} value
#' \code{KillingDuringHostSeeking * alpha_0}, using the control \code{alpha_0}
#' of the same arm (unwashed or washed fit), and its insecticide decay is
#' recomputed from the absolute unwashed and washed values.
#'
#' @param net_type Character string; net type label as in the \code{netType}
#'   column of \code{ITN_parameterisations_Champagne2025}.
#'   Use \code{\link{list_ITN_params}} to see available values.
#' @param EHT Character string; experimental hut trial identifier as in the
#'   \code{EHT} column.
#' @param mode Character; \code{"standard"} (default) for the three-host model
#'   or \code{"dummy_host"} for the four-host model.
#' @param washed Logical (default \code{FALSE}). If \code{FALSE}, returns
#'   \emph{Unwashed} (new net) efficacy values; if \code{TRUE}, returns
#'   \emph{Washed} values.
#' @param rescale_hsk Logical (default \code{TRUE}); dummy-host mode only.
#'   If \code{TRUE}, multiply host-seeking killing by the control
#'   \code{alpha_0} of its arm (see Details). Set to \code{FALSE} for
#'   datasets whose host-seeking killing is already absolute.
#' @param verbose Logical (default \code{TRUE}). Print a summary of the
#'   loaded parameterisation.
#'
#' @return Named list with:
#' \itemize{
#'   \item \code{efficacy}: named numeric vector for the \code{efficacy}
#'     argument of \code{\link{build_intervention}}.
#'   \item \code{halflife_insecticide}: named numeric vector (years) encoding
#'     a \strong{linear} per-component decay (time to zero).
#'     Pass directly to the \code{halflife_insecticide} argument of
#'     \code{\link{build_intervention}}.
#'   \item \code{name}: suggested intervention label
#'     (\code{"net_type (EHT)"}).
#'   \item \code{reference}: citation string from the dataset.
#'   \item \code{alpha_0}: control availabilities \code{c(Unwashed, Washed)}
#'     used to rescale host-seeking killing, or \code{NULL} if no rescaling
#'     was done.
#' }
#'
#' @author Clara Champagne, \email{clara.champagne@swisstph.ch}
#'
#' @seealso \code{\link{list_ITN_params}}, \code{\link{build_intervention}}
#'
#' @examples
#' # Standard three-host model
#' itn_p = get_ITN_params("Interceptor G2", "Kibondo")
#' itn_p$efficacy
#' itn_p$halflife_insecticide
#'
#' # Dummy-host (four-host) model
#' itn_dh = get_ITN_params("Interceptor G2", "Kibondo", mode = "dummy_host")
#' itn_dh$efficacy
#'
#' @importFrom stats setNames
#' @export
#'
get_ITN_params = function(net_type, EHT, mode = "standard",
                          washed = FALSE, rescale_hsk = TRUE,
                          verbose = TRUE) {
    mode = match.arg(mode, c("standard", "dummy_host"))
    param_map = if (mode == "standard") .ITN_STANDARD_MAP else .ITN_DUMMYHOST_MAP

    db = ITN_parameterisations_Champagne2025

    # Validate inputs
    available = unique(db$netType[db$netType != "control"])
    if (!(net_type %in% available)) {
        stop("net_type '", net_type, "' not found. ",
             "Use list_ITN_params() to see available options.")
    }
    available_EHT = unique(db$EHT[db$netType == net_type])
    if (!(EHT %in% available_EHT)) {
        stop("EHT '", EHT, "' not found for net_type '", net_type, "'. ",
             "Available: ", paste(available_EHT, collapse = ", "), ".")
    }

    rows = db[db$netType == net_type & db$EHT == EHT &
                  db$parameter %in% names(param_map), ]

    val_col  = if (washed) "Washed" else "Unwashed"
    efficacy = setNames(rows[[val_col]], param_map[rows$parameter])

    # Linear time-to-zero in years (3 / (1 - Washed/Unwashed)); negative values
    # (efficacy increased with washing) are kept as fitted.
    hl_years  = rows$halflife_insecticide
    halflife_insecticide = setNames(hl_years, param_map[rows$parameter])

    ref  = rows$reference[1]
    name = paste0(net_type, " (", EHT, ")")

    # Host-seeking killing is fitted relative to the control availability of
    # its trial arm (mu_i = mu_0 + KillingDuringHostSeeking * alpha_0 in the
    # EHT model). Rescale it to an absolute value per arm, so that values are
    # comparable across trials, and recompute its insecticide decay from the
    # absolute values (the control alpha_0 may differ between arms).
    alpha_0 = NULL
    if (rescale_hsk && "HostSeekingKilling" %in% names(efficacy)) {
        alpha_0  = control_alpha_0(db, EHT, ref)
        hsk      = rows[rows$parameter == "KillingDuringHostSeeking", ]
        hsk_abs  = c(Unwashed = hsk$Unwashed * alpha_0[["Unwashed"]],
                     Washed   = hsk$Washed   * alpha_0[["Washed"]])
        aged_time = 3   # years of the washed (20x) arm, as in the dataset
        efficacy[["HostSeekingKilling"]] = hsk_abs[[val_col]]
        halflife_insecticide[["HostSeekingKilling"]] =
            aged_time / (1 - hsk_abs[["Washed"]] / hsk_abs[["Unwashed"]])
    }

    if (verbose) {
        cat("ITN parameterisation loaded:", name, "\n")
        cat("Mode:", mode, "\n")
        cat("Reference:", ref, "\n")
        cat("Efficacy (", if (washed) "washed" else "unwashed", "):\n", sep = "")
        print(round(efficacy, 3))
        if (!is.null(alpha_0))
            cat("HostSeekingKilling rescaled by the control alpha_0 (unwashed ",
                alpha_0[["Unwashed"]], ", washed ", alpha_0[["Washed"]], ")\n",
                sep = "")
        cat("Insecticide decay, linear time to zero (years; omitted = no decay):\n")
        if (length(halflife_insecticide) > 0) print(round(halflife_insecticide, 3))
        else cat("  (none - all components constant)\n")
    }

    list(efficacy             = efficacy,
         halflife_insecticide = halflife_insecticide,
         mode                 = mode,
         name                 = name,
         reference            = ref,
         alpha_0              = alpha_0)
}


# Internal: control-arm availability alpha_0 (Unwashed and Washed fits) of an
# experimental hut trial, matched on EHT and, when a trial has several control
# rows, on the reference.
control_alpha_0 = function(db, EHT, reference) {
    ctrl = db[db$netType == "control" & db$EHT == EHT &
                  db$parameter == "alpha_0", ]
    if (nrow(ctrl) > 1L) ctrl = ctrl[ctrl$reference == reference, ]
    if (nrow(ctrl) != 1L)
        stop("Cannot identify a unique control alpha_0 for EHT '", EHT,
             "' (reference '", reference, "'): found ", nrow(ctrl),
             " matching control rows.")
    c(Unwashed = ctrl$Unwashed, Washed = ctrl$Washed)
}
