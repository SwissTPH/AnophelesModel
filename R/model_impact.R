# Core functions for calculating entomological model outputs and vectorial
# capacity reduction for one or more intervention objects.

# Internal: shared time-loop engine used by calculate_impact(),
# calculate_combined_impact() and run_interventions().
#
# eff  — time-varying parameters of protected humans, each [np x npoints]:
#        alphai_1, PBi_1, PCi_1, PDi_1, PEi_1, surv_mat (attrition survival),
#        alphai_4 (dummy-host availability per protected human, used only
#        when use_dummy is TRUE) and use_dummy (logical).
# base — constant parameters of the other host types: alpha, PBi, PCi, PDi,
#        PEi as c(unprotected humans, animals), and Kvi as
#        c(protected humans, unprotected humans, animals).
# coverage_vec — coverages to evaluate. Coverage 0 is the VC baseline for
#        the impact; it is added internally when absent and then dropped
#        from the output.
# keep_time — also return the per-timepoint vc / impact for the first
#        parameter set as [npoints x n_cov] matrices.
#
# Unit host populations (1 human, 1 animal): avg_impact is scale-invariant
# and Nv0 cancels, so Nv0 = 1 and N_total = 2.
#
# Returns list(avg_vc, avg_impact [np x n_cov], coverage, vc_t, impact_t).
simulate_vc = function(eff, base, vec_p, host_p, coverage_vec,
                       keep_time = FALSE) {
    np        = nrow(eff$alphai_1)
    npoints   = ncol(eff$alphai_1)
    use_dummy = isTRUE(eff$use_dummy)

    has_zero  = 0 %in% coverage_vec
    cov_eval  = if (has_zero) coverage_vec else c(0, coverage_vec)
    zero_col  = which(cov_eval == 0)[1L]
    n_cov     = length(cov_eval)
    keep_cols = if (has_zero) seq_len(n_cov) else seq_len(n_cov)[-zero_col]

    n_rows    = np * n_cov
    param_idx = rep(seq_len(np),    each  = n_cov)
    cov_idx   = rep(seq_len(n_cov), times = np)

    Kvi_big = rep_row(if (use_dummy) c(base$Kvi, 0) else base$Kvi, n_rows)

    vc_sum     = matrix(0, np, n_cov)
    impact_sum = matrix(0, np, n_cov)
    if (keep_time) {
        vc_t     = matrix(NA_real_, npoints, length(keep_cols))
        impact_t = matrix(NA_real_, npoints, length(keep_cols))
    }

    # Protected-human values at the current timepoint t, one per output row
    prot = function(m) m[param_idx, t]

    for (t in seq_len(npoints)) {
        frac1 = cov_eval[cov_idx] * prot(eff$surv_mat)

        if (use_dummy) {
            alphai_big = cbind(prot(eff$alphai_1), base$alpha[1], base$alpha[2],
                               prot(eff$alphai_4))
            PBi_big  = cbind(prot(eff$PBi_1), base$PBi[1], base$PBi[2], 0)
            PCi_big  = cbind(prot(eff$PCi_1), base$PCi[1], base$PCi[2], 0)
            PDi_big  = cbind(prot(eff$PDi_1), base$PDi[1], base$PDi[2], 0)
            PEi_big  = cbind(prot(eff$PEi_1), base$PEi[1], base$PEi[2], 0)
            frac_big = cbind(frac1, 1 - frac1, 1, frac1)
        } else {
            alphai_big = cbind(prot(eff$alphai_1), base$alpha[1], base$alpha[2])
            PBi_big  = cbind(prot(eff$PBi_1), base$PBi[1], base$PBi[2])
            PCi_big  = cbind(prot(eff$PCi_1), base$PCi[1], base$PCi[2])
            PDi_big  = cbind(prot(eff$PDi_1), base$PDi[1], base$PDi[2])
            PEi_big  = cbind(prot(eff$PEi_1), base$PEi[1], base$PEi[2])
            frac_big = cbind(frac1, 1 - frac1, 1)
        }

        ent = eval_entomological_model(
            list(PBi = PBi_big, PCi = PCi_big,
                 PDi = PDi_big, PEi = PEi_big, Kvi = Kvi_big),
            vec_p, host_p, 1L, alphai_big * frac_big, frac_big / 2L, 2L)

        vc_mat     = matrix(ent$vc_all, nrow = np, ncol = n_cov, byrow = TRUE)
        impact_mat = 1 - vc_mat / vc_mat[, zero_col]
        impact_mat[!is.finite(impact_mat)] = 0

        vc_sum     = vc_sum     + vc_mat
        impact_sum = impact_sum + impact_mat
        if (keep_time) {
            vc_t[t, ]     = vc_mat[1L, keep_cols]
            impact_t[t, ] = impact_mat[1L, keep_cols]
        }
    }

    list(avg_vc     = vc_sum[, keep_cols, drop = FALSE]     / npoints,
         avg_impact = impact_sum[, keep_cols, drop = FALSE] / npoints,
         coverage   = cov_eval[keep_cols],
         vc_t       = if (keep_time) vc_t     else NULL,
         impact_t   = if (keep_time) impact_t else NULL)
}

# Internal: validate exposure factor(s) applied to coverage.
check_exposure = function(exposure, scalar = FALSE) {
    if (!is.numeric(exposure) || anyNA(exposure) ||
        any(exposure < 0 | exposure > 1) ||
        (scalar && length(exposure) != 1L))
        stop("exposure must be ", if (scalar) "a single number" else "numeric",
             " in [0, 1]")
    invisible(exposure)
}

# Internal: constant parameters of unprotected humans and animals (base
# argument of simulate_vc) from host parameters of build_model_obj().
host_base = function(host_p) {
    list(alpha = host_p$alphai[1:2],
         PBi   = host_p$PBi[1:2], PCi = host_p$PCi[1:2],
         PDi   = host_p$PDi[1:2], PEi = host_p$PEi[1:2],
         Kvi   = c(host_p$Kvi[1], host_p$Kvi[1:2]))
}

#########calculate_impact#####
#' @title Calculate vectorial capacity reduction for a list of interventions
#'
#' @description For each intervention object in \code{interventions_vec},
#' evaluates the entomological model at every interpolation point and returns
#' average vectorial capacity and impact across the deployment period.
#'
#' @param interventions_vec List of intervention objects created with
#' \code{build_intervention()}.
#' @param coverage_vec Numeric vector of deployment coverages (values in
#' \code{[0, 1]}). The impact is always computed relative to coverage 0 (no
#' intervention), which is evaluated internally when not supplied.
#' @param model_p Model object created with \code{build_model_obj()}.
#' @param exposure Proportion of human exposure to bites that occurs where
#' the intervention protects, in \code{[0, 1]} (default 1). The intervention
#' acts on an effective coverage \code{coverage_vec * exposure}. Typically
#' \code{calculate_exposure(...)$Exposure_Indoor_whileinbed} for ITNs or
#' \code{$Exposure_Indoor_total} for IRS. Reported coverages remain the
#' deployment coverages.
#' @return List with:
#' \itemize{
#'   \item \code{vec_p}: vector parameters from \code{model_p}
#'   \item \code{host_p}: host parameters from \code{model_p}
#'   \item \code{interventions_vec}: input list augmented with, per intervention:
#'   \itemize{
#'     \item \code{effects$vc}: vectorial capacity matrix
#'       (time points rows x \code{length(coverage_vec)} cols)
#'     \item \code{effects$impact}: VC reduction matrix (same dimensions)
#'     \item \code{effects$avg_vc}: column means of \code{vc}
#'     \item \code{effects$avg_impact}: column means of \code{impact}
#'     \item \code{coverages}: the supplied \code{coverage_vec}
#'   }
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briet
#' @author Nakul Chitnis
#' @author Tom Smith
#'
#' @export
#'
calculate_impact = function(interventions_vec, coverage_vec, model_p,
                            exposure = 1) {
    check_exposure(exposure, scalar = TRUE)
    for (i in seq_along(interventions_vec)) {
        e    = interventions_vec[[i]]$effects
        rows = seq_len(interventions_vec[[i]]$duration)

        # Unprotected humans and animals (columns 2-3) must be time-constant
        for (nm in c("alphai", "PBi", "PCi", "PDi", "PEi")) {
            m = e[[nm]][rows, 2:3, drop = FALSE]
            if (any(apply(m, 2, function(x) diff(range(x))) > 1e-12))
                stop("calculate_impact: ", nm, " of unprotected humans / ",
                     "animals (columns 2-3) must be constant over time")
        }

        n_host = ncol(e$alphai)
        row1   = function(nm, k = 1L) matrix(e[[nm]][rows, k], nrow = 1L)
        eff = list(alphai_1  = row1("alphai"),
                   PBi_1     = row1("PBi"), PCi_1 = row1("PCi"),
                   PDi_1     = row1("PDi"), PEi_1 = row1("PEi"),
                   alphai_4  = if (n_host == 4L) row1("alphai", 4L) else NULL,
                   surv_mat  = matrix(e$survival[rows], nrow = 1L),
                   use_dummy = n_host == 4L)
        base = list(alpha = e$alphai[1, 2:3],
                    PBi   = e$PBi[1, 2:3], PCi = e$PCi[1, 2:3],
                    PDi   = e$PDi[1, 2:3], PEi = e$PEi[1, 2:3],
                    Kvi   = e$Kvi[1:3])

        sim = simulate_vc(eff, base, model_p$vec_params, model_p$host_params,
                          coverage_vec * exposure, keep_time = TRUE)

        interventions_vec[[i]]$coverages          = coverage_vec
        interventions_vec[[i]]$effects$vc         = sim$vc_t
        interventions_vec[[i]]$effects$impact     = sim$impact_t
        interventions_vec[[i]]$effects$avg_vc     = as.vector(sim$avg_vc)
        interventions_vec[[i]]$effects$avg_impact = as.vector(sim$avg_impact)
    }

    return(list(vec_p = model_p$vec_params, host_p = model_p$host_params,
                interventions_vec = interventions_vec))
}


#########calculate_impact_timepoint#########
#' @title Evaluate the entomological model at a single time point
#'
#' @description Thin wrapper around \code{eval_entomological_model} for
#' evaluating the model at one interpolation point given an explicit host-count
#' matrix.
#'
#' @param intervention_obj Intervention effects object (a single time-point
#' slice: scalars/vectors for \code{alphai}, \code{PBi}, etc.).
#' @param model_obj Model object from \code{build_model_obj()}.
#' @param n_vec Number of daily emerging mosquitoes.
#' @param Ni Matrix of host fractions (rows = coverages, cols = host types),
#'   consistent with the unit-population convention used by
#'   \code{build_model_obj()}: protected humans in [0,1], unprotected humans
#'   in [0,1] (summing to 1), animals = 1.
#'
#' @return List of entomological outputs from \code{eval_entomological_model}:
#' \code{Pf_all}, \code{or_all}, \code{sr_all}, \code{sigmai_all},
#' \code{eiri_all}, \code{vc_all}, \code{f_all}, \code{PAmu_all}.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @export
#'
calculate_impact_timepoint = function(intervention_obj, model_obj, n_vec, Ni) {
    N_total   = sum(Ni[1, ])   # total units from user-supplied host fractions
    t_avail   = rep_row(intervention_obj$alphai, nrow(Ni)) * Ni
    host_frac = Ni / N_total
    interv    = intervention_obj[setdiff(names(intervention_obj), "alphai")]
    eval_entomological_model(interv, model_obj$vec_params,
                             model_obj$host_params, n_vec,
                             t_avail, host_frac, N_total)
}


#########calculate_combined_impact#########
#' @title Calculate impact for a combination of two interventions
#'
#' @description Computes the mean reduction in vectorial capacity when two
#' interventions are deployed simultaneously at specified coverages. Effects
#' are combined multiplicatively (relative to the unintervened baseline).
#'
#' Dummy-host (4-host) interventions are supported, also combined with
#' standard ones. Host-seeking killing is \strong{additive}: each product acts
#' as its own dummy host, so for people covered by both interventions the
#' dummy-host availabilities add up.
#'
#' The two interventions are evaluated over the shorter of their durations.
#'
#' Unlike \code{calculate_impact()}, there is no \code{exposure} argument:
#' the protected exposure of two products overlaps (e.g. in-bed exposure for
#' ITNs is part of indoor exposure for IRS), so supply exposure-adjusted
#' coverages (e.g. \code{coverage * Exposure_Indoor_whileinbed}) directly.
#'
#' @param combination_name String label for the combination.
#' @param intervention1 Intervention object from \code{build_intervention()}.
#' @param intervention2 Intervention object from \code{build_intervention()}.
#' @param cov_intervention1 Coverage of intervention 1 (scalar in \code{[0,1]}).
#' @param cov_intervention2 Coverage of intervention 2 (scalar in \code{[0,1]}).
#' @param cov_combination Coverage of the overlap (default \code{NULL} sets it
#' to \code{cov_intervention1 * cov_intervention2}).
#' @param model_p Model object from \code{build_model_obj()}.
#'
#' @return Mean reduction in vectorial capacity (scalar).
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Tom Smith
#' @author Clara Champagne
#'
#' @export
#'
calculate_combined_impact = function(combination_name = "combination",
                                     intervention1, intervention2,
                                     cov_intervention1, cov_intervention2,
                                     cov_combination = NULL,
                                     model_p) {
    C_i  = cov_intervention1
    C_j  = cov_intervention2
    C_ij = if (is.null(cov_combination)) C_i * C_j else cov_combination

    Cov = c(1 - C_i - C_j + C_ij, C_i - C_ij, C_j - C_ij, C_ij)
    message(combination_name, ": assigned coverages ", paste(Cov, collapse = " "))
    if (min(Cov) < 0) {
        stop("\nCoverage of combination must be between ",
             max((C_i + C_j - 1), 0), " and ", min(C_i, C_j))
    }

    # One point per day: evaluate over the shorter of the two durations
    rows = seq_len(min(intervention1$duration, intervention2$duration))
    e1   = intervention1$effects
    e2   = intervention2$effects

    # Baseline (unintervened) parameters from intervention 1
    alphau = e1$alphai[rows, 2]
    P_Bu   = e1$PBi[rows, 2]
    P_Cu   = e1$PCi[rows, 2]
    P_Du   = e1$PDi[rows, 2]
    P_Eu   = e1$PEi[rows, 2]

    # Intervention 1 parameters
    alphai = e1$alphai[rows, 1]
    P_Bi   = e1$PBi[rows, 1]
    P_Ci   = e1$PCi[rows, 1]
    P_Di   = e1$PDi[rows, 1]
    P_Ei   = e1$PEi[rows, 1]
    S_i    = e1$survival[rows]

    # Intervention 2 parameters
    alphaj = e2$alphai[rows, 1]
    P_Bj   = e2$PBi[rows, 1]
    P_Cj   = e2$PCi[rows, 1]
    P_Dj   = e2$PDi[rows, 1]
    P_Ej   = e2$PEi[rows, 1]
    S_j    = e2$survival[rows]

    # Relative efficacies
    R_alphai = alphai / alphau; R_alphaj = alphaj / alphau
    R_Bi = P_Bi / P_Bu;        R_Bj = P_Bj / P_Bu
    R_Ci = P_Ci / P_Cu;        R_Cj = P_Cj / P_Cu
    R_Di = P_Di / P_Du;        R_Dj = P_Dj / P_Du
    R_Ei = P_Ei / P_Eu;        R_Ej = P_Ej / P_Eu

    # Combined effects (multiplicative)
    alphaij = alphau * R_alphai * R_alphaj
    P_Bij   = P_Bu   * R_Bi * R_Bj
    P_Cij   = P_Cu   * R_Ci * R_Cj
    P_Dij   = P_Du   * R_Di * R_Dj
    P_Eij   = P_Eu   * R_Ei * R_Ej

    C_iEff  = C_i  * S_i
    C_jEff  = C_j  * S_j
    C_ijEff = C_ij * S_i * S_j
    CovEff  = cbind(1 - C_iEff - C_jEff + C_ijEff,
                    C_iEff  - C_ijEff,
                    C_jEff  - C_ijEff,
                    C_ijEff)

    # Weighted-average parameters for the two-host-type model
    alphaw = CovEff[,1]*alphau  + CovEff[,2]*alphai  + CovEff[,3]*alphaj  + CovEff[,4]*alphaij
    PBw  = (CovEff[,1]*alphau*P_Bu + CovEff[,2]*alphai*P_Bi +
            CovEff[,3]*alphaj*P_Bj + CovEff[,4]*alphaij*P_Bij) / alphaw
    PCw  = (CovEff[,1]*alphau*P_Bu*P_Cu + CovEff[,2]*alphai*P_Bi*P_Ci +
            CovEff[,3]*alphaj*P_Bj*P_Cj + CovEff[,4]*alphaij*P_Bij*P_Cij) / (PBw * alphaw)
    PDw  = (CovEff[,1]*alphau*P_Bu*P_Cu*P_Du + CovEff[,2]*alphai*P_Bi*P_Ci*P_Di +
            CovEff[,3]*alphaj*P_Bj*P_Cj*P_Dj + CovEff[,4]*alphaij*P_Bij*P_Cij*P_Dij) /
           (PCw * PBw * alphaw)
    PEw  = (CovEff[,1]*alphau*P_Bu*P_Cu*P_Du*P_Eu + CovEff[,2]*alphai*P_Bi*P_Ci*P_Di*P_Ei +
            CovEff[,3]*alphaj*P_Bj*P_Cj*P_Dj*P_Ej + CovEff[,4]*alphaij*P_Bij*P_Cij*P_Dij*P_Eij) /
           (PDw * PCw * PBw * alphaw)

    # Dummy hosts (host-seeking killing), additive for people covered by both
    d_i = if (ncol(e1$alphai) == 4L) e1$alphai[rows, 4] else 0
    d_j = if (ncol(e2$alphai) == 4L) e2$alphai[rows, 4] else 0
    dummyw = CovEff[,2]*d_i + CovEff[,3]*d_j + CovEff[,4]*(d_i + d_j)

    # The four coverage groups (incl. survival) collapse exactly into a single
    # protected host type with the weighted parameters, evaluated at coverage 1
    as_row = function(x) matrix(x, nrow = 1L)
    eff = list(alphai_1  = as_row(alphaw),
               PBi_1     = as_row(PBw), PCi_1 = as_row(PCw),
               PDi_1     = as_row(PDw), PEi_1 = as_row(PEw),
               alphai_4  = as_row(dummyw),
               surv_mat  = as_row(rep(1, length(rows))),
               use_dummy = ncol(e1$alphai) == 4L || ncol(e2$alphai) == 4L)
    base = list(alpha = e1$alphai[1, 2:3],
                PBi   = e1$PBi[1, 2:3], PCi = e1$PCi[1, 2:3],
                PDi   = e1$PDi[1, 2:3], PEi = e1$PEi[1, 2:3],
                Kvi   = e1$Kvi[1:3])

    sim = simulate_vc(eff, base, model_p$vec_params, model_p$host_params,
                      coverage_vec = 1)
    return(sim$avg_impact[1, 1])
}
