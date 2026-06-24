# Core functions for calculating entomological model outputs and vectorial
# capacity reduction for one or more intervention objects.

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
#' \code{[0, 1]}).
#' @param model_p Model object created with \code{build_model_obj()}.
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
calculate_impact = function(interventions_vec, coverage_vec, model_p) {
    # Unit host populations: 1 unprotected human + 1 animal.
    # alphai is per-individual-host; results (avg_impact) are scale-invariant.
    # Nv0 cancels in avg_impact = 1 - vc/vc_baseline; fixed at 1 internally.
    Nv0     = 1L
    nhuman  = 1L
    N_total = 2L

    for (i in seq_along(interventions_vec)) {
        interventions_vec[[i]]$effects$vc     = NULL
        interventions_vec[[i]]$effects$impact = NULL
        interventions_vec[[i]]$coverages      = coverage_vec

        n_host_cols = ncol(interventions_vec[[i]]$effects$alphai)
        num_ip_points = interventions_vec[[i]]$duration

        for (ip in seq_len(num_ip_points)) {
            Ni1 = coverage_vec * nhuman *
                interventions_vec[[i]]$effects$survival[ip]
            Ni2 = nhuman - Ni1
            Ni3 = N_total - Ni1 - Ni2   # = 1 (one animal)
            # 4th column = dummy hosts (same count as protected humans)
            Ni  = if (n_host_cols == 4) cbind(Ni1, Ni2, Ni3, Ni1)
                  else                  cbind(Ni1, Ni2, Ni3)

            interv     = interventions_vec[[i]]$effects
            alphai_ip  = interv$alphai[ip, ]
            interv$PBi = interv$PBi[ip, ]
            interv$PCi = interv$PCi[ip, ]
            interv$PDi = interv$PDi[ip, ]
            interv$PEi = interv$PEi[ip, ]

            n_cov   = nrow(Ni)
            t_avail = rep.row(alphai_ip, n_cov) * Ni
            host_frac = Ni / N_total

            ent = eval_entomological_model(interv, model_p$vec_params,
                                           model_p$host_params, Nv0,
                                           t_avail, host_frac, N_total)

            interventions_vec[[i]]$effects$vc =
                rbind(interventions_vec[[i]]$effects$vc, ent$vc_all)
            interventions_vec[[i]]$effects$impact =
                rbind(interventions_vec[[i]]$effects$impact,
                      1 - ent$vc_all / ent$vc_all[1])
        }
        interventions_vec[[i]]$effects$avg_vc     =
            colMeans(interventions_vec[[i]]$effects$vc)
        interventions_vec[[i]]$effects$avg_impact =
            colMeans(interventions_vec[[i]]$effects$impact)
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
    t_avail   = rep.row(intervention_obj$alphai, nrow(Ni)) * Ni
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
    cat("Assigned coverages: ", Cov, "\n")
    if (min(Cov) < 0) {
        stop("\nCoverage of combination must be between ",
             max((C_i + C_j - 1), 0), " and ", min(C_i, C_j))
    }

    interval1     = intervention1$duration / length(intervention1$effects$alphai[, 1])
    interval2     = intervention2$duration / length(intervention2$effects$alphai[, 1])
    intervalratio = interval1 / interval2

    if (!identical(round(intervalratio), intervalratio) &
        !identical(round(1 / intervalratio), 1 / intervalratio)) {
        stop("Interpolation point intervals are not in an integral ratio.")
    }

    if (intervalratio >= 1) { thin1 = intervalratio; thin2 = 1
    } else                  { thin1 = 1; thin2 = 1 / intervalratio }

    duration = min(intervention1$duration, intervention2$duration)
    nips     = duration / max(interval1, interval2)

    # Baseline (unintervened) parameters from intervention 1
    alphau = thin_and_truncate(intervention1$effects$alphai[, 2], thin1, nips)
    P_Bu   = thin_and_truncate(intervention1$effects$PBi[, 2],   thin1, nips)
    P_Cu   = thin_and_truncate(intervention1$effects$PCi[, 2],   thin1, nips)
    P_Du   = thin_and_truncate(intervention1$effects$PDi[, 2],   thin1, nips)
    P_Eu   = thin_and_truncate(intervention1$effects$PEi[, 2],   thin1, nips)

    # Intervention 1 parameters
    alphai = thin_and_truncate(intervention1$effects$alphai[, 1], thin1, nips)
    P_Bi   = thin_and_truncate(intervention1$effects$PBi[, 1],   thin1, nips)
    P_Ci   = thin_and_truncate(intervention1$effects$PCi[, 1],   thin1, nips)
    P_Di   = thin_and_truncate(intervention1$effects$PDi[, 1],   thin1, nips)
    P_Ei   = thin_and_truncate(intervention1$effects$PEi[, 1],   thin1, nips)
    S_i    = thin_and_truncate(intervention1$effects$survival,   thin1, nips)

    # Intervention 2 parameters
    alphaj = thin_and_truncate(intervention2$effects$alphai[, 1], thin2, nips)
    P_Bj   = thin_and_truncate(intervention2$effects$PBi[, 1],   thin2, nips)
    P_Cj   = thin_and_truncate(intervention2$effects$PCi[, 1],   thin2, nips)
    P_Dj   = thin_and_truncate(intervention2$effects$PDi[, 1],   thin2, nips)
    P_Ej   = thin_and_truncate(intervention2$effects$PEi[, 1],   thin2, nips)
    S_j    = thin_and_truncate(intervention2$effects$survival,   thin2, nips)

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

    effects            = list(survival = rep(1, nips),
                              Kvi      = intervention1$effects$Kvi)
    effects$alphai     = replace_effect(alphaw, intervention1$effects$alphai, thin1, nips)
    effects$PBi        = replace_effect(PBw,    intervention1$effects$PBi,   thin1, nips)
    effects$PCi        = replace_effect(PCw,    intervention1$effects$PCi,   thin1, nips)
    effects$PDi        = replace_effect(PDw,    intervention1$effects$PDi,   thin1, nips)
    effects$PEi        = replace_effect(PEw,    intervention1$effects$PEi,   thin1, nips)

    combination_int = list(
        description    = list(description1 = intervention1$description,
                              description2 = intervention2$description),
        id             = combination_name,
        parameterisation = combination_name,
        effects        = effects,
        coverages      = c(0, 1),
        duration       = duration
    )

    result = calculate_impact(
        interventions_vec = list(combination = combination_int),
        coverage_vec      = c(0, 1),
        model_p           = model_p
    )
    return(result$interventions_vec$combination$effects$avg_impact[2])
}
