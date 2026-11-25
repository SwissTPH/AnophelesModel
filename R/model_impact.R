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
# keep_time — also return the per-timepoint vc / impact of all parameter
#        sets as [np x n_cov x npoints] arrays.
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
        vc_t     = array(NA_real_, c(np, length(keep_cols), npoints))
        impact_t = array(NA_real_, c(np, length(keep_cols), npoints))
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

        vc_mat = matrix(ent$vc_all, nrow = np, ncol = n_cov, byrow = TRUE)
        baseline_vc = vc_mat[, zero_col]
        if (any(!is.finite(baseline_vc)))
            stop("simulate_vc: non-finite baseline vectorial capacity at time point ", t,
                 where_rows(!is.finite(baseline_vc)))
        if (any(baseline_vc == 0))
            stop("simulate_vc: baseline vectorial capacity is zero at time point ", t,
                 where_rows(baseline_vc == 0), "; relative impact is undefined")
        if (any(baseline_vc < 0))
            stop("simulate_vc: negative baseline vectorial capacity at time point ", t,
                 where_rows(baseline_vc < 0))
        if (any(!is.finite(vc_mat)))
            stop("simulate_vc: non-finite vectorial capacity at time point ", t,
                 where_rows(rowSums(!is.finite(vc_mat)) > 0))
        impact_mat = 1 - vc_mat / baseline_vc
        if (any(!is.finite(impact_mat)))
            stop("simulate_vc: non-finite relative impact at time point ", t,
                 where_rows(rowSums(!is.finite(impact_mat)) > 0))

        vc_sum     = vc_sum     + vc_mat
        impact_sum = impact_sum + impact_mat
        if (keep_time) {
            vc_t[, , t]     = vc_mat[, keep_cols]
            impact_t[, , t] = impact_mat[, keep_cols]
        }
    }

    list(avg_vc     = vc_sum[, keep_cols, drop = FALSE]     / npoints,
         avg_impact = impact_sum[, keep_cols, drop = FALSE] / npoints,
         coverage   = cov_eval[keep_cols],
         vc_t       = if (keep_time) vc_t     else NULL,
         impact_t   = if (keep_time) impact_t else NULL)
}

# Internal: " for parameter set(s) i, j, ..." from a logical vector over the
# parameter sets (rows of param_sets) of simulate_vc(), for error messages.
where_rows = function(bad) {
    idx = which(bad)
    shown = paste(utils::head(idx, 10L), collapse = ", ")
    paste0(" for parameter set(s) ", shown,
           if (length(idx) > 10L) paste0(" and ", length(idx) - 10L, " more") else "")
}

# Internal: validate exposure factor(s) applied to coverage.
check_exposure = function(exposure, scalar = FALSE) {
    if (!is.numeric(exposure) || !is.null(dim(exposure)) || length(exposure) == 0L ||
        any(!is.finite(exposure)) ||
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
#' evaluates the entomological model on every day of the intervention and
#' returns the average vectorial capacity and impact (reduction in
#' vectorial capacity) at each coverage. This is
#' \code{\link{run_interventions}} applied to the parameters stored in each
#' intervention (\code{$param_set}), so both functions give the same results
#' and the same output format.
#'
#' The model parameters are those of \code{model_p} (the \code{model_p} used
#' to build the interventions does not matter).
#'
#' @param interventions_vec Intervention objects created with
#' \code{build_intervention()}, combined with \code{c()} (e.g.
#' \code{c(itn1, itn2)}); a single \code{build_intervention()} output works
#' as is.
#' @param coverage_vec Numeric vector of deployment coverages (values in
#' \code{[0, 1]}). The impact is always computed relative to coverage 0 (no
#' intervention), which is evaluated internally when not supplied.
#' @param model_p Model object created with \code{build_model_obj()}.
#' @param exposure Proportion of human exposure to bites that occurs where
#' the intervention protects, in \code{[0, 1]} (default 1). The intervention
#' acts on an effective coverage \code{coverage_vec * exposure}. Typically
#' \code{calculate_exposure(...)$Exposure_Indoor_whileinbed} for ITNs, and
#' \code{$indoor_resting} or \code{$Exposure_Indoor_total} for IRS (see
#' \code{\link{calculate_exposure}}). Reported coverages remain the
#' deployment coverages.
#' @param keep_time Logical (default \code{FALSE}): also return the
#' vectorial capacity and impact on each day.
#'
#' @return A \code{data.table} with one row per intervention and coverage:
#' \code{intervention} (position in \code{interventions_vec}), \code{name},
#' the intervention parameters (as in \code{run_interventions()}),
#' \code{coverage}, \code{exposure}, \code{avg_vc} and \code{avg_impact}.
#' With \code{keep_time = TRUE}, a list with this table as \code{impact} and a
#' long table \code{time} with \code{intervention}, \code{name},
#' \code{coverage}, \code{day} (0 = deployment), \code{vc} and \code{impact}.
#' Plot it with \code{\link{plot_impact}}.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briet
#' @author Nakul Chitnis
#' @author Tom Smith
#'
#' @export
#'
calculate_impact = function(interventions_vec, coverage_vec, model_p,
                            exposure = 1, keep_time = FALSE) {
    check_coverage(coverage_vec)
    check_exposure(exposure, scalar = TRUE)
    if (!is.list(interventions_vec) || length(interventions_vec) == 0L)
        stop("interventions_vec must be a non-empty list of interventions ",
             "from build_intervention()")
    impact = vector("list", length(interventions_vec))
    time   = vector("list", length(interventions_vec))

    for (i in seq_along(interventions_vec)) {
        iv = interventions_vec[[i]]
        if (!is.list(iv) || is.null(iv$param_set))
            stop("calculate_impact, intervention ", i, ": not an intervention ",
                 "from build_intervention() (combine interventions with c(), ",
                 "not list())")
        # One run_interventions() call per intervention (durations and model
        # structures can differ); its model message is redundant here, and
        # errors name the intervention instead of the parameter set
        res = tryCatch(
            suppressMessages(run_interventions(
                iv$param_set, model_p, coverage_vec,
                duration_years    = iv$duration / 365,
                insecticide_decay = !isFALSE(iv$settings$insecticide_decay),
                exposure          = exposure, keep_time = keep_time)),
            error = function(err) stop(
                "calculate_impact, intervention ", i, ": ",
                sub(" for parameter set\\(s\\) 1", "",
                    sub("^run_interventions: ", "", conditionMessage(err))),
                call. = FALSE))
        add_index = function(dt) {
            data.table::set(dt, j = "intervention", value = i)
            data.table::setcolorder(dt, c("intervention", setdiff(names(dt), "intervention")))
            dt
        }
        if (keep_time) {
            impact[[i]] = add_index(res$impact)
            data.table::set(res$time, j = "param_set", value = NULL)   # always 1
            time[[i]]   = add_index(res$time)
        } else {
            impact[[i]] = add_index(res)
        }
    }

    impact = data.table::rbindlist(impact, fill = TRUE)
    if (!keep_time) return(impact)
    list(impact = impact, time = data.table::rbindlist(time, fill = TRUE))
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
#' As in \code{calculate_impact()}, their daily effects are rebuilt from their
#' parameters (\code{$param_set}: efficacies, attrition, insecticide decay,
#' with their duration and \code{insecticide_decay} setting) with the host
#' and vector parameters of \code{model_p}: the model used to build them does
#' not matter.
#'
#' Unlike \code{calculate_impact()}, there is no \code{exposure} argument:
#' the protected exposure of two products overlaps (e.g. in-bed exposure for
#' ITNs is part of indoor exposure for IRS), so supply exposure-adjusted
#' coverages (e.g. \code{coverage * Exposure_Indoor_whileinbed}) directly.
#'
#' @param combination_name String label for the combination.
#' @param intervention1 Intervention object from \code{build_intervention()}
#'   (an element, e.g. \code{interv[[1]]}).
#' @param intervention2 Intervention object from \code{build_intervention()}
#'   (an element, e.g. \code{interv[[1]]}).
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
    check_coverage(cov_intervention1, "cov_intervention1", scalar = TRUE)
    check_coverage(cov_intervention2, "cov_intervention2", scalar = TRUE)
    if (!is.null(cov_combination))
        check_coverage(cov_combination, "cov_combination", scalar = TRUE)
    C_i  = cov_intervention1
    C_j  = cov_intervention2
    C_ij = if (is.null(cov_combination)) C_i * C_j else cov_combination

    Cov = c(1 - C_i - C_j + C_ij, C_i - C_ij, C_j - C_ij, C_ij)
    # Valid overlaps at the bounds (max(0, C_i + C_j - 1), min(C_i, C_j)) can
    # give a group coverage of about -1e-16 in floating point: tolerated (and
    # set to 0 below), anything more negative is an error
    tol = 1e-12
    if (min(Cov) < -tol) {
        stop("\nCoverage of combination must be between ",
             max((C_i + C_j - 1), 0), " and ", min(C_i, C_j))
    }
    Cov = pmax(Cov, 0)
    message(combination_name, ": assigned coverages ", paste(Cov, collapse = " "))

    # Daily effects of both interventions rebuilt from their parameters
    # ($param_set: efficacies, attrition, insecticide decay; duration and
    # insecticide_decay setting kept) with the supplied model, as in
    # calculate_impact(): the model used to build them does not matter.
    # Evaluated over the shorter of the two durations.
    e1 = combined_effects(intervention1, "intervention1", model_p)
    e2 = combined_effects(intervention2, "intervention2", model_p)
    rows = seq_len(min(length(e1$surv), length(e2$surv)))
    h = model_p$host_params

    # Baseline (unintervened) human parameters of the model
    alphau = rep(h$alphai[1], length(rows))
    P_Bu   = rep(h$PBi[1], length(rows))
    P_Cu   = rep(h$PCi[1], length(rows))
    P_Du   = rep(h$PDi[1], length(rows))
    P_Eu   = rep(h$PEi[1], length(rows))

    # Intervention 1 parameters
    alphai = e1$alphai[rows]
    P_Bi   = e1$PBi[rows]
    P_Ci   = e1$PCi[rows]
    P_Di   = e1$PDi[rows]
    P_Ei   = e1$PEi[rows]
    S_i    = e1$surv[rows]

    # Intervention 2 parameters
    alphaj = e2$alphai[rows]
    P_Bj   = e2$PBi[rows]
    P_Cj   = e2$PCi[rows]
    P_Dj   = e2$PDi[rows]
    P_Ej   = e2$PEi[rows]
    S_j    = e2$surv[rows]

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
    # Non-negative for feasible overlaps (survival <= 1); only floating-point
    # noise at the bounds is removed
    CovEff[CovEff < 0 & CovEff > -tol] = 0

    # Conditional probabilities of later feeding stages are irrelevant when
    # no mosquitoes reach the preceding stage. Only this exact 0/0 case is
    # assigned zero; other invalid values must remain visible to simulate_vc.
    conditional_probability = function(numerator, denominator) {
        result = numerator / denominator
        unreachable = is.finite(numerator) & is.finite(denominator) &
                      numerator == 0 & denominator == 0
        result[unreachable] = 0
        result
    }

    # Weighted-average parameters for the two-host-type model
    alphaw = CovEff[,1]*alphau  + CovEff[,2]*alphai  + CovEff[,3]*alphaj  + CovEff[,4]*alphaij
    PBw  = conditional_probability(
        CovEff[,1]*alphau*P_Bu + CovEff[,2]*alphai*P_Bi +
        CovEff[,3]*alphaj*P_Bj + CovEff[,4]*alphaij*P_Bij, alphaw)
    PCw  = conditional_probability(
        CovEff[,1]*alphau*P_Bu*P_Cu + CovEff[,2]*alphai*P_Bi*P_Ci +
        CovEff[,3]*alphaj*P_Bj*P_Cj + CovEff[,4]*alphaij*P_Bij*P_Cij,
        PBw * alphaw)
    PDw  = conditional_probability(
        CovEff[,1]*alphau*P_Bu*P_Cu*P_Du + CovEff[,2]*alphai*P_Bi*P_Ci*P_Di +
        CovEff[,3]*alphaj*P_Bj*P_Cj*P_Dj + CovEff[,4]*alphaij*P_Bij*P_Cij*P_Dij,
        PCw * PBw * alphaw)
    PEw  = conditional_probability(
        CovEff[,1]*alphau*P_Bu*P_Cu*P_Du*P_Eu + CovEff[,2]*alphai*P_Bi*P_Ci*P_Di*P_Ei +
        CovEff[,3]*alphaj*P_Bj*P_Cj*P_Dj*P_Ej + CovEff[,4]*alphaij*P_Bij*P_Cij*P_Dij*P_Eij,
        PDw * PCw * PBw * alphaw)

    # Dummy hosts (host-seeking killing), additive for people covered by both
    d_i = if (e1$use_dummy) e1$dummy[rows] else 0
    d_j = if (e2$use_dummy) e2$dummy[rows] else 0
    dummyw = CovEff[,2]*d_i + CovEff[,3]*d_j + CovEff[,4]*(d_i + d_j)

    # The four coverage groups (incl. survival) collapse exactly into a single
    # protected host type with the weighted parameters, evaluated at coverage 1
    as_row = function(x) matrix(x, nrow = 1L)
    eff = list(alphai_1  = as_row(alphaw),
               PBi_1     = as_row(PBw), PCi_1 = as_row(PCw),
               PDi_1     = as_row(PDw), PEi_1 = as_row(PEw),
               alphai_4  = as_row(dummyw),
               surv_mat  = as_row(rep(1, length(rows))),
               use_dummy = e1$use_dummy || e2$use_dummy)

    sim = simulate_vc(eff, host_base(h), model_p$vec_params, h, coverage_vec = 1)
    return(sim$avg_impact[1, 1])
}

# Internal: daily parameters of protected humans of an intervention from
# build_intervention(), rebuilt from its $param_set with the host parameters
# of model_p (same decay engine as calculate_impact() / run_interventions())
combined_effects = function(iv, what, model_p) {
    if (!is.list(iv) || is.null(iv$param_set) || is.null(iv$duration))
        stop("calculate_combined_impact: ", what, " must be an intervention from ",
             "build_intervention(), e.g. interv[[1]]")
    days = seq_len(iv$duration) - 1L
    mm = tryCatch(
        make_mult_matrices(iv$param_set, days,
                           insecticide_decay = !isFALSE(iv$settings$insecticide_decay)),
        error = function(err) stop("calculate_combined_impact, ", what, ": ",
                                   conditionMessage(err), call. = FALSE))
    eff = apply_eff_matrices(mm, model_p$host_params)
    list(alphai = eff$alphai_1[1L, ], PBi = eff$PBi_1[1L, ], PCi = eff$PCi_1[1L, ],
         PDi = eff$PDi_1[1L, ], PEi = eff$PEi_1[1L, ], surv = eff$surv_mat[1L, ],
         dummy = eff$alphai_4[1L, ], use_dummy = isTRUE(eff$use_dummy))
}
