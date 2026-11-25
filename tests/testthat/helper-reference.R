# Reference implementations used as independent test oracles.
#
# These are verbatim copies of calculate_impact() and calculate_combined_impact()
# as they were before both were rebuilt on the shared simulate_vc() engine
# (step 3 of the refactoring). They loop explicitly over time points and must
# NOT be changed to call package code beyond eval_entomological_model().

ref_rep.row = function(x, n) matrix(rep(x, each = n), nrow = n)

reference_calculate_impact = function(interventions_vec, coverage_vec, model_p) {
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
            Ni3 = N_total - Ni1 - Ni2
            Ni  = if (n_host_cols == 4) cbind(Ni1, Ni2, Ni3, Ni1)
                  else                  cbind(Ni1, Ni2, Ni3)

            interv     = interventions_vec[[i]]$effects
            alphai_ip  = interv$alphai[ip, ]
            interv$PBi = interv$PBi[ip, ]
            interv$PCi = interv$PCi[ip, ]
            interv$PDi = interv$PDi[ip, ]
            interv$PEi = interv$PEi[ip, ]

            n_cov   = nrow(Ni)
            t_avail = ref_rep.row(alphai_ip, n_cov) * Ni
            host_frac = Ni / N_total

            ent = AnophelesModel:::eval_entomological_model(
                interv, model_p$vec_params, model_p$host_params, Nv0,
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

ref_thin_and_truncate = function(vec_in, thin, nips, redeploy = 1) {
    vec_out = vec_in[c(TRUE, rep(FALSE, times = (thin - 1)))]
    vec_out = rep(vec_out, redeploy)
    return(vec_out[seq(1:nips)])
}

ref_replace_effect = function(col1, df0, thin, nips) {
    col2 = ref_thin_and_truncate(t(df0[, 2]), thin = thin, nips = nips)
    col3 = ref_thin_and_truncate(t(df0[, 3]), thin = thin, nips = nips)
    unname(cbind(col1, col2, col3))
}

# Standard (3-host) interventions only.
reference_combined_impact = function(intervention1, intervention2,
                                     cov_intervention1, cov_intervention2,
                                     cov_combination = NULL, model_p) {
    C_i  = cov_intervention1
    C_j  = cov_intervention2
    C_ij = if (is.null(cov_combination)) C_i * C_j else cov_combination

    interval1     = intervention1$duration / length(intervention1$effects$alphai[, 1])
    interval2     = intervention2$duration / length(intervention2$effects$alphai[, 1])
    intervalratio = interval1 / interval2
    if (intervalratio >= 1) { thin1 = intervalratio; thin2 = 1
    } else                  { thin1 = 1; thin2 = 1 / intervalratio }

    duration = min(intervention1$duration, intervention2$duration)
    nips     = duration / max(interval1, interval2)
    tt = ref_thin_and_truncate

    alphau = tt(intervention1$effects$alphai[, 2], thin1, nips)
    P_Bu   = tt(intervention1$effects$PBi[, 2],   thin1, nips)
    P_Cu   = tt(intervention1$effects$PCi[, 2],   thin1, nips)
    P_Du   = tt(intervention1$effects$PDi[, 2],   thin1, nips)
    P_Eu   = tt(intervention1$effects$PEi[, 2],   thin1, nips)

    alphai = tt(intervention1$effects$alphai[, 1], thin1, nips)
    P_Bi   = tt(intervention1$effects$PBi[, 1],   thin1, nips)
    P_Ci   = tt(intervention1$effects$PCi[, 1],   thin1, nips)
    P_Di   = tt(intervention1$effects$PDi[, 1],   thin1, nips)
    P_Ei   = tt(intervention1$effects$PEi[, 1],   thin1, nips)
    S_i    = tt(intervention1$effects$survival,   thin1, nips)

    alphaj = tt(intervention2$effects$alphai[, 1], thin2, nips)
    P_Bj   = tt(intervention2$effects$PBi[, 1],   thin2, nips)
    P_Cj   = tt(intervention2$effects$PCi[, 1],   thin2, nips)
    P_Dj   = tt(intervention2$effects$PDi[, 1],   thin2, nips)
    P_Ej   = tt(intervention2$effects$PEi[, 1],   thin2, nips)
    S_j    = tt(intervention2$effects$survival,   thin2, nips)

    R_alphai = alphai / alphau; R_alphaj = alphaj / alphau
    R_Bi = P_Bi / P_Bu;        R_Bj = P_Bj / P_Bu
    R_Ci = P_Ci / P_Cu;        R_Cj = P_Cj / P_Cu
    R_Di = P_Di / P_Du;        R_Dj = P_Dj / P_Du
    R_Ei = P_Ei / P_Eu;        R_Ej = P_Ej / P_Eu

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

    effects        = list(survival = rep(1, nips), Kvi = intervention1$effects$Kvi)
    effects$alphai = ref_replace_effect(alphaw, intervention1$effects$alphai, thin1, nips)
    effects$PBi    = ref_replace_effect(PBw,    intervention1$effects$PBi,   thin1, nips)
    effects$PCi    = ref_replace_effect(PCw,    intervention1$effects$PCi,   thin1, nips)
    effects$PDi    = ref_replace_effect(PDw,    intervention1$effects$PDi,   thin1, nips)
    effects$PEi    = ref_replace_effect(PEw,    intervention1$effects$PEi,   thin1, nips)

    combination_int = list(effects = effects, coverages = c(0, 1), duration = duration)
    result = reference_calculate_impact(list(combination = combination_int),
                                        c(0, 1), model_p)
    result$interventions_vec$combination$effects$avg_impact[2]
}
