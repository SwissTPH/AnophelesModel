#################################
# internal_entomological_model
#
# Calculates the entomological parameters for various coverages and host types
#
# Based on the entomological model implementation code from Nakul Chitnis,
# Olivier Briët, Tom Smith, and Peter Pemberton-Ross
#
# created on 30.10.2018 by Monica Golumbeanu
# monica.golumbeanu@unibas.ch
#################################

##############################
# Internal function calc_hosts_ent_params: calculates for each host type their
# availability to mosquitoes (alphai) and the mosquito death rate (muvA).
#
# INPUT:
#       vec_p: list object with the vector parameters
#       host_p: list object with the host parameters
#       maxpop: maximum population size
# OUTPUT: list object with attributes:
#       muvA: mosquito death rate
#       alphai: availability to mosquitoes for each host type
#############################
calc_hosts_ent_params = function(vec_p, h_p) {
    # Unit host populations: one unprotected human, one animal.
    # alphai is per-individual-host availability; results are scale-invariant.
    Ni = c(0L, 1L, 1L)
    # three types of hosts, two types are not affected by interventions
    m = 1; n = 2
    zetai = c(rep(NA, m),rep(vec_p$zeta.3,n-m))
    PA = 1 - vec_p$A0

    # Assuming that there are only unprotected humans and non-human hosts
    # in the absence of interventions (eq. 14&15 Chitnis et al 2010) :
    PA1 = vec_p$A0 * vec_p$M * vec_p$Chi *
        sum(zetai[c(m+1):n] * h_p$PBi[c(m+1):n] * h_p$PCi[c(m+1):n]) /
        (h_p$PBi[m] *h_p$PCi[m] *
             sum((vec_p$Chi * h_p$PDi[m] * h_p$PEi[m] + (1-vec_p$Chi) *
                      h_p$PDi[c(m+1):n] * h_p$PEi[c(m+1):n]) * zetai[c(m+1):n] *
                     h_p$PBi[c(m+1):n] * h_p$PCi[c(m+1):n]))

    PAh = (vec_p$A0 * vec_p$M * (1 - vec_p$Chi)) / sum(zetai[c(m+1):n] *
            h_p$PBi[c(m+1):n] * h_p$PCi[c(m+1):n] *
            (vec_p$Chi*h_p$PDi[m] * h_p$PEi[m] + (1-vec_p$Chi) *
            h_p$PDi[c(m+1):n] * h_p$PEi[c(m+1):n]))

    # Probability that a mosquito encounters a host of type i on a given night
    # (eq. 8 Chitnis et al 2010)
    PAi = c(PA1, zetai[c(m+1):n]*PAh)
    Ni.nointerventions = c(Ni[1]+Ni[2], Ni[3])

    # Availability rate of each individual host of type i to mosquitoes
    # (eq. 26 Chitnis et al 2010)
    alphai = (1/Ni.nointerventions)*(PAi/(1-PA))*(-log(PA)/vec_p$td)
    # alphai[1] = alphai[2]

    # Per capita death rate of a mosquito while searching for a blood meal
    # in the absence of interventions (eq. 27 Chitnis et al 2010)
    muvA = ((1 - (PA + sum(PAi[m:n]))) / (1 - PA)) * (-log(PA) / vec_p$td)

    if(muvA < 0) {
        stop("Simulated mosquito dynamics during the feeding cycle is not reflected by the mosquito bionomics parameters!
        Consider increasing the values for PBi, PCi, PDi, PEi in the def_host_param() function call.",
             call. = FALSE)
    }
    h_p$muvA = muvA
    h_p$alphai = alphai
    return(h_p)
}

# Calculates the probability that a mosquito survives the extrinsic
# incubation period (eq. 3c Chitnis et al. 2007). This probability is then used
# to calculate the sporozoite rate and the vectorial capacity.
#
# The formula is defined for a whole number of days per feeding cycle, so the
# resting duration tau (estimated, non-integer) is rounded to whole days here,
# with at least 1 day (floor(tau + 0.5), not round(), which rounds halves to
# even). The exact tau is used elsewhere (e.g. the feeding cycle duration f).
calc_psts = function(ts, tau, PA, Pdf) {
    tau = max(1, floor(tau + 0.5))
    # TO DO: add this in the general checking function
    if(ts < tau) {
        ts = tau
        # TO DO: check why this happens for some data entries
        #stop("ts is less than tau. Check parameter values.");
    }
    n_cov = length(PA)
    # eq. 4 Chitnis et al. 2007
    kplus = floor(ts/tau) - 1
    # An upper index of zero still contributes the term at k=0.
    # Build the index sequence only for non-empty sums.
    if (kplus >= 0){
        kp_v = seq.int(0L, kplus)
        s_kplus = rowSums(rep_row(choose(ts - (kp_v+1) * tau + kp_v, kp_v), n_cov) *
                              rep_col(PA, kplus+1) ^ rep_row(ts - (kp_v+1) * tau, n_cov) *
                              rep_col(Pdf, kplus+1) ^ rep_row(kp_v, n_cov))
    } else {
        s_kplus = rep(0, n_cov)
    }

    # Sum over the days l = 1, ..., tau - 1 of a feeding cycle (no terms when
    # tau = 1 day)
    suml = rep(0, n_cov)
    sumlv = matrix(0, nrow = tau-1, ncol = n_cov);
    for(l in seq_len(tau - 1)) {
        # eq. 4 Chitnis et al. 2007
        klplus = floor((ts + l) / tau) - 2
        if (klplus >= 0) {
            klp_v = seq.int(0L, klplus)
            s_klplus = rowSums(rep_row(choose(ts + l - (klp_v+2) * tau + klp_v, klp_v), n_cov) *
                                   rep_col(PA, klplus+1) ^ rep_row(ts + l - (klp_v + 2) * tau, n_cov) *
                                   rep_col(Pdf, klplus+1) ^ rep_row(klp_v+1, n_cov))
        } else {
            s_klplus = rep(0, n_cov)
        }
        sumlv[l, ] = s_klplus
        suml = suml + s_klplus
    }

    psts = s_kplus + suml
    result = list(psts = psts, sumkplus = s_kplus, sumlv = sumlv)
    return(result)
}

# Calculates entomological parameters for all coverages and host types for
# a given intervention.
#
# Scale-insensitive parameterisation:
#   t_avail  — n_rows x n_host matrix of total host availabilities (alphai * Ni).
#              Passed in by the caller; alphai is no longer an argument.
#   host_frac       — n_rows x n_host matrix of host-count proportions (Ni / N_total).
#   N_total  — scalar total host population (used only in sigmai and vc).
#
# interv must contain PBi, PCi, PDi, PEi, Kvi; each may be a vector of length
# n_host (expanded internally) or an already-expanded n_rows x n_host matrix.
eval_entomological_model = function(interv, vec_p, host_p, Nv0,
                                    t_avail, host_frac, N_total) {
    n_rows       = nrow(t_avail)
    n_host_types = ncol(t_avail)

    # Expand per-host vectors to full n_rows x n_host matrices when the
    # caller passes scalar-per-host values; leave matrices untouched.
    if (is.vector(interv$PBi)) {
        PBi_m = rep_row(interv$PBi, n_rows)
        PCi_m = rep_row(interv$PCi, n_rows)
        PDi_m = rep_row(interv$PDi, n_rows)
        PEi_m = rep_row(interv$PEi, n_rows)
        Kvi_m = rep_row(interv$Kvi, n_rows)
    } else {
        PBi_m = interv$PBi
        PCi_m = interv$PCi
        PDi_m = interv$PDi
        PEi_m = interv$PEi
        Kvi_m = interv$Kvi
    }

    # Probability that the mosquito:
    # is still host seeking after time td
    PA = exp(-(rowSums(t_avail) + host_p$muvA) * vec_p$td)
    # finds host i
    PAi = (1-PA) * t_avail / (rowSums(t_avail) + host_p$muvA)
    # dies while host seeking
    PAmu = 1 - (PA + rowSums(PAi))
    # finds a host on a given night and survives a complete feeding cycle
    Pdf = rowSums(PAi * PBi_m * PCi_m * PDi_m * PEi_m)
    # surviving a feeding cycle (eq. 2 Chitnis et al. 2007)
    Pf = Pdf / (1-PA)
    # finds a host on a given day, survives and does not get infected
    Pduf = rowSums(PAi * PBi_m * PCi_m * PDi_m * PEi_m * (1 - Kvi_m))
    # survives a feeding cycle and does not get infected
    Puf = Pduf / (1-PA)
    # finds a host on a given night and then survives and gets infected
    Pdif = Pdf - Pduf
    # survives a feeding cycle and gets infected
    Pif = Pf - Puf

    # total number of host-seeking mosquitoes (eq. 6a Chitnis et al. 2007)
    Nv = Nv0 / (1-PA-Pdf)
    # delayed oocyst rate
    or = (Pf-Puf) / (1-Puf)

    res = calc_psts(vec_p$ts, vec_p$tau, PA, Pdf)
    psts     = res$psts
    sumkplus = res$sumkplus
    sumlv    = res$sumlv
    sr = (Pdif/(1 - PA - Pdf)) * (1 - Pdif / (1 - PA - Pduf)) * psts

    # host biting rate (eq. 15 Chitnis et al 2007); host_frac * N_total = Ni
    sigmai = PAi * PBi_m * rep_col(Nv, n_host_types) /
             (host_frac * N_total * (host_frac > 0))

    # EIR (eq. 16 Chitnis et al. 2007)
    eiri = sigmai * rep_col(sr, n_host_types)

    # vectorial capacity (eq. 21 Chitnis et al 2007);
    # N_total * rowSums(host_frac[,1:2]) = rowSums(Ni[,1:2])
    vc = Nv0 / (N_total * rowSums(host_frac[, 1:2])) *
         1/((1-PA-Pdf)^2) * psts *
         rowSums(PAi[, 1:2] * PBi_m[, 1:2]) *
         rowSums(PAi[, 1:2] * PBi_m[, 1:2] * PCi_m[, 1:2] *
                              PDi_m[, 1:2] * PEi_m[, 1:2])

    # duration of the feeding cycle (eq. 23 Chitnis et al 2007)
    f = vec_p$tau + PA / (1 - PA)

    res = list(Pf_all = Pf, or_all = or, sr_all = sr, sigmai_all = sigmai,
               eiri_all = eiri, vc_all = vc, f_all = f, PAmu_all = PAmu)
    return(res)
}
