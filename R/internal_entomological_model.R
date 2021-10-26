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
calc_hosts_ent_params = function(vec_p, h_p, maxpop) {
    # we assume that there is an equal number of humans and animals
    Ni = c(0, floor(maxpop/2), floor(maxpop/2))
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

    h_p$muvA = muvA
    h_p$alphai = alphai
    return(h_p)
}

# Calculates the probability that a mosquito survives the extrinsic
# incubation period (eq. 3c Chitnis et al. 2007). This probability is then used
# to calculate the sporozoite rate and the vectorial capacity.
calc_psts = function(ts, tau, PA, Pdf) {
    # TO DO: add this in the general checking function
    if(ts < tau) {
        ts = tau
        # TO DO: check why this happens for some data entries
        #stop("ts is less than tau. Check parameter values.");
    }
    n_cov = length(PA)
    # eq. 4 Chitnis et al. 2007
    kplus = floor(ts/tau) - 1
    kp_v = c(0:kplus)
    if (kplus > 0){
        s_kplus = rowSums(rep.row(choose(ts - (kp_v+1) * tau + kp_v, kp_v), n_cov) *
                              rep.col(PA, kplus+1) ^ rep.row(ts - (kp_v+1) * tau, n_cov) *
                              rep.col(Pdf, kplus+1) ^ rep.row(kp_v, n_cov))
    } else {
        s_kplus = rep(0, n_cov)
    }

    # There there is an issue when tau is less than 2
    if (tau < 2) {
        tau = 2
    }
    suml = rep(0, n_cov)
    sumlv = matrix(0, nrow = tau-1, ncol = n_cov);
    for(l in 1:(tau-1)) {
        # eq. 4 Chitnis et al. 2007
        klplus = floor((ts + l) / tau) - 2
        klp_v = c(0:klplus)
        if (klplus > 0) {
            s_klplus = rowSums(rep.row(choose(ts + l - (klp_v+2) * tau + klp_v, klp_v), n_cov) *
                                   rep.col(PA, klplus+1) ^ rep.row(ts + l - (klp_v + 2) * tau, n_cov) *
                                   rep.col(Pdf, klplus+1) ^ rep.row(klp_v+1, n_cov))
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

# TO DO: Check if this is needed and implement the function if necessary
calc_upsilon = function (tau, ts, PA, Pdf, Pdif, Pduf, sumkplus, sumlv){
    ###########################################################################
    # Create the skeleton of Upsilon - so we can find eigenvalues.
    ###########################################################################
    tau <- input_params[[intervention]][['tau']]
    ts <- input_params[[intervention]][['ts']]
    mt <- ts + tau - 1;
    orde <- 2*mt + tau;
    indexones <- c(2:mt, c((mt+2):(2*mt)), 2*mt+2,2*mt+tau);

    Upsilon <- matrix(0,orde,orde) #zeros(orde,orde);
    for(i in indexones){
        Upsilon[i,i-1] <- 1;
    }
    # We now calculate Upsilon.
    Upsilon[1,1] <- PA;
    Upsilon[1,tau] <- Pdf;

    Upsilon[mt+1,tau] <- Pdif;
    Upsilon[mt+1,mt+1] <- PA;
    Upsilon[mt+1,mt+tau] <- Pduf;

    Upsilon[2*mt+1,ts] <- Pdif *sumkplus;
    Upsilon[2*mt+1,mt+ts] <- -Pdif * sumkplus;
    for (ll in 1:c(tau-1)){
        Upsilon[2*mt+1,ts+ll] <- Pdif * sumlv[ll];
        Upsilon[2*mt+1,mt+ts+ll] <- -Pdif * sumlv[ll];
    }
    Upsilon[2*mt+1,2*mt+1] <- PA;
    Upsilon[2*mt+1,2*mt+tau] <- Pdf;

    tmp<-eigen(Upsilon); eigvals<-tmp$values; V<-tmp$vectors

    maxeigval <- max(abs(eigvals));
    return(maxeigval)
}

# Calculates entomological parameters for all coverages and host types for
# a given intervention.
f_eval_ent_quant = function(interv, vec_p, host_p, Nv0, Ni, upsilon = FALSE) {
    # Ni is for all the coverages and types of hosts
    n_cov = nrow(Ni)
    n_host_types = length(interv$alphai)
    t_avail = rep.row(interv$alphai, n_cov) * Ni
    # t_avail = matrix(interv$alphai, nrow = n_cov, ncol = n_host_types,
    #                  byrow = TRUE) * Ni
    # Probability that the mosquito:
    # is still host seeking after time td
    PA = exp(-(rowSums(t_avail) + host_p$muvA) * vec_p$td)
    # finds host i
    PAi = (1-PA) * t_avail/(rowSums(t_avail) + host_p$muvA)
    # dies while host seeking
    PAmu = 1 - (PA + rowSums(PAi))
    # finds a host on a given night and survives a complete feeding cycle
    Pdf = rowSums(PAi * rep.row(interv$PBi,n_cov) * rep.row(interv$PCi,n_cov) *
                      rep.row(interv$PDi,n_cov) * rep.row(interv$PEi,n_cov))
    # surviving a feeding cycle (eq. 2 Chitnis et al. 2007)
    Pf = Pdf / (1-PA)
    # finds a host on a given day, survives and does not get infected
    Pduf = rowSums(PAi * rep.row(interv$PBi,n_cov) * rep.row(interv$PCi,n_cov) *
                       rep.row(interv$PDi,n_cov) * rep.row(interv$PEi,n_cov) *
                       rep.row((1-interv$Kvi),n_cov))
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

    # we use the equilibrium expression from the difference equations to
    # calculate sporozoite rate and not the empirical derivation.
    res = calc_psts(vec_p$ts, vec_p$tau, PA, Pdf)
    psts = res$psts
    sumkplus = res$sumkplus
    sumlv = res$sumlv
    sr = (Pdif/(1 - PA - Pdf)) * (1 - Pdif / (1 - PA-Pduf)) * psts

    # host biting rate (eq. 15 Chitnis et al 2007)
    sigmai = PAi * rep.row(interv$PBi,n_cov) * rep.col(Nv,n_host_types) / (Ni * (Ni>0))

    # EIR (eq. 16 chitnis et al. 2007)
    eiri = sigmai * rep.col(sr, n_host_types)

    # vectorial capacity (eq. 21 Chitnis et al 2007)
    vc = Nv0/rowSums(Ni[,1:2]) * 1/((1-PA-Pdf)^2) * psts *
        rowSums(PAi[,1:2]*rep.row(interv$PBi[1:2],n_cov)) *
        rowSums(PAi[,1:2]*rep.row(interv$PBi[1:2],n_cov) *
                                                rep.row(interv$PCi[1:2],n_cov) *
                                                rep.row(interv$PDi[1:2],n_cov) *
                                                rep.row(interv$PEi[1:2],n_cov))

    # duration of the feeding cycle (eq. 23 Chitnis et al 2007)
    f = vec_p$tau + PA / (1 - PA)

    if (upsilon) {
        maxeig = calc_upsilon(vec_p$tau, vec_p$ts, PA, Pdf, Pdif, Pduf, sumkplus, sumlv)
    } else {
        maxeig = NULL
    }

    res = list(Pf_all = Pf, or_all=or, sr_all=sr, sigmai_all=sigmai,
               eiri_all=eiri, vc_all=vc, f_all=f, PAmu_all=PAmu, maxeig=maxeig)
    return(res)
}
