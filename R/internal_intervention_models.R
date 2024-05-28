############################################
# Functions defining intervention objects
############################################

# Generic values added by Tom Smith Feb 2017 (from Chitnis et al, 2010, Table 9)
# The library of interventions was extended by Olivier Briet in 2018,
# allowing arbitrary decay functions manipulated via an interpolation algorithm.
# The IRS code changed to read a parameter table added by Tom Smith (Aug 2019)
# This code generates the interpolation points for the decays for each parameter
# but differs from the original implementation (for the Albimanus paper) in not
# making use of the vector_specific_parameter vector or Pii
calc_IRS_p = function(int_obj, vec_params, int_host, activity_cycles, nips, specified_multiplier = NULL) {
    print("IRS")
    IRS_models_params = interventions_param$IRS_params
    int_summary = interventions_param$interventions_summary
    parameter_set = IRS_models_params[IRS_models_params$Parameterisation ==
                                                    int_obj$parameterisation,]
    intervention = as.character(int_summary$Intervention[
                    int_summary$Parameterisation == int_obj$parameterisation])
    duration = int_summary$Duration[int_summary$Parameterisation ==
                                                    int_obj$parameterisation]

    indoor_outdoor = get_in_out_exp(activity_cycles, vec_params, specified_multiplier = specified_multiplier)

    ns = c("Parameterisation", "Decay_type", "Initial_effect", "L", "k")
    long_series = melt(parameter_set[, setdiff(colnames(parameter_set), ns)],
                    id.vars = c('Measured_quantity'))
    long_series = long_series[!is.na(long_series$value), ]
    long_series$time = as.numeric(gsub("\\T_", "", long_series$variable))
    IRS_measured_quantities = long_series[c('Measured_quantity','time','value')]

    times = (seq(1:nips) - 0.5)*duration/nips
    parameters = length(parameter_set[,1])
    points = NULL #vector("list", parameters)
    for(par in 1:nrow(parameter_set)){
        reqParam = parameter_set$Measured_quantity[par]
        if(parameter_set$Decay_type[par]=='approxfun'){
            input_times = with(IRS_measured_quantities,
                               time[Measured_quantity == reqParam])
            input_points = with(IRS_measured_quantities,
                                value[Measured_quantity==reqParam])
            required = data.frame(input_times, input_points)
            prediction_points = data.frame(input_times = times)
            # TO DO: fix the warning that we get here:
            lm2 = lm(input_points ~ bs(input_times), data = required)
            points[[reqParam]] = predict(lm2, prediction_points)
        } else {
            decay_p = list(L=parameter_set$L[par], k=parameter_set$k[par])
            decay_f = paste0(parameter_set$Decay_type[par],'_function')
            points[[reqParam]] = parameter_set$Initial_effect[par] *
                do.call(decay_f, list(time = times, p = decay_p))
        }
        points[[reqParam]] = points[[reqParam]] *
                                adjustment_for_location(param=reqParam,
                                                indoor_outdoor, intervention)
    }
    # param_names = parameter_set$Param[1:parameters]
    # points = setNames(points, parameter_set$Measured_quantity)

    for (name in parameter_set$Measured_quantity) {
        if (name == "alphai") {
            int_obj$effects[[name]][, 1] = int_obj$effects[[name]][, 2] *
                                                            (1 - points[[name]])
        } else {
            int_obj$effects[[name]][, 1] = int_obj$effects[[name]][, 1] *
                                                            (1 - points[[name]])
        }
        int_obj$effects[[paste0(name, "_decay")]] = points[[name]]
    }
    return(int_obj)
}

######################################################################
# Functions required for assembling data on LLIN effects from different datasets
# for creating input to the AnophelesModel R package
# Assembled by Tom Smith, June 2019 from data and code provided by Olivier Briet
# Includes vectorization of Olivier's code for interpolation points
#####################################################################

# Calculate attrition and holed area based on PMI data
calc_net_decay = function(net_type, country, insecticide_type, n_ips, duration){

    # Check that there are available entries in the database for the net type
    # and country if the net type is not the default one
    if (net_type != "Default") {
        PMI_entries = with(interventions_param$LLINs_params$durability_estim,
                           Semester[which(Country == country &
                                              Net_Type == net_type)])
        if(length(PMI_entries) == 0) {
            err_msg = paste("Attrition and holed area data are not available",
                            "for the specified net type and country.",
                            "To retrieve the list of available",
                            "data use get_net_insecticides().")
            stop(err_msg)
        }
    }

    # Check that there are available entries in the database for the insecticide
    LLINs_insecticide = interventions_param$LLINs_params$insecticide_c
    insecticide_p = as.list(LLINs_insecticide[, insecticide_type])

    if(length(insecticide_p) == 0) {
        err_msg = paste("Decay values are not available",
                        "for the specified insecticide.",
                        "To retrieve the list of available",
                        "insecticides use get_net_types().")
        stop(err_msg)
    }

    if(net_type == "Default"){
        # Calculate default attrition and holed area
        # Data from Briet et al (2019)
        # attrition and insecticide decay are from Briet & Penny (2013)
        # holed area is from  Morgan J et al Am J Trop Med Hyg. 2015.
        s_decay_p = list(initialEffect = 1, L = 20.7725,
                         k = 18, duration = duration)
        survival = interp_decay("smoothcompact_function", s_decay_p, n_ips)
        #<holeRate mean="1.80" sigma="0.80"/>
        #<ripRate mean="1.80" sigma="0.80"/><ripFactor value="0.3"/>
        #This approximates the central scenario hole index
        h_decay_p = list(initialEffect = 1, a = 0,
                         b = 58.5949, duration = duration)
        holed.surface.area = interp_decay("parabolic_growth_function",
                                          h_decay_p, n_ips)
    } else {
        # Country-specific attrition and holed area by LLIN type from PMI study
        # Attrition
        survival_data = with(interventions_param$LLINs_params$durability_estim,
                             Value[which(Country == country &
                                             Net_Type == net_type &
                                             Parameter == 'Survival')])
        semester = with(interventions_param$LLINs_params$durability_estim,
                        Semester[which(Country == country &
                                           Net_Type == net_type &
                                           Parameter == 'Survival')])
        # assign midpoint of each semester as net age in years
        net_age = (semester-0.5)*0.5
        tr_survival = log((100 - (survival_data-0.1))/100)
        ip_tr_survival = spline_interpolation(tr_survival, semester,
                                              n_ips, duration)
        survival = ifelse(ip_tr_survival > 0, 1, 1-exp(ip_tr_survival))
        # Holed area
        semester = with(interventions_param$LLINs_params$durability_estim,
                        Semester[which(Country == country &
                                           Net_Type == net_type &
                                           Parameter == 'Mean of log transformed holed area')])
        holed_area_data = with(
            interventions_param$LLINs_params$durability_estim,
            Value[which(Country == country &
                            Net_Type == net_type &
                            Parameter == 'Mean of log transformed holed area')])
        holed.surface.area = exp(spline_interpolation(holed_area_data, semester,
                                                      n_ips, duration))
    }

    # Cap the holed surface area at the total surface area of the net
    total_LLIN_surface = 192000
    holed.surface.area.cap = ifelse(holed.surface.area > total_LLIN_surface,
                                    total_LLIN_surface, holed.surface.area)
    logHoles = log(holed.surface.area.cap + 1)

    # Select the relevant insecticide decay parameters
    LLINs_insecticide = interventions_param$LLINs_params$insecticide_c
    insecticide_p = as.list(LLINs_insecticide[, insecticide_type])

    # Obtain the vector of interpolation points for insecticide decay
    insecticide.content = interp_decay("exponential_function",
                                       list(initialEffect = insecticide_p$initialInsecticide,
                                            L = 1.5, duration = duration), n_ips)
    ips = seq(1:n_ips)
    t = duration*(ips - 1)/n_ips
    net_decay = data.frame(time = t, logHoles = logHoles,
                           insecticideContent = insecticide.content,
                           survival = survival)
    return(as.data.frame(net_decay))
}

#LLIN parameters assembled by combining point estimates from different studies
# Core vector operations
calc_prob_vectors = function(PEnt, PEnt.u, PAtt, PAtt.u, PBmu, PBi.u, PBmu.u,
                             PCmu, PCi.u, PCmu.u, alphai, indoor_outdoor){
    PEnt.pred = PEnt/PEnt.u
    PAtt.pred = PAtt/PAtt.u
    alphai.reduction = 1 - PEnt.pred*PAtt.pred
    PBmu.pred = 1 - (1 - PBmu)/(1 - PBmu.u)
    PCmu.pred = 1 - (1 - PCmu)/(1 - PCmu.u)
    alphai_decay = adjustment_for_location("alphai",
                        indoor_outdoor, "LLINs")*alphai.reduction
    alphai_int = alphai[2]*(1 - alphai_decay)
    PBi_decay = adjustment_for_location("PBi",
                        indoor_outdoor, "LLINs")*PBmu.pred
    PBi_int = PBi.u*(1 - PBi_decay)
    PCi_decay = adjustment_for_location("PCi",
                        indoor_outdoor, "LLINs")*PCmu.pred
    PCi_int = PCi.u*(1 - PCi_decay)
    ip_vecs = data.frame(alphai_int, PBi_int, PCi_int,
                         alphai_decay, PBi_decay, PCi_decay)
    return(ip_vecs)
}

calc_LLINs_p = function(int_obj, vec_params, activity_cycles, nips, specified_multiplier = NULL) {
    # Select the relevant coefficients of the LLIN regression model
    LLINs_model_coeff = interventions_param$LLINs_params$model_coeff
    net_p = as.list(LLINs_model_coeff[int_obj$parameterisation,])

    # Select the relevant insecticide decay parameters
    LLINs_insecticide = interventions_param$LLINs_params$insecticide_c
    insecticide_p = as.list(LLINs_insecticide[, int_obj$LLIN_insecticide])

    # Select the duration of the intervention
    d_idx = which(interventions_param$interventions_summary$Parameterisation ==
                           int_obj$parameterisation)
    duration = as.double(interventions_param$interventions_summary[d_idx,
                                                                   "Duration"])

    # First obtain the limiting probabilities for the no-net case
    # Entry, attack, pre and post-prandial effects without a net
    logHolesMax = log(net_p$HolesMax+1)
    PEnt.u = ilogit(net_p$beta0.PEnt)
    PAtt.u = ilogit(net_p$beta0.PAtt + net_p$beta1.PAtt*logHolesMax)
    PBmu.u = ilogit(net_p$beta0.PBmu + net_p$beta1.PBmu*logHolesMax)
    PCmu.u = ilogit(net_p$beta0.PCmu + net_p$beta1.PCmu*logHolesMax)

    # Obtain the vectors of interpolation points for attrition and holed area
    net_demog = calc_net_decay(net_type = int_obj$LLIN_type,
                              country = int_obj$LLIN_country,
                              insecticide_type = int_obj$LLIN_insecticide,
                              n_ips = nips, duration = duration)
    logHoles = net_demog$logHoles
    ips = seq(1:nips)
    net_age = duration * ips/nips

    # and the vector of interpolation points for insecticide decay
    insecticide.content = net_demog$insecticideContent

    # Calculate the exposure multiplier
    in_out_exp = get_in_out_exp(activity_cycles, vec_params, specified_multiplier = specified_multiplier)

    # The regression model used is
    # logit(PXxx) = beta0 + beta1 * log(Holes + 1) +
    #               scaling-factor * beta2 * log(Chem + 1) +
    #               beta3 * scaling-factor * log(Holes + 1) * log(Chem + 1),
    # where the PXxx represents the proportion of mosquitoes entering (PEnt),
    # attacking (PAtt), being killed before feeding (PBmu),
    # or being killed after feeding (PCmu).
    # Holes is the holed surface area in cm2,
    # Chem is the insecticide concentration in mg/m2.
    # HolesMax is the assumed total surface of an LLIN, and is used to
    # calculate the effects for an unprotected human.
    log_ic = log(insecticide.content + 1)
    scalingfactor = insecticide_p$scalingfactor
    PEnt = ilogit(net_p$beta0.PEnt + scalingfactor*net_p$beta1.PEnt*log_ic)
    PAtt = ilogit(net_p$beta0.PAtt + net_p$beta1.PAtt*logHoles +
                      scalingfactor*net_p$beta2.PAtt*log_ic +
                      net_p$beta3.PAtt*scalingfactor*logHoles*log_ic)
    PBmu = ilogit(net_p$beta0.PBmu + net_p$beta1.PBmu*logHoles +
                      scalingfactor*net_p$beta2.PBmu*log_ic +
                      net_p$beta3.PBmu*scalingfactor*logHoles*log_ic)
    logitPCmu = net_p$beta0.PCmu + net_p$beta1.PCmu*logHoles +
                scalingfactor*net_p$beta2.PCmu*log_ic +
                net_p$beta3.PCmu*scalingfactor*logHoles*log_ic
    PCmu = ilogit(ifelse(logitPCmu > 600, 600, logitPCmu))
    PBi = int_obj$effects$PBi[1,2]
    PCi = int_obj$effects$PCi[1,2]
    alphai = int_obj$effects$alphai[1,]
    ip_vecs = calc_prob_vectors(PEnt = PEnt, PEnt.u = PEnt.u, PAtt = PAtt,
                                PAtt.u = PAtt.u, PBmu = PBmu, PBi.u = PBi,
                                PBmu.u = PBmu.u, PCmu = PCmu, PCi.u = PCi,
                                PCmu.u = PCmu.u, alphai = alphai, in_out_exp)

    # Extract the parameters needed by the entomological model
    int_obj$effects$alphai[, 1] = ip_vecs$alphai_int
    int_obj$effects$PBi[, 1] = ip_vecs$PBi_int
    int_obj$effects$PCi[, 1] = ip_vecs$PCi_int
    int_obj$effects$alphai_decay = ip_vecs$alphai_decay
    int_obj$effects$PBi_decay = ip_vecs$PBi_decay
    int_obj$effects$PCi_decay = ip_vecs$PCi_decay
    int_obj$effects$survival = net_demog$survival
    return(int_obj)
}

######################################################################
# Function required for calculating house screenning impact
#####################################################################
calc_House_screening_p = function(int_obj, vec_params, activity_cycles, nips, specified_multiplier = NULL) {
    indoor_outdoor = get_in_out_exp(activity_cycles, vec_params, specified_multiplier = specified_multiplier)
    int_obj$effects$alphai[,1] = int_obj$effects$alphai[,2] *
        (1 - adjustment_for_location("alphai", indoor_outdoor,
                                     "House_screening") * 0.59)
    return(int_obj)
}

######################################################################
# Function required for calculating behavior change impact
# TO DO: check if this function is correct
#####################################################################
calc_Stay_indoors_p = function(int_obj, vec_params, activity_cycles, nips, specified_multiplier = NULL) {
    indoor_outdoor = get_in_out_exp(activity_cycles, vec_params, specified_multiplier = specified_multiplier)
    int_obj$effects$alphai[,1] = int_obj$effects$alphai[,2] *
        (1 - adjustment_for_location("alphai", indoor_outdoor,
                                     "Stay_indoors"))
    return(int_obj)
}
