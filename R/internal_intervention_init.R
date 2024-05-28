#################################
# internal_intervention_init
#
# Contains initialization functions for building intervention objects and
# calculating intervention-specific exposure parameters
#
################################

# Check if the provided intervention effects are well defined, i.e.,
# all the model parameters capturing the effects of the
# interventions are provided and they have the right dimensions.
# INPUT:
# int_obj = input intervention object
# num_ip_points = number of interpolation points
# OUTPUT:
# boolean indicating whether the object is well defined or not
check_custom_interv_effects = function(int_obj, num_ip_points) {
    ok = TRUE

    # Check that all attributes are provided
    if(is.null(int_obj$effects$Kvi) || is.null(int_obj$effects$alphai) ||
       is.null(int_obj$effects$PBi) || is.null(int_obj$effects$PCi) ||
       is.null(int_obj$effects$PDi) || is.null(int_obj$effects$PEi) ||
       is.null(int_obj$effects$survival)) {
        print("ERROR: Empty intervention effects.")
            ok = FALSE
    }

    # Check that all attributes have the correct dimensions
    if((length(int_obj$effects$Kvi) !=  3) ||
       (dim(int_obj$effects$alphai) != c(num_ip_points, 3)) ||
       (dim(int_obj$effects$PBi) != c(num_ip_points, 3)) ||
       (dim(int_obj$effects$PCi) != c(num_ip_points, 3)) ||
       (dim(int_obj$effects$PDi) != c(num_ip_points, 3)) ||
       (dim(int_obj$effects$PEi) != c(num_ip_points, 3)) ||
       (length(int_obj$effects$survival) != num_ip_points)) {
        print("ERROR: Wrong dimensions for the intervention effects.")
        ok = FALSE
    }

    # Check that all attributes are probabilities
    if(!(all(between(int_obj$effects$Kvi, 0, 1)) == TRUE) ||
       !(all(between(int_obj$effects$alphai, 0, 1)) == TRUE) ||
       !(all(between(int_obj$effects$PBi, 0, 1)) == TRUE) ||
       !(all(between(int_obj$effects$PCi, 0, 1)) == TRUE) ||
       !(all(between(int_obj$effects$PDi, 0, 1)) == TRUE) ||
       !(all(between(int_obj$effects$PEi, 0, 1)) == TRUE) ||
       !(all(between(int_obj$effects$survival, 0, 1)) == TRUE)) {
        print("ERROR: Values for intervention effects are not between 0 and 1.")
        ok = FALSE
    }

    return(ok)
}

# Initialize an intervention object with values for availabilities of
# protected and unprotected human hosts, and animal hosts, as well as
# probabilities of the mosquito oviposition cycle for the three types of hosts.
# INPUT: h_params = list with entomological parameters for humans and animal
#                   hosts
#        ni = number of interpolation points
# OUTPUT: an object with all the initialized values in the field "effects"
init_intervention_effects = function(h_params, ni) {
    # interv_obj = h_params
    interv_obj = NULL
    # Add human hosts protected by the intervention and construct
    # the matrix of interpolation points
    interv_obj$effects$host_types = c("protected humans", "unprotected humans",
                                      "animals")
    interv_obj$effects$Kvi = c(h_params$Kvi[1], h_params$Kvi)
    interv_obj$effects$alphai = rep.row(c(h_params$alphai[1],
                                          h_params$alphai), ni)
    interv_obj$effects$PBi = rep.row(c(h_params$PBi[1], h_params$PBi), ni)
    interv_obj$effects$PCi = rep.row(c(h_params$PCi[1], h_params$PCi), ni)
    interv_obj$effects$PDi = rep.row(c(h_params$PDi[1], h_params$PDi), ni)
    interv_obj$effects$PEi = rep.row(c(h_params$PEi[1], h_params$PEi), ni)
    interv_obj$effects$survival = rep(1, ni)
    return(interv_obj)
}

# Initialize the effects of an intervention object with parameters
# extracted from the package database
# INPUT: interv_obj = intervention object
#        hosts_params = list of host-specific entomological parameters
#        vec_params = list of vector entomological parameters
#        activity = activity patterns of mosquitoes and humans
#        ip = number of interpolation points
# OUTPUT: updated intervention object containing an attribute for the
# intervention effects with the following:
#   PBi: probability that a mosquito bites host i
#   PCi: probability that a mosquito finds a resting place after
#           biting a host of type i
#   PDi: probability that a mosquito survives the resting phase
#   after biting a host of type i
#   PEi: probability that a mosquito lays eggs and
#           returns to host-seeking after biting a host of type i
#   Kvi: proportion of susceptible mosquitoes that become infected
#           after biting any host of type i
#   alphai: availability to mosquitoes for host of type i
calc_interv_effects_db <- function(interv_obj, model_p, ip, specified_multiplier = NULL) {

    # Initialize intervention object effects values
    interv_obj <- c(interv_obj, init_intervention_effects(model_p$host_params, ip))
    # Calculate intervention parameters at each time/interpolation point
    db_interventions <- list_intervention_models()
    if (interv_obj$id != "No intervention") {
        if (interv_obj$id %in% db_interventions$Intervention) {
            f_name = paste("calc_", interv_obj$id, "_p", sep = "")
            # update the parameters by calling the intervention function
            print(paste("calc_interv_effects_db", specified_multiplier))
            interv_obj = do.call(f_name,
                                      list(int_obj = interv_obj,
                                           vec_params = model_p$vec_params,
                                           activity_cycles = model_p$activity,
                                           nips = ip,
                                           specified_multiplier = specified_multiplier))
        } else {
            err_msg = paste0("No intervention model available for",
                             interv_obj$id, ". To check available interventions
                             models parameterisations, use list_int_models().")
            stop(err_msg)
        }
    }

    return(interv_obj)
}

# Function to calculate the proportion of averted exposure to adjust for
# depending on the human location importance for  intervention impact
adjustment_for_location = function(param, indoor_outdoor, intervention){
    f1 = function(indoor_outdoor) { # LLINs
        m = with(indoor_outdoor, switch(param,
                                        "alphai" = Exposure_Indoor_whileinbed,
                                        "PBi" = Exposure_Indoor_whileinbed,
                                        "PCi" = Exposure_Indoor_whileinbed,
                                        "PDi" = indoor_resting))
        return(m)
    }
    f2 = function(indoor_outdoor) { # IRS, House screening
        m = with(indoor_outdoor, switch(param,
                                        "alphai" = Exposure_Indoor_total,
                                        "PBi" = Exposure_Indoor_total,
                                        "PCi" = Exposure_Indoor_total,
                                        "PDi" = indoor_resting))
        return(m)
    }
    f3 = function(indoor_outdoor) { # LLINsO
        m = with(indoor_outdoor, switch(param,
                                         "alphai" = Exposure_Outdoor_whileinbed,
                                         "PBi" = Exposure_Outdoor_whileinbed,
                                         "PCi" = Exposure_Outdoor_whileinbed,
                                         "PDi" = 0))
        return(m)
    }
    f4 = function(indoor_outdoor) { # Behavior change: stay indoors
        m = with(indoor_outdoor, switch(param,
                "alphai" = Exposure_Indoor_total/(Exposure_Indoor_whileinbed +
                        Exposure_Outdoor_total - Exposure_Outdoor_whileinbed),
                "PBi" = 0,
                "PCi" = 0,
                "PDi" = 0))
        return(m)
    }
    multiplier = switch(intervention,
                        'LLINs'= f1(indoor_outdoor),
                        'IRS'= f2(indoor_outdoor),
                        # ITN outdoors (e.g. on the roof)
                        'LLINsO'= f3(indoor_outdoor),
                        # House screening affects all indoor biting (like IRS)
                        'House_screening' = f2(indoor_outdoor),
                        'Stay_indoors'= f4(indoor_outdoor),
                        'Larviciding' = 1.0)
    return(multiplier)
}

