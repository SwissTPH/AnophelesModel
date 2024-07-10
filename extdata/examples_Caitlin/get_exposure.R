## The adjustment_for_location function calculates the proportion of averted exposure to adjust for.
## This depends on the human location importance for  intervention impact.
adjustment_for_location <- function(param, indoor_outdoor, intervention){
    f1 <- function(indoor_outdoor) { # LLINs
          m <- with(indoor_outdoor, switch(param,
                                           "alphai" = Exposure_Indoor_whileinbed,
                                           "PBi" = Exposure_Indoor_whileinbed,
                                           "PCi" = Exposure_Indoor_whileinbed,
                                           "PDi" = indoor_resting))
          return(m)
    }
    f2 <- function(indoor_outdoor) { # IRS, House screening
          m <- with(indoor_outdoor, switch(param,
                                           "alphai" = Exposure_Indoor_total,
                                          "PBi" = Exposure_Indoor_total,
                                          "PCi" = Exposure_Indoor_total,
                                          "PDi" = indoor_resting))
          return(m)
    }
    f3 <- function(indoor_outdoor) { # LLINsO
          m <- with(indoor_outdoor, switch(param,
                                           "alphai" <- Exposure_Outdoor_whileinbed,
                                           "PBi" <- Exposure_Outdoor_whileinbed,
                                           "PCi" <- Exposure_Outdoor_whileinbed,
                                           "PDi" <- 0))
          return(m)
    }
    f4 <- function(indoor_outdoor) { # Behavior change: stay indoors
          m = with(indoor_outdoor, switch(param,
                                          "alphai" = Exposure_Indoor_total/(Exposure_Indoor_whileinbed +
                                                                            Exposure_Outdoor_total -
                                                                            Exposure_Outdoor_whileinbed),
                                          "PBi" = 0,
                                          "PCi" = 0,
                                          "PDi" = 0))
          return(m)
    }
    multiplier <- switch(intervention,
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


## The get_in_out_exp function calculates human exposure to biting.
## Adjust it to account for a specified exposure multiplier value.

get_in_out_exp <- function(activity_cycles, vec_p, specified_multiplier = NULL) {

    # If specified_multiplier is not NULL, return a list with each element equal to the specified multiplier.
    if (!is.null(specified_multiplier)) {
        indoor_outdoor <- list(specified_multiplier, specified_multiplier,
                               specified_multiplier, specified_multiplier,
                               specified_multiplier)
        names(indoor_outdoor) <- c('Exposure_Indoor_total', 'Exposure_Outdoor_total',
                                   'Exposure_Indoor_whileinbed', 'Exposure_Outdoor_whileinbed',
                                   'indoor_resting')
        return(indoor_outdoor)
    }

    # Calculate exposure to biting by category in the absence of interventions
    # Endophagy is ideally the proportion of bites that are indoors from matched
    # indoor:outdoor landing collections
    activity_cycles$HBI <- activity_cycles$HBI / sum(activity_cycles$HBI, na.rm = TRUE) * vec_p$endophagy
    activity_cycles$HBO <- activity_cycles$HBO / sum(activity_cycles$HBO, na.rm = TRUE) * (1 - vec_p$endophagy)

    # Assume humans are indoors while sleeping unless a separate humans_indoors cycle is specified.
    activity_cycles$humans_indoors <- with(activity_cycles, ifelse(is.na(humans_indoors),
                                                                   humans_in_bed, humans_indoors))
    activity_cycles$Exposure_Indoor_total <- with(activity_cycles, HBI * humans_indoors)
    activity_cycles$Exposure_Outdoor_total <- with(activity_cycles, HBO * (1 - humans_indoors))

    # If humans_indoors < humans_in_bed we assign the difference to outdoor sleeping.
    activity_cycles$Exposure_Indoor_whileinbed <- with(activity_cycles, HBI * pmin(humans_in_bed,
                                                                                   humans_indoors, na.rm = TRUE))
    activity_cycles$Exposure_Outdoor_whileinbed <- with(activity_cycles, HBO * pmax(humans_in_bed - humans_indoors,
                                                                                    rep(0, length(humans_indoors)),
                                                                                    na.rm = TRUE))
    Overall_total = sum(activity_cycles$Exposure_Indoor_total, na.rm = TRUE) +
                    sum(activity_cycles$Exposure_Outdoor_total, na.rm = TRUE)

    # Calculate exposures.
    # Total indoor exposure as a proportion of overall exposure
    Exposure_Indoor_total <- sum(activity_cycles$Exposure_Indoor_total, na.rm = TRUE) / Overall_total
    # Total outdoor exposure as a proportion of overall exposure
    Exposure_Outdoor_total <- sum(activity_cycles$Exposure_Outdoor_total, na.rm = TRUE) / Overall_total
    # Total indoor in bed exposure as a proportion of overall exposure
    Exposure_Indoor_whileinbed <- sum(activity_cycles$Exposure_Indoor_whileinbed, na.rm = TRUE) / Overall_total
    # Total outdoor in bed exposure as a proportion of overall exposure
    Exposure_Outdoor_whileinbed <- sum(activity_cycles$Exposure_Outdoor_whileinbed, na.rm = TRUE) / Overall_total

    # Indoor-outdoor patterns relate to both mosquito and human behaviour.
    indoor_outdoor <- list(Exposure_Indoor_total, Exposure_Outdoor_total,
                           Exposure_Indoor_whileinbed,
                           Exposure_Outdoor_whileinbed, vec_p$endophily)
    names(indoor_outdoor) <- c('Exposure_Indoor_total', 'Exposure_Outdoor_total',
                               'Exposure_Indoor_whileinbed',
                               'Exposure_Outdoor_whileinbed', 'indoor_resting')
    return(indoor_outdoor)
}


## The get_exposure_multiplier function calculates the adjustment for averted exposure.
## Adjust it to return a specified exposure multiplier or calculate one if not provided.

get_exposure_multiplier <- function(param_name, model_obj, intervention_type, specified_multiplier = NULL) {
    # If specified_multiplier is not NULL, return it directly.
    if (!is.null(specified_multiplier)) {
        return(specified_multiplier)
    }
    # Check the inputs
    if (!(param_name %in% c("alphai", "PBi", "PCi", "PDi"))) {
        stop("The argument param_name should be one of: alphai, PBi, PCi, PDi.")
    }
    interventions = c("LLINs", "IRS", "LLINsO", "Screening", "Larviciding")
    if (!(intervention_type %in% interventions)) {
        stop(paste0("The argument intervention_type should be one of: ", paste(interventions, collapse = ", ")))
    }
    # Calculate indoor and outdoor exposure to biting
    biting_exp = get_in_out_exp(model_obj$activity, model_obj$vec_params)
    # Calculate the exposure multiplier value
    multiplier = adjustment_for_location(param_name, biting_exp, intervention_type)
    return(multiplier)
}

## Conduct sensitivity analysis using Anopheles gambiae.

# Define vector object.
custom_params <- as.data.frame(cbind.data.frame(species_name = "Anopheles gambiae", M = 0.6134499, Chi = 0.4749833,
                                               A0 = 0.5744970, zeta.3 = 1, td = 0.33, tau = 3, ts = 10, to = 5,
                                               endophily = 0.7743084844, endophagy = 0.6578578))
vec_p <- def_vector_params(mosquito_species = "Anopheles gambiae", vector_table = custom_params)

# Define host object.
hosts_p <- def_host_params()

# Define activity patterns.
activity <- NULL
activity$HBI<- c(0.000000000, 0.000000000 ,0.000491400, 0.000491400, 0.005896806, 0.007862408, 0.028009828, 0.048157248,
                 0.068304668, 0.066339066, 0.062899263, 0.062899263, 0.070270270, 0.077641278, 0.000000000, 0.000000000)
activity$HBO<- c(0.000000000, 0.000000000 ,0.000982801, 0.000982801, 0.003931204, 0.025061425, 0.042260442, 0.056019656,
                 0.073710074, 0.077149877, 0.077149877, 0.073710074, 0.038329238, 0.031449631, 0.000000000, 0.000000000)
activity$humans_indoors <- c(0.00000000, 0.28380952, 0.61904762, 0.84761905, 0.96190476, 1.00000000, 1.00000000, 1.00000000,
                            1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.96190476, 0.00952381, 0.00000000, 0.00000000)
activity$humans_in_bed <- c(0.00000000, 0.00000000, 0.00000000, 0.2271333, 0.6666333, 1.00000000, 1.00000000, 1.00000000,
                            1.00000000, 1.00000000, 1.00000000, 1.00000000, 0.8833167, 0.2006667, 0.00000000, 0.00000000)
activity_obj <- def_activity_patterns(activity)

# Initialise entomological model.
model_obj <- build_model_obj(vec_p = vec_p, hosts_p = hosts_p, activity = activity, total_pop = 2000)

# Add forced exposure input to model_obj.

# Run sensitivity analysis for get_in_out_exp.
no_multiplier <- get_in_out_exp(activity_cycles = activity, vec_p = vec_p, specified_multiplier = NULL)
print(no_multiplier)
with_multiplier <- get_in_out_exp(activity_cycles = activity, vec_p = vec_p, specified_multiplier = 1.5)
print(with_multiplier)

# Run sensitivity analysis for get_exposure_multiplier.
no_multiplier <- get_exposure_multiplier(param_nam = "alphai", model_obj = model_obj,
                                         intervention_type = "LLINs", specified_multiplier = NULL)
print(no_multiplier)
with_multiplier <- get_exposure_multiplier(param_nam = "alphai", model_obj = model_obj,
                                           intervention_type = "LLINs", specified_multiplier = 1.5)
print(with_multiplier)



