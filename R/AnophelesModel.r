# This file contains the package functions for calculating the interventions
# impact on the vectorial capacity. In addition, the main function of the
# package, AnophelesModel(), incorporating all the steps of the package
# workflow can be found at the end of this file.

#########calculate_impact#####
#' @title calculate intervention impact in terms of vectorial capacity reduction
#'
#' @description \code{calc_impact} This function calculates the
#' proportion by which vectorial capacity is reduced for each of a list of interventions
#' when deployed to a cohort of humans
#' @param interventions_vec vector of intervention names
#' @param coverage_vec vector of coverages to consider for each intervention
#' @param human_behavior vector of human behavior before biting starts to
#' consider for each intervention. Can be "inside" or "outside".
#' @param model_p list of vector- and host-specific entomological parameters
#' (see description of output from function \code{calc_model_params})
#' @param Nv0 number of mosquitoes born and entering the host-seeking state
#' per day
#' @param num_ip_points number of time points to be considered
#'
#' @return list object with the following attributes:
#' \itemize{
#' \item \code{vec_p}: list of vector-specific entomological parameters (see
#' description of output from function \code{get_vector_params})
#' \item \code{host_p}: list of host-specific entomological parameters (see
#' description of output from function \code{get_host_params})
#' \item \code{interventions_vec}: vector of intervention-specific entomological
#' parameters and impact (vectorial capacity) values
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @references TO DO
#'
#' @importFrom minpack.lm "nlsLM"
#'
#' @examples
#'
#' @export
#'
calculate_impact = function(interventions_vec, coverage_vec,
                            model_p, Nv0, num_ip_points) {
    # Calculate impact for each intervention
    for (i in 1:length(interventions_vec)){
        # assume same number of humans and animals
        nhuman = floor(model_p$total_pop/2)
        interventions_vec[[i]]$effects$vc = NULL
        interventions_vec[[i]]$effects$impact = NULL
        interventions_vec[[i]]$coverages = coverage_vec
        # calculate parameters and impact at each time point
        for (ip in c(1:num_ip_points)) {
            # calculate proportion of protected and unprotected people
            Ni1 = coverage_vec * nhuman *
                                    interventions_vec[[i]]$effects$survival[ip]
            Ni2 = nhuman - Ni1
            Ni3 = model_p$total_pop - Ni1 - Ni2
            Ni = cbind(Ni1, Ni2, Ni3)
            # calculate intervention impact on vectorial capacity at the
            # selected time (interpolation) point
            interv = interventions_vec[[i]]$effects
            interv$alphai = interv$alphai[ip,]
            interv$PBi = interv$PBi[ip,]
            interv$PCi = interv$PCi[ip,]
            interv$PDi = interv$PDi[ip,]
            interv$PEi = interv$PEi[ip,]
            ent_params = f_eval_ent_quant(interv, model_p$vec_params,
                                          model_p$host_params, Nv0, Ni, FALSE)
            # concatenate the rows for vc and impact
            # calculate average vc and impact at the end
            interventions_vec[[i]]$effects$vc =
                rbind(interventions_vec[[i]]$effects$vc, ent_params$vc_all)
            interventions_vec[[i]]$effects$impact =
                rbind(interventions_vec[[i]]$effects$impact,
                                1 - ent_params$vc_all/ent_params$vc_all[1])
        }
        interventions_vec[[i]]$effects$avg_vc =
                                    colMeans(interventions_vec[[i]]$effects$vc)
        interventions_vec[[i]]$effects$avg_impact =
                                colMeans(interventions_vec[[i]]$effects$impact)
    }

    return(list(vec_p = model_p$vec_p, host_p = model_p$host_p,
                interventions_vec = interventions_vec))
}

#########calculate_impact_ip#########
#' @title calculates reduction in vectorial capacity for one interpolation point
#'
#' @description \code{calculate_impact_ip} This function calculates the
#' proportion by which vectorial capacity is reduced when an intervention is
#' deployed to a cohort of humans at a given time point
#' @param intervention_obj object with the following attributes
#' \itemize{
#' \item \code{PBi}: probability that a mosquito bites host i
#' \item \code{PCi}: probability that a mosquito finds a resting place after
#' biting a host of type i
#' \item \code{PDi}: probability that a mosquito survives the resting phase
#' after biting a host of type i
#' \item \code{PEi}: probability that a mosquito lays eggs and
#' returns to host-seeking after biting a host of type i
#' \item \code{Kvi}: proportion of susceptible mosquitoes that become infected
#' after biting any host of type i
#' \item \code{muvA}: mosquito death rate
#' \item \code{alphai}: availability to mosquitoes for host of type i
#' }
#' @param model_obj list of vector- and host-specific entomological parameters
#' (see description of output from function \code{calc_model_params})
#' @param N_vec number of daily emerging mosquitoes
#' @param Ni data frame with the number of protected and unprotected hosts for
#' various intervention coverages
#'
#' @return object with the following attributes (columns = different coverages):
#' #' \itemize{
#' \item \code{Pf_all}: probability that mosquitoes survive the feeding cycle
#' #' \item \code{or_all}: delayed oocyst rate
#' #' \item \code{sr_all}: sporozoite rate
#' #' \item \code{sigmai_all}: host biting rate
#' #' \item \code{eiri_all}: entomological innoculation rate
#' #' \item \code{vc_all}: vectorial capacity
#' #' \item \code{f_all}: duration of the feeding cycle
#' #' \item \code{PAmu_all}: probability that mosquitoes die during host seeking
#' #' \item \code{maxeig_all}: maximum eigen value of the upsilon matrix
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#'
#' @examples
#'
#' @export
#'
calculate_impact_ip = function(intervention_obj, model_obj, n_vec, Ni) {
    impact_ip = f_eval_ent_quant(intervention_obj, model_obj$vec_params,
                     model_obj$host_params, n_vec, Ni, FALSE)
    return(impact_ip)
}


#########calculate_impact_var#####
#' @title calculate intervention impact on the vectorial capacity
#' and assess its variation accounting for the variation of input parameters
#' @description \code{calc_impact_var} calculates the
#' proportion by which vectorial capacity is reduced for each of a list of
#' interventions when deployed to a cohort of humans
#' @param mosquito_species string corresponding to the name of the mosquito
#' species to load the bionomic parameters for. This argument is case
#' sensitive and must be one of the species provided with the package:
#' \emph{Anopheles gambiae}, \emph{Anopheles albimanus}, etc.
#' To see the available mosquito species in th package,
#' you can use the function \code{list_all_species()}.
#' Default value is Anopheles gambiae.
#' @param vec_ent_table data frame with custom vector entomological values.
#' Must be provided if custom parameter values should be used instead of
#' the ones in the package database corresponding to the
#' provided moquito species.
#' The data frame should have the same structure (column names) as the database
#' object \code{vec_ent_param} (see Data documentation). Default value is NULL.
#' @param host_table data frame with custom host-specific values.
#' Must be provided if custom parameter values should be used instead of
#' the ones in the package database.
#' The provided data frame should have the same structure as the database
#' object \code{host_ent_param} (see Data documentation). Default value is NULL.
#' @param activity can be either a string corresponding to default entries:
#' "default_Anopheles_gambiae", "default_Anopheles_albimanus"
#' or a list object with the following attributes:
#' \itemize{
#' \item \code{HBI}: proportion of human biting indoors
#' \item \code{HBO}: propotion human biting outdoors
#' \item \code{humans_indoors}: proportion of humans indoor
#' \item \code{humans_in_bed}:proportion of humans in bed
#' }
#' These attribues can be either time series with the corresponding values at
#' each time point, or characters indicating an entry ID in the package
#' database. The function list_rhythms() can be used to retrieve the
#' available entries for all geographical locations.
#' @param interventions vector of intervention names
#' @param coverage_vec vector of coverages to consider for each intervention
#' @param total_pop total population size of human and animal hosts
#' @param n_vec number of mosquitoes born and entering the host-seeking state
#' per day
#' @param n_time_points number of time points to be considered
#' @param n_sample_points number of samples to select for assessing variance
#' @param plot_result if TRUE, the function plots the reduction of the vectorial
#' capacity for each intervention in the \code{interventions} list
#'
#' @return list object with the following attributes:
#' \itemize{
#' \item \code{vec_p}: list of vector-specific entomological parameters (see
#' description of output from function \code{get_vector_params})
#' \item \code{host_p}: list of host-specific entomological parameters (see
#' description of output from function \code{get_host_params})
#' \item \code{interventions_vec}: vector of intervention-specific entomological
#' parameters and impact (vectorial capacity) values
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#'
#' @importFrom tgp "lhs"
#' @importFrom dplyr "between"
#'
#' @examples
#'
#' @export
#'
calculate_impact_var = function(mosquito_species = "Anopheles gambiae",
                                vec_ent_table = NULL,
                                host_table = NULL,
                                activity_patterns = "default_Anopheles_gambiae",
                                interventions = intervention_obj_examples,
                                coverage_vec = c(seq(0, 0.9, by = 0.1), 0.95, 0.99),
                                total_pop = 2000, n_vec = 10000,
                                n_time_points = 100, n_sample_points = 100,
                                plot_result = TRUE) {

    # Define the vector bionomics parameters
    mosq_param_vals = as.data.frame(def_vector_params(
                                            mosquito_species = mosquito_species,
                                            vector_table = vec_ent_table,
                                            verbose = FALSE))
    # Define the host specific parameters
    host_obj = def_host_params(verbose = FALSE)
    # Define the activity patterns
    activity_obj = def_activity_patterns(activity_patterns)

    # For the parameters where sd is provided, sample from [-2sd, 2sd]
    M = c(mosq_param_vals$M - 2*mosq_param_vals$M.sd,
          mosq_param_vals$M + 2*mosq_param_vals$M.sd)
    A0 = c(mosq_param_vals$A0 - 2*mosq_param_vals$A0.sd,
          mosq_param_vals$A0 + 2*mosq_param_vals$A0.sd)
    endophily = c(mosq_param_vals$endophily - 2*mosq_param_vals$endophily.sd,
           mosq_param_vals$endophily + 2*mosq_param_vals$endophily.sd)
    endophagy = c(mosq_param_vals$endophagy - 2*mosq_param_vals$endophagy.sd,
           mosq_param_vals$endophagy + 2*mosq_param_vals$endophagy.sd)
    mosq_param_ranges = rbind(M, A0, endophily, endophagy)
    sampled_points = as.data.frame(lhs(n_sample_points, mosq_param_ranges))

    # Build a table with the sampled parameter values and additional attributes
    colnames(sampled_points) = rownames(mosq_param_ranges)
    constant_col = setdiff(colnames(mosq_param_vals), colnames(sampled_points))
    sampled_points[, constant_col] = mosq_param_vals[, constant_col]

    # Calculate impact for each set of sapled parameter values
    impact_tab = NULL
    for (i in 1:nrow(sampled_points)) {
        # Define the vector object
        vec_obj = def_vector_params(mosquito_species = mosquito_species,
                                    vector_table = sampled_points[i,],
                                    verbose = FALSE)
        # Define the model object
        model_obj = build_model_obj(vec_obj, host_obj, activity_obj, total_pop)
        # Define interventions array
        interventions_vec = def_interventions_effects(interventions, model_obj,
                                                      n_time_points, FALSE, NULL)
        # Calculate impact
        impact_obj = calculate_impact(interventions_vec, coverage_vec,
                                      model_obj, n_vec, n_time_points)
        # Construct the impacts element
        for(interv_imp in impact_obj$interventions_vec) {
            intervention_name = interv_imp$description
            intervention_coverage = interv_imp$coverages
            intervention_impact = round(interv_imp$effects$avg_impact, digits = 3)
            run_no = i
            impact_entry = cbind.data.frame(intervention_name,
                                        intervention_coverage,
                                        intervention_impact,
                                        run_no)
            impact_tab = rbind.data.frame(impact_tab, impact_entry)
        }
    }

    # Plot the reduction in vectorial capacity for each intervention
    # and coverages
    if(plot_result) {
        p = plot_impact_var(mosquito_species, impact_tab)
        plot(p)
    }

    return(impact_tab)
}

#########AnophelesModel#####
#' @title main function of the AnophelesModel package
#'
#' @description \code{AnophelesModel} calculates, for a given \emph{Anopheles}
#' mosquito species, the vectorial capacity reduction obtained for a given
#' list of interventions and their deployment coverages.
#' @param mosquito_species string corresponding to the name of the mosquito
#' species to load the bionomic parameters for. This argument is case
#' sensitive and must be one of the species provided with the package:
#' \emph{Anopheles gambiae}, \emph{Anopheles albimanus}, etc.
#' To see the available mosquito species in th package,
#' you can use the function \code{list_all_species()}.
#' @param activity_patterns can be either a string for available default values:
#' "default_Anopheles_gambiae", "default_Anopheles_albimanus"
#' or a list object with the following attributes:
#' \itemize{
#' \item \code{HBI}: proportion of mosquito biting of humans indoors
#' \item \code{HBO}: propotion of mosquito biting of humans outdoors
#' \item \code{humans_indoors}: proportion of humans indoors
#' \item \code{humans_in_bed}: proportion of humans in bed
#' }
#' These attribues can be either time series with the corresponding values at
#' different consecutive time points, or characters indicating
#' an entry ID in the package
#' database. The function \code{list_rhythms()} can be used to visualize the
#' available database entries with activity patterns of mosquitoes and
#' humans for various geographical locations.
#' @param n_time_points number of time points to be considered, default is 100
#' @param total_pop total number of hosts, default is 2000
#' @param n_vec number of mosquitoes born and entering the host-seeking state
#' per day, default is 10000
#' @param interventions list of intervention objects. Each intervention object
#' is a list itself with the following elements (attributes):
#'  \itemize{
#'  \item \code{id}: string with the intervention id. To use an intervention
#'  whose parameterisation is provided in the package database, \code{id} must
#'  be one of "IRS", "LLINs", or "House_screening".
#'  \item \code{description}: string containing a short description of the
#'  intervention, can be used to differentiate between
#'  interventions which have the same id;
#'  \item \code{parameterisation}: string corresponding to the name of the
#'  intervention parameterisation to be used to calculate intervention effects.
#'  To use a parameterisation available in the
#'  package database, this attribute needs to match one of the parameterisations
#'  (column "Parameterisation") included in the intervention summary data table,
#'  \code{interventions_param$interventions_summary}.
#'  For a parameterisation not present in the package database, this
#'  argument can contain any name chosen by the user and the attribute
#'  \code{effects} needs to be provided.
#'  \item \code{effects}: list of intervention survival and effects on the
#'  mosquito oviposition cycle; This attribute is \strong{optional}
#'  if \code{parameterisation} matches one of the intervention parameterisations
#'  available in the package database.
#'
#'  This attribute allows defining intervention effects which are not in
#'  the package database. Most of these effects are defined at
#'  consecutive time points which are
#'  equally distributed throughout the duration of the intervention.
#'  The number of time points needs to match the provided argument
#'  \code{n_time_points}. Except for \code{survival} which is a vector,
#'  these effects are represented by 3-column matrices, where each column,
#'  \code{i=\{1, 2, 3\}}, corresponds to protected human hosts, unprotected
#'  human hosts and animal hosts, respectively. The effects are defined
#'  thorugh the following list attributes:
#'  \itemize{
#'  \item \code{Kvi}: proportion of susceptible mosquitoes that become
#'  infected after biting a host of type i, contains one row and 3 columns
#'  \item \code{alphai}: availability of host i to mosquitoes, contains
#'  \code{n_time_points} rows and 3 columns
#'  \item \code{PBi}: probability that a mosquito bites a host of type i,
#'  contains \code{n_time_points} rows and 3 columns
#'  \item \code{PCi}: probability that a mosquito finds a resting place after
#' biting a host of type i, contains \code{n_time_points} rows and 3 columns
#'  \item \code{PDi}: probability that a mosquito survives the resting phase
#' after biting a host of type i, contains \code{n_time_points}
#' rows and 3 columns
#'  \item \code{PEi}: probability that a mosquito lays eggs and
#' returns to host-seeking after biting a host of type i,
#' contains \code{n_time_points} rows and 3 columns
#'  \item \code{survival}: survival of the intervention,
#'  vector of length \code{n_time_points}
#'  }
#'  }
#'  If the attribute \code{id} is "LLINs" and a parameterisation from
#'  the package is used, the following additional
#'  attributes for the LLIN intervention object must be provided:
#'  \itemize{
#'  \item \code{type}: type of mosquito net
#'  \item \code{country}: country where the data for the chosen net type
#'  was collected.
#'  \item \code{insecticide}: type of insecticide
#'  }
#'  To list all available LLIN types and corresponding countries,
#'  use the function \code{get_net_types()}. For all available insecticide
#'  types, see the data table
#'  \code{interventions_param$LLINs_params$insecticide_c}.
#' @param coverages vector of deployment coverages to be considered for the
#' interventions, default value is \{0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
#' 0.8, 0.9, 0.95, 0.99\}
#' @param plot_result if TRUE, the function plots the reduction of the vectorial
#' capacity for each intervention in the \code{interventions} list
#'
#' @return list object with the following attributes:
#' \itemize{
#' \item \code{vec_p}: list of vector-specific entomological parameters:
#' \itemize{
#' \item \code{species_name}: name of the mosquito species
#' \item \code{M}: parous rate (proportion of host seeking mosquitoes that have
#' laid eggs at least once)
#' \item \code{Chi}: human blood index (proportion of mosquito blood meals
#' derived from humans)
#' \item \code{A0}: sac rate (proportion of mosquitoes who laid eggs the same day)
#' \item \code{zeta.3}: relative availability of different non-human hosts
#' \item \code{td}: proportion of a day that a mosquito actively seeks a host
#' \item \code{tau}: time required for a mosquito that has encountered a host to
#' return to host seeking
#' \item \code{ts}: duration of the extrinsic incubation period (time required
#' for sporozoites to develop in mosquitoes)
#' \item \code{endophily:} proportion of indoor resting mosquitoes
#' \item \code{endophagy:} proportion of indoor feeding mosquitoes
#' }
#'
#' \item \code{host_p}: list of host-specific entomological parameters:
#' \itemize{
#' \item \code{PBi}: probability that a mosquito bites host i
#' \item \code{PCi}: probability that a mosquito finds a resting place after
#' biting a host of type i
#' \item \code{PDi}: probability that a mosquito survives the resting phase
#' after biting a host of type i
#' \item \code{Kvi}:proportion of susceptible mosquitoes that become infected
#' after biting any host of type i
#' }
#' Each vector has three elements corresponding to the probability values for:
#' (1) humans protected by interventions, (2) humans not protected by
#' interventions, and (3) animals.
#' In absence of interventions, the first two values of each
#' vector are identical.
#'
#' \item \code{interventions_vec}: list of intervention objects including their
#' specifications and effects on the vectorial capacity. It contains all the
#' attributes defined above for the \code{interventions} argument of the
#' function. In addition, the \code{effects} list contains the following
#' attributes:
#' \itemize{
#' \item \code{alphai_decay}: vector of length \code{n_time_points} with the
#'  decay of the availability to mosquitoes of humans protected by the
#'  intervention
#'  \item \code{PBi_decay}: vector of length \code{n_time_points} with the
#'  decay of the probability that a mosquito bites humans protected by the
#'  intervention (preprandial effect)
#'  \item \code{PCi_decay}: vector of length \code{n_time_points} with the
#'  decay of the probability that a mosquito finds a resting place after
#'  biting humans protected by the intervention (postprandial effect)
#' \item{vc}: matrix containing the vectorial capacity at each time point
#' (\code{n_time_points} rows) and for the specified intervention coverages
#' (number of columns equal to the length of the \code{coverages} vector)
#' \item{impact} matrix containing the proportion of reduction of the vectorial
#' capacity obtained at each time point (\code{n_time_points} rows) relative
#' to the vectorial capacity before intervention deployment and for the
#' specified intervention coverages (number of columns equal to the length of
#' the \code{coverages} vector)
#' \item{avg_vc}: average vectorial capacity across all time points and for
#' the specified intervention coverages
#' (length equal to the length of the \code{coverages} vector)
#' \item{avg_impact}: average vectorial capacity reduction across all
#' time points and for the specified intervention coverages
#' (length equal to the length of the \code{coverages} vector)
#' }
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @import splines
#'
#' @examples
#' AnophelesModel()
#'
#' @export
#'
AnophelesModel = function(mosquito_species = "Anopheles gambiae",
                          activity_patterns = "default_Anopheles_gambiae",
                          n_time_points = 100, total_pop = 2000, n_vec = 10000,
                          interventions = intervention_obj_examples,
                          coverages = c(seq(0,0.9, by = 0.1), 0.95, 0.99),
                          plot_result = TRUE) {

    # Set the vector parameters
    print("Setting vector parameterization ...")
    vec_params = def_vector_params(mosquito_species)

    # Set the activity patterns
    print("Setting activity patterns ...")
    activity = def_activity_patterns(activity_patterns)

    # Set the host parameters
    print("Setting host-specific parameterization ...")
    hosts_params = def_host_params(mosquito_species)

    # Initialize entomological model-specific parameters
    print("Initializing entomological model ...")
    model_params = build_model_obj(vec_params, hosts_params,
                                     activity, total_pop)

    # Define intervention effects on the transition probabilities between
    # consecutive stages of the mosquito oviposition cycle
    print("Defining interventions effects ...")
    intervention_vec = def_interventions_effects(interventions, model_params,
                                                 n_time_points, FALSE, NULL)

    # Calculate intervention impact
    print("Calculating interventions impact ...")
    impacts = calculate_impact(intervention_vec, coverages, model_params,
                               n_vec, n_time_points)

    # Plot the reduction in vectorial capacity for each intervention
    # and coverages
    if(plot_result) {
        p = plot_impact_species(impacts, "VC_red")
        plot(p)
    }

    return(impacts)
}


