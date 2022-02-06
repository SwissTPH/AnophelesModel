#########get_OM_ento_snippet####
#' @title creates an xml snippet to be included in an OpenMalaria scenario
#'
#' @description \code{get_OM_ento_snippet} Once the vector and host parameters
#' defined, this function can be used to generate
#' the xml tags for the <entomology> component of the scenario xml file
#' required as input for running the OpenMalaria agent-based simulation model
#' of malaria epidemiology. It creates the <mosq> element containing
#' parameters describing the mosquito life cycle, as well as the <nonHumanHosts>
#' element with parameters for non-human hosts.
#' @param vec_p list of vector-specific entomological parameteres (see
#' \code{def_vector_params} for creating this object)
#' @param hosts_p list of host-specific entomological parameters (see
#' \code{def_host_params} for creating this object)
#'
#' @return list with two xml snippets:
#' \itemize{
#' \item \code{<mosq>}: mosquito species parameters describing the
#' lifecycle and behaviour
#' \item \code{<nonHumanHosts>}: non-human parameters to allow for zoophily
#' }
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @examples
#' vec_params = def_vector_params()
#' hosts_params = def_host_params()
#' xml_snippets = get_OM_ento_snippet(vec_params, hosts_params)
#' print(xml_snippets)
#'
#' @import XML
#'
#' @export
#'
get_OM_ento_snippet = function(vec_params, hosts_params) {
    # Define the <mosq> snippet
    mosq_snippet = newXMLNode("mosq", attrs = list(minInfectedThreshold = 0.01))
    invisible(newXMLNode("mosqRestDuration", parent = mosq_snippet,
                         attrs = list(value = 2)))
    invisible(newXMLNode("extrinsicIncubationPeriod", parent = mosq_snippet,
                         attrs = list(value = vec_params$ts)))
    invisible(newXMLNode("mosqLaidEggsSameDayProportion", parent = mosq_snippet,
                         attrs = list(value = vec_params$A0)))
    invisible(newXMLNode("mosqSeekingDuration", parent = mosq_snippet,
                         attrs = list(value = vec_params$tau)))
    invisible(newXMLNode("mosqSurvivalFeedingCycleProbability", parent = mosq_snippet,
                         attrs = list(value = vec_params$M)))
    invisible(newXMLNode("availability", parent = mosq_snippet))
    invisible(newXMLNode("mosqProbBiting", parent = mosq_snippet,
                         attrs = list(mean = hosts_params$PBi[1],
                                      variance = 0)))
    invisible(newXMLNode("mosqProbFindRestSite", parent = mosq_snippet,
                         attrs = list(mean = hosts_params$PCi[1],
                                      variance = 0)))
    invisible(newXMLNode("mosqProbResting", parent = mosq_snippet,
                         attrs = list(mean = hosts_params$PDi[1],
                                      variance = 0)))
    invisible(newXMLNode("mosqProbOvipositing", parent = mosq_snippet,
                         attrs = list(mean = hosts_params$PEi[1])))
    invisible(newXMLNode("mosqHumanBloodIndex", parent = mosq_snippet,
                         attrs = list(mean = vec_params$Chi)))

    # Define the non-human host snippet
    nonHumanHosts_snippet = newXMLNode("nonHumanHosts",
                                       attrs = list(name = "unprotectedAnimals"))
    invisible(newXMLNode("mosqRelativeEntoAvailability",
                         parent = nonHumanHosts_snippet,
                         attrs = list(value = 1)))
    invisible(newXMLNode("mosqProbBiting",
                         parent = nonHumanHosts_snippet,
                         attrs = list(value = hosts_params$PBi[2])))
    invisible(newXMLNode("mosqProbFindRestSite",
                         parent = nonHumanHosts_snippet,
                         attrs = list(value = hosts_params$PCi[2])))
    invisible(newXMLNode("mosqProbResting",
                         parent = nonHumanHosts_snippet,
                         attrs = list(value = hosts_params$PDi[2])))

    return (list(mosq_snippet = mosq_snippet,
                 nonHumanHosts_snippet = nonHumanHosts_snippet))

    # return(capture.output(cat('<mosq minInfectedThreshold="0.01">
    #     <mosqRestDuration value="2"/>
    #     <extrinsicIncubationPeriod value="', vec_params$ts, '"/>
    #     <mosqLaidEggsSameDayProportion value="', vec_params$A0, '"/>
    #     <mosqSeekingDuration value="', vec_params$tau, '"/>
    #     <mosqSurvivalFeedingCycleProbability value="', vec_params$M, '"/>
    #     <availabilityVariance value="0"/>
    #     <mosqProbBiting mean="', hosts_params$PBi[1], '" variance="0"/>
    #     <mosqProbFindRestSite mean="', hosts_params$PCi[1], '" variance="0"/>
    #     <mosqProbResting mean="', hosts_params$PDi[1], '" variance="0"/>
    #     <mosqProbOvipositing value="', hosts_params$PEi[1], '"/>
    #     <mosqHumanBloodIndex value="', vec_params$Chi, '"/>
    #     </mosq> \n
    #     <nonHumanHosts name="unprotectedAnimals" />
    #     <mosqRelativeEntoAvailability value="', 1.0,'"/>
    #     <mosqProbBiting value="', hosts_params$PBi[2], '"/>
    #     <mosqProbFindRestSite value="', hosts_params$PCi[2], '"/>
    #     <mosqProbResting value="', hosts_params$PDi[2], '"/>
    #     </nonHumanHosts>"', sep="", file = xml_file)))
}

#########get_OM_GVI_snippet####
#' @title creates an xml snippet to be included in an OpenMalaria scenario
#' for the GVI intervention
#'
#' @description \code{get_OM_GVI_snippet} This function can be used to
#' create the relevant xml tags for the <GVI> component of the
#' scenario xml file required as input for running the OpenMalaria
#' agent-based simulation model.
#' Precisely, it creates the <mosq> element containing
#' parameters describing the mosquito life cycle, as well as the <nonHumanHosts>
#' element with parameters specific to animal hosts.
#' @param intervention_impact object created by the AnophelesModel main function
#'
#' @return TO DO
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#' @author Olivier Briët, \email{olivier.briet@swisstph.ch}
#' @author Nakul Chitnis, \email{nakul.chitnis@swisstph.ch}
#' @author Tom Smith, \email{thomas.smith@swisstph.ch}
#'
#' @references TO DO
#'
#' @examples
#'
#' @export
#'
get_OM_GVI_snippet = function(species, intervention_impact, num_points,
                              plot_f = TRUE) {
    id = intervention_impact$id
    # Retrieve the intervention duration
    duration = interventions_param$interventions_summary[which(
        interventions_param$interventions_summary$Intervention ==
            intervention_impact$id &
            interventions_param$interventions_summary$Parameterisation ==
            intervention_impact$parameterisation), "Duration"]
    # Deterrency: 1-alphai
    print("Creating snippet for deterrency")
    if (!is.null(intervention_impact$effects$alphai_decay)){
        best_deterrency_fit = get_best_decay_fit(
            intervention_impact$effects$alphai_decay,
            duration, "deterrency", plot_f)
    } else {
        print("No deterrency effect, decay on alphai is empty.")
        best_deterrency_fit = NULL
    }

    deterrency_snippet = prepare_GVI_snippet(species, best_deterrency_fit,
                                             "deterrency", id)

    # Preprandial killing: 1-PBi
    print("Creating snippet for preprandial killing")
    if (!is.null(intervention_impact$effects$PBi_decay)){
        best_preprandial_fit = get_best_decay_fit(
            intervention_impact$effects$PBi_decay,
            duration,
            "preprandial killing", plot_f)
    } else {
        print("No preprandial effect, decay on PBi is empty.")
        best_preprandial_fit = NULL
    }
    preprandial_snippet = prepare_GVI_snippet(species, best_preprandial_fit,
                                              "preprandial", id)

    # Postprandial killing: decay can be on PDi or on PCi
    print("Creating snippet for postprandial killing")
    if (id == "IRS") {
        if (!is.null(intervention_impact$effects$PDi_decay)) {
            best_postprandial_fit = get_best_decay_fit(
                intervention_impact$effects$PDi_decay,
                duration, "postprandial killing", plot_f)
        } else if (!is.null(intervention_impact$effects$PCi_decay)) {
            best_postprandial_fit = get_best_decay_fit(
                intervention_impact$effects$PCi_decay,
                duration, "postprandial killing", plot_f)
        } else {
            print("No postprandial effect, decays on PCi or PDi are empty.")
            best_postprandial_fit = NULL
        }

    } else if (id == "LLINs") {
        best_postprandial_fit = get_best_decay_fit(
            intervention_impact$effects$PCi_decay,
            duration, "postprandial killing", plot_f)
    }

    postprandial_snippet = prepare_GVI_snippet(species, best_postprandial_fit,
                                               "postprandial", id)
    return(list(deterrency_snippet = deterrency_snippet,
                preprandial_snippet = preprandial_snippet,
                postprandial_snippet = postprandial_snippet))
}
