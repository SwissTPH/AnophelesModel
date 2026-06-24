#########get_OM_ento_snippet####
#' @title Create an OpenMalaria entomology XML snippet
#'
#' @description Generates the \code{<mosq>} and \code{<nonHumanHosts>} XML
#' tags for the \code{<entomology>} section of an OpenMalaria scenario file,
#' based on vector and host entomological parameters.
#'
#' @param vec_params List of vector-specific entomological parameters
#' (see \code{def_vector_params}).
#' @param hosts_params List of host-specific entomological parameters
#' (see \code{def_host_params}).
#'
#' @return List with two XML node objects: \code{mosq_snippet} and
#' \code{nonHumanHosts_snippet}.
#'
#' @author Monica Golumbeanu, \email{monica.golumbeanu@swisstph.ch}
#'
#' @examples
#' vec_params   = def_vector_params()
#' hosts_params = def_host_params()
#' xml_snippets = get_OM_ento_snippet(vec_params, hosts_params)
#' print(xml_snippets)
#'
#' @import XML
#'
#' @export
#'
get_OM_ento_snippet = function(vec_params, hosts_params) {
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
                         attrs = list(mean = hosts_params$PBi[1], variance = 0)))
    invisible(newXMLNode("mosqProbFindRestSite", parent = mosq_snippet,
                         attrs = list(mean = hosts_params$PCi[1], variance = 0)))
    invisible(newXMLNode("mosqProbResting", parent = mosq_snippet,
                         attrs = list(mean = hosts_params$PDi[1], variance = 0)))
    invisible(newXMLNode("mosqProbOvipositing", parent = mosq_snippet,
                         attrs = list(mean = hosts_params$PEi[1])))
    invisible(newXMLNode("mosqHumanBloodIndex", parent = mosq_snippet,
                         attrs = list(mean = vec_params$Chi)))

    nonHumanHosts_snippet = newXMLNode("nonHumanHosts",
                                       attrs = list(name = "unprotectedAnimals"))
    invisible(newXMLNode("mosqRelativeEntoAvailability",
                         parent = nonHumanHosts_snippet, attrs = list(value = 1)))
    invisible(newXMLNode("mosqProbBiting",
                         parent = nonHumanHosts_snippet,
                         attrs = list(value = hosts_params$PBi[2])))
    invisible(newXMLNode("mosqProbFindRestSite",
                         parent = nonHumanHosts_snippet,
                         attrs = list(value = hosts_params$PCi[2])))
    invisible(newXMLNode("mosqProbResting",
                         parent = nonHumanHosts_snippet,
                         attrs = list(value = hosts_params$PDi[2])))

    return(list(mosq_snippet = mosq_snippet,
                nonHumanHosts_snippet = nonHumanHosts_snippet))
}
