###############################
# Script for creating the base xml for the SSA commodity forecast analysis using
# the latest version of OpenMalariaUtilities available at:
# https://github.com/SwissTPH/r-openMalariaUtilities
#
# Varied components:
# - age structure
# - seasonality
# - vectors contribution to transmission
# - historical interventions (ITN, IRS, CM)
#
# Output: a base .xml file containing @placeholders@ for the varied parameters.
# These placeholders will be populated with country_name-specific values.
#
# 20.09.2022
# monica.golumbeanu@unibas.ch
###############################

# To install OpenMalariaUtilities:
# devtools::install_github("SwissTPH/r-openMalariaUtilities", force = TRUE)

# Load the necessary packages
library(devtools)
library(openMalariaUtilities)
library(tidyverse)
library(omucompat)
library(OMAddons)
library(stringr)
library(AnophelesModel)

# Function that creates a list with all the elements which are specific for the 
# base xml (placeholders, interventions, etc.)
create_baseList = function(country_name, sim_start, versionnum) {
  
  ## Basic xml skeleton
  baseList = list(
    # Mandatory
    expName = country_name,
    # Mandatory
    OMVersion = versionnum,
    # Mandatory
    demography = list(),
    monitoring = list(),
    interventions = list(),
    healthSystem = list(),
    entomology = list(),
    # These are optional for OM
    # parasiteGenetics = list(),
    # pharmacology = list(),
    # diagnostics = list(),
    model = list()
  )
  
  # Create demography
  baseList = defineDemography(baseList,
                              name = country_name,
                              popSize = 10000L,
                              maximumAgeYrs = 85,
                              lowerbound = 0,
                              poppercent = c(0.03142771,
                                             0.1186073,
                                             0.1240923,
                                             0.1227429,
                                             0.1177359,
                                             0.1056282,
                                             0.0817358,
                                             0.06877233,
                                             0.05405937,
                                             0.04480186,
                                             0.03526427,
                                             0.02830656,
                                             0.02189422,
                                             0.01647409,
                                             0.01234103,
                                             0.008166892,
                                             0.004809187,
                                             0.00332171),
                              upperbound = c(1, seq(5, 85, by = 5)))
  
  ## Create monitoring snippet
  baseList[["monitoring"]] = list(
    name = "Yearly Surveys",
    ## Mandatory, different from OM schema
    startDate = sim_start,
    continuous = monitoringContinuousGen(period = 1,
                                         options = list(
                                           name = c("input EIR", "simulated EIR", "human infectiousness", "N_v0",
                                                    "immunity h", "immunity Y", "new infections",
                                                    "num transmitting humans", "ITN coverage", "GVI coverage", "alpha",
                                                    "P_B", "P_C*P_D"),
                                           value = c("true", "true", "true", "true", "true", "true", "true", "true",
                                                     "true", "true", "true", "false", "false")
                                         )
    ),
    SurveyOptions = monitoringSurveyOptionsGen(
      options = list(
        name = c("nHost", "nPatent", "nUncomp", "nSevere", "nDirDeaths",
                 "inputEIR", "simulatedEIR","nTreatments1","nTreatments2","nTreatments3"),
        value = c("true", "true", "true", "true", "false", "true", "true", "true", "true", "true")
      )
    ),
    surveys = monitoringSurveyTimesGen(detectionLimit = 100, startDate = "1999-01-01", #sim_start
                                       endDate = "2025-01-01", 
                                       interval = list(days = c(5), months = c(1:12), years = c(1998:2025)), 
                                       simStart = sim_start),
    ## surveyAgeGroupsGen will write thirdDimension table to cache, important for postprocessing
    ageGroup = surveyAgeGroupsGen(lowerbound = 0, upperbounds = c(1, 2, 5, 10, 100))
  )
  
  ## Entomology section: MANDATORY
  seasonalityParameters = list(`Anopheles farauti` = list(annualEIR="15",
                                                          input="EIR",
                                                          seasonality=c(0.1051963,
                                                                        0.1047198,
                                                                        0.1033515,
                                                                        0.1120686,
                                                                        0.09736436,
                                                                        0.07414688,
                                                                        0.05476991,
                                                                        0.0550555,
                                                                        0.05779579,
                                                                        0.06660052,
                                                                        0.08033242,
                                                                        0.08859837),
                                                          propInfected="0.078",
                                                          propInfectious="0.021"))
  # Define vector species in the simulation
  vectorSpecies = c("Anopheles farauti")
  mosquitoParameters = mosquitoParameterization(vectorSpecies)
  
  baseList$entomology$vector=list()
  baseList = defineEntomology(baseList, seasonalityParameters, 
                              mosquitoParameters)
  
  # Begin interventions for humans
  interventionList = list(LLIN_interv = list(id="LLINs", 
                                             description="test LLIN",
                                             parameterisation="LLINs01",
                                             LLIN_type="Default",
                                             LLIN_insecticide="Default",
                                             LLIN_country="Kenya"))
  
  vectorControlParameters = vectorControlParameterization(vectorSpecies,
                                                          interventionList)
  # Define LLIN intervention
  baseList$interventions$human = list()
  baseList = define_vector_control(baseList, vectorControlParameters)
  
  ## Deployment section
  
  # ITN deployment:
  
  # At the moment the workflow is not flexible enough to allow custom GVI values.
  # So we need to replace the GVI effect values directly in the base xml with:
  
  # $deterrency_snippet
  # $deterrency_snippet$GVI_xml_snippet
  # <GVI name="GVI_LLINs" id="GVI_LLINs_1">
  #   <anophelesParams mosquito="Anopheles farauti" propActive="1"/>
  #   <decay L="8.7559913701415" function="weibull" k="0.701628143609033"/>
  #   <deterrency value="0.522400289851454"/>
  #   <preprandialKillingEffect value="0"/>
  #   <postprandialKillingEffect value="0"/>
  #   </GVI> 
  
  # $preprandial_snippet$GVI_xml_snippet
  # <GVI name="GVI_LLINs" id="GVI_LLINs_1">
  #   <anophelesParams mosquito="Anopheles farauti" propActive="1"/>
  #   <decay L="1.86705167285381" function="weibull" k="1.7217937150353"/>
  #   <deterrency value="0"/>
  #   <preprandialKillingEffect value="0.65024760042102"/>
  #   <postprandialKillingEffect value="0"/>
  #   </GVI> 
  
  # $postprandial_snippet$GVI_xml_snippet
  # <GVI name="GVI_LLINs" id="GVI_LLINs_1">
  #   <anophelesParams mosquito="Anopheles farauti" propActive="1"/>
  #   <decay L="1.37722671904136" function="weibull" k="1.01424385957538"/>
  #   <deterrency value="0"/>
  #   <preprandialKillingEffect value="0"/>
  #   <postprandialKillingEffect value="0.431483113001422"/>
  #   </GVI> 
    
  baseList = deploy_IT(baseList = baseList, component = "LLIN_interv", 
                       effects=c("deterrency", "preprandialKillingEffect", 
                                 "postprandialKillingEffect"),
                       coverage = 0.6, dates = c("2000-01-01"))
  
  # Importation: MANDATORY
  baseList = define_importedInfections_compat(baseList = baseList, 10, time = 0)
  
  # Health system
  # Write health system: MANDATORY
  baseList = define_health_system(baseList = baseList, 
                                  pSeekOfficialCareUncomplicated1 = 0.5,
                                  pSeekOfficialCareUncomplicated2 = 0.5)
  
  ## Specify seed: MANDATORY
  baseList = write_end_compat(baseList = baseList, 
                              seed = "@seed@", modelname = "base")
  
  return(baseList)
}

# For testing
# a = create_baseList("Kenya", "1918-01-01", 44L) 

