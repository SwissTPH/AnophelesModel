#################################
# Main initialization script of the workflow, creates for each country
# an analysis directory with all the files and scripts
# consisting of the following steps:
# 1. Defining the base xml
# 1. Defining the simulation settings 
# 2. Creating the scripts for generating the simulation scenarios 
# 3. Creating the scripts for running OpenMalaria simulations
# 4. Creating the scripts for postprocessing
#
# The script creates for each country a folder with all the scripts.
#
# created 22.09.2021
# monica.golumbeanu@unibas.ch
#################################

# History cleanup
rm(list=ls())

# Package installation for the first time:
# To install OpenMalariaUtilities:
# devtools::install_github("SwissTPH/r-openMalariaUtilities", force = TRUE, ref = "master") #database optimization: db-opt
# To install OMAddons and OMSlurm, first you need to clone the GitLab repositories and then:
# devtools::install("~/GitRepos/omuaddons/")
# devtools::install("~/GitRepos/omu-slurm/")
# devtools::install("~/GitRepos/omu-compat/")

# Load the necessary packages
library(devtools)
library(openMalariaUtilities)
library(OMAddons)
library(omuslurm)
library(omucompat)

#####################################
# Initialization
#####################################

# Set working directory to the one where this file is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
print(paste("Working directory set to ", dirname(rstudioapi::getActiveDocumentContext()$path)))

# Load the base xml list setup function and auxiliary functions
source("00_create_base_PNG.R")

# Define root directory with all the experiments according to the user
if (Sys.getenv("USER") == "golmon00") {
  root_dir_path = "/scicore/home/pothin/golmon00/OpenMalaria/AnophelesModel/"
} else {
  print("Please specify the paths to the necessary folders!")
}

iso_code = "PNG"

#####################################
# Experiment setup
#####################################
# Definition of the main country folder where all results will be stored
# clearCache
setupDirs(experimentName = iso_code, rootDir = root_dir_path, replace = TRUE)

# Initialize cache and create the base xml file
baseList_country = create_baseList(country_name = iso_code,
                                   sim_start = "1918-01-01",
                                   versionnum = 44L)
createBaseXml(baseList_country, replace = TRUE)

## Copy necessary Open Malaria files. It needs to be called after
## createBaseXml, otherwise the cache is not set up.
setupOM()

#####################################
# Defining the simulation scenarios
#####################################
# Define the list with all the scenario variations per country
# (population size, seed, EIR)
full = list()
full$seed = c(1:10)
full$setting = iso_code
full$EIR = 10

#### 'scens' will contain all possible combinations of these scenario variations
scens = expand.grid(full)

print(paste(nrow(scens), "scenarios defined for", iso_code))

# Ensure that the scenario dataframe is correctly defined (adds ID and file columns)
scens = finalizeScenarios(scens)

# Store the scenarios in the cache folder
storeScenarios(scens)

#####################################
# Validate the xml
#####################################
if (validateXML(xmlfile = getCache(x = "baseXml"), 
                schema = paste0(getCache(x = "experimentDir"), "/scenario_44.xsd"), 
                scenarios = scens)) {
  print ("XML definition is valid.")
} 

#####################################
# Prepare scripts for creating, running and postprocessing all the scenarios and simulations
#####################################
print ("Generating analysis scripts ...")
## 1. Prepare the scripts for creating the scenarios
# Make sure to adjust the nCPU, memCPU, time and qos if you run larger experiments
slurmPrepareScenarios(expName = iso_code, scenarios = scens, nCPU = 10, 
                      memCPU = "350MB")

## 2. Prepare the scripts for running OpenMalaria simulations
# Make sure to adjust the nCPU, memCPU, time and qos if you run larger experiments
slurmPrepareSimulations(expName = iso_code, scenarios = scens, 
                        memCPU = "100MB", nCPU = 10)

## 3. Prepare the scripts for post-processing the OpenMalaria outputs
# Define the age groups of interest for the outputs (including aggregations)
age_groups_list = c("0-5", "2-10", "0-100")

# Define the OpenMalaria outputs of interest; these will correspond to the 
# columns of the results table to be stored in the database
results_columns = c("scenario_id",
                    "date", "age_group", "date_aggregation", 
                    "nTreatments1", "nTreatments2", "nTreatments3", 
                    "nHost", "nUncomp", "nSevere",
                    "tUncomp", "tSevere",
                    "incidenceRate", "prevalenceRate")

# Remove the results database if it already exists
# Overwriting an existing database with the same scenario IDs will not work
db_file = file.path(paste0(getCache("rootDir"), iso_code, ".sqlite"))
if (file.exists(db_file)) {
  print(paste0("A database for ", iso_code, " exists already and will be removed."))
  file.remove(db_file)
}

# Generate the postprocessing script
# Make sure to adjust the nCPU, memCPU, time and qos if you run larger experiments
slurmPrepareResults(expDir = getCache("experimentDir"), dbName = iso_code,
                    resultsName = "om_results", resultsCols = results_columns,
                    aggrFun = CalcEpiOutputs,
                    aggrFunArgs = list(indicators = results_columns, 
                                       aggregateByAgeGroup = age_groups_list,
                                       aggregateByDate = "month"),
                    ntasks = 1, mem = "1GB", nCPU = 10, 
                    strategy = "batch", indexOn = NULL)


