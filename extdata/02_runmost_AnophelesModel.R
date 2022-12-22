#################################
# Main running script of the workflow, executes the following steps
# for each country:
# 1. Generates the simulation scenarios 
# 2. Runs OpenMalaria simulations
# 3. Postprocessing
#
# created 27.09.2021
# monica.golumbeanu@unibas.ch
#################################

# History cleanup
rm(list=ls())

# T run this in a screen session, first you need to load the R module:
# module purge
# module load R/4.1.2-foss-2018b-Python-3.6.6

# Load the necessary packages
library(devtools)
library(openMalariaUtilities)
library(OMAddons)
library(omuslurm)
library(omucompat)
library(RSQLite)

# Define root directory with all the experiments according to the user
print(paste("User: ", Sys.getenv("USER")))
if (Sys.getenv("USER") == "golmon00") {
  root_dir_path = "/scicore/home/pothin/golmon00/OpenMalaria/AnophelesModel/"
} else {
  print("Please specify the paths to the necessary folders!")
}

iso_code = "PNG"

print(paste("Running analysis for", iso_code))

# Load experiment
start_time = Sys.time()
print(paste(Sys.time(), "-> Loading experiment ..."))
experiment_folder = paste0(root_dir_path, iso_code)
loadExperiment(experiment_folder)

# Create scenarios
print(paste(Sys.time(), "-> Creating scenarios ..."))
slurmCreateScenarios()

# Check that all scenarios were created
check_scenarios_created(experiment_folder, stop_if_missing = TRUE)

print(paste(Sys.time(), "-> Running OpenMalaria simulations ..."))
slurmRunSimulation()

# Check that all OpenMalaria simulations were created
check_simulations_created(experiment_folder, stop_if_missing = FALSE)

print(paste(Sys.time(), "-> Running postprocessing ..."))
slurmRunResults()
end_time = Sys.time()

print(paste(Sys.time(), "-> Finished running for", iso_code))
print(as.difftime(end_time - start_time, format = "%X", units = "auto", tz = "CET"))

