# # # # # # # # # # # # # #
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #

# set location of the script and load config params
if(!("here" %in% installed.packages()[,"Package"])){ install.packages("here") }
library("here")
params <- read.csv2(here("config", "main_config.csv"), stringsAsFactors = F)

# helper method to get all needed packages if not installed
helper.neededPackages = read.csv2(here("config", "packages.csv"), stringsAsFactors = F)$name
helper.updatePackages = function() {
  needed_packages = helper.neededPackages
  new_packages = needed_packages[!(needed_packages %in% installed.packages()[,"Package"])]
  if (length(new_packages) > 0){install.packages(new_packages)}
}

# install  and include packages
helper.updatePackages()
for(i in 1:length(helper.neededPackages)){
  library(package = as.character(helper.neededPackages[i]), character.only = T)
}


# # Init helper methods
RESULT_VERSION = params$path[params$parameter == "RESULT_VERSION"]
source(here("Helper.R"))

# # Configuration Of *relative* paths
STAMMDATEN_FILEPATH = here("aux", params$path[params$parameter == "STAMMDATEN_FILEPATH"])
FAHRLAGEN_FILEPATH = here("aux", params$path[params$parameter == "FAHRLAGEN_FILEPATH"])
FINVEBTS_FILEPATH = here("aux", params$path[params$parameter == "FINVEBTS_FILEPATH"])


# # Configuration for parallel computing
# Note: It is strongly recommended to keep at least one core unccupied, since RStudio and OS
NUMBER_OF_CORES = as.integer(params$path[params$parameter == "NUMBER_OF_CORES"])

# # # # # # # # # # # # # # #
# # 1 # Mapping Fahrlagen -> STA
# # # #
#
# # Input:
# #STA_FOLDER = "../ZugChar_Untersuchungen/STAs_/"
# WOLKEN_FILEPATH = "./aux/WolkenBST_v02.csv"
# STA_FOLDER = "./2013_Fahrlagen/ERIKA_STA_181108"
# STA_MAPPING_FILE = "./2013_Fahrlagen/mapping_STAaltneu.csv"
# # Output:
# FAHRLAGEN_STAFIT_FILEPATH = "Fahrlagen_14.11.2013_final_v14_STAFIT.csv"
# BTS2STA_FILEPATH = "bts2sta.csv"
# #STAGROUPS_FILEPATH = "STA_GROUPS_tweaked200.csv"
# STAGROUPS_FILEPATH = "STAGROUPS_v11.csv"
# STA_RESULT_FOLDER = "STAs/"
# # Options:
# DO_OVERLAPPING = F # overlapping currently bugged
# DO_STAFIT = T
# DO_MAPPING_STA_BTS = T
#
# # Execute:
# source("newA-V-MitLaufwegen.R")

#
# # # # # # # # # # # # # # #
# # 2 #
# # # #
#
# # Output:
# TEMP_TFZ_FRAME_FILEPATH = "TFZ_Frame.csv"
# # Execute:
# source("analyzeTFZ+WEIGHT.R")
#
# # # # # # # # # # # # # # #
# # 3 # bottomUp_new_a(v)
# # # #
#
# TFZ_LIST_FOR_A_FRAME_FILEPATH = "TFZ_Frame_aFrame.csv"
# TFZ_LIST_DTSTA = "dtSTA.csv"
# BOTTOMUP_RESULT_FOLDER = "all90/"
# A_FRAME_RESULT_FOLDER = "a_frame/"
#
# source("bottomUp_new_a(v).R")
#
# # # # # # # # # # # # # # #
# # 4a # setCoveringOptimizer
# # # ##
#
# BOTTOMUP_REDUCED_RESULT_FOLDER = "all90/reduced/"
# COVERING_RESULT_FOLDER = "bottomup/merge_av/"
# SELECTION_RESULT_FOLDER = "SelectedFiles_Opti_v01.csv"
#
# source("setCoveringOptimizer.R")
#
# # # # # # # # # # # # # # #
# # 4b # Combining two 90% Characteristics
# # # ##
#
# # coming soon
#
# # # # # # # # # # # # # # #
# # 5 # GainMaximizer.R
# # # #
# OPTIMIZATION_RESULT_FOLDER = "optimized"
# source("GainMaximizer.R")
#
# # # # # # # # # # # # # # #
# # 6 # SystemtrassenAssigner.R
# # # #
#
# source("SystemtrassenAssigner.R")
#
# # # # # # # # # # # # # # #
# # 7 # Tagesgang.R
# # # #
#
# CHOSEN_REM = "13" # String!
# source("Tagesgang.R")
