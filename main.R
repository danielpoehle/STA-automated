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
RESULT_VERSION = params$value[params$parameter == "RESULT_VERSION"]
source(here("scripts", "Helper.R"))

# # Configuration Of *relative* paths
STAMMDATEN_FILEPATH = here("aux", params$value[params$parameter == "STAMMDATEN_FILEPATH"])
FAHRLAGEN_FILEPATH = here("aux", params$value[params$parameter == "FAHRLAGEN_FILEPATH"])
FINVEBTS_FILEPATH = here("aux", params$value[params$parameter == "FINVEBTS_FILEPATH"])


# # Configuration for parallel computing
# Note: It is strongly recommended to keep at least one core unccupied, since RStudio and OS
NUMBER_OF_CORES = as.integer(params$value[params$parameter == "NUMBER_OF_CORES"])

# # # # # # # # # # # # # # #
# # 1 # Mapping Fahrlagen -> STA
# # # #
#
# # Input:
#STA_FOLDER = "../ZugChar_Untersuchungen/STAs_/"
WOLKEN_FILEPATH = here("aux", params$value[params$parameter == "WOLKEN_FILEPATH"])
STA_ROUTES_FOLDER = here("aux", "ROUTES", params$value[params$parameter == "STA_ROUTES_FOLDER"])
STA_MAPPING_FILE = here("aux", params$value[params$parameter == "STA_MAPPING_FILE"])
# Output:
OUT_FAHRLAGEN_STAFIT_FILEPATH = params$value[params$parameter == "OUT_FAHRLAGEN_STAFIT_FILEPATH"]
OUT_BTS2STA_FILEPATH = params$value[params$parameter == "OUT_BTS2STA_FILEPATH"]
STAGROUPS_FILEPATH = params$value[params$parameter == "STAGROUPS_FILEPATH"]
STA_RESULT_FOLDER = params$value[params$parameter == "STA_RESULT_FOLDER"]
# Options:
DO_OVERLAPPING = F # overlapping currently bugged
DO_STAFIT = params$value[params$parameter == "DO_STAFIT"]
DO_MAPPING_STA_BTS = params$value[params$parameter == "DO_MAPPING_STA_BTS"]

# Init running time calculations
source(here("scripts", "a-v-calculations.R"))
source(here("scripts", "T10kmCalculator.R"))


# Execute:
#source("newA-V-MitLaufwegen.R")

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
