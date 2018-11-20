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
source(here("scripts", "assignFLG-STA.R"))


# # # # # # # # # # # # # # #
# # 2 # bottomUp
# # # #
#
TEMP_TFZ_FRAME_FILEPATH = here("aux", params$value[params$parameter == "TEMP_TFZ_FRAME_FILEPATH"])
OUT_TFZ_LIST_FOR_A_FRAME_FILEPATH = params$value[params$parameter == "TFZ_LIST_FOR_A_FRAME_FILEPATH"]
OUT_TFZ_LIST_DTSTA = params$value[params$parameter == "TFZ_LIST_DTSTA"]
A_FRAME_RESULT_FOLDER = params$value[params$parameter == "A_FRAME_RESULT_FOLDER"]

# Execute:
source(here("scripts", "bottomUp.R"))

# # # # # # # # # # # # # # #
# # 3 # make2x90
# # # #
#
zch3x90 <- read.csv2(here("config", "3x90.csv"), stringsAsFactors = F)$STA
OUT_2x90_FOLDER = params$value[params$parameter == "OUT_2x90_FOLDER"]

# Execute:
source(here("scripts", "make2x90.R"))

# # # # # # # # # # # # # # #
# # 4 # select2x90
# # # #
#
OUT_MAN_AFRAME_FOLDER = params$value[params$parameter == "OUT_MAN_AFRAME_FOLDER"]
OUT_PAIRFRAME_FOLDER = params$value[params$parameter == "OUT_PAIRFRAME_FOLDER"]
OUT_GAINFRAME_FOLDER = params$value[params$parameter == "OUT_GAINFRAME_FOLDER"]
OUT_COMP_OPTI = params$value[params$parameter == "OUT_COMP_OPTI"]
OUT_GAIN_OPTI = params$value[params$parameter == "OUT_GAIN_OPTI"]
OUT_PRE_OPTI = params$value[params$parameter == "OUT_PRE_OPTI"]
OUT_ALL2x90 = params$value[params$parameter == "OUT_ALL2x90"]
OUT_POST_OPTI = params$value[params$parameter == "OUT_POST_OPTI"]
OPT_FOLDER = params$value[params$parameter == "OPT_FOLDER"]
TMP_OPT_FOLDER = params$value[params$parameter == "TMP_OPT_FOLDER"]

# Execute:
source(here("scripts", "select2x90.R"))

# # # # # # # # # # # # # # #
# # 5 # Tagesgang.R
# # # #
#
# source("Tagesgang.R")
