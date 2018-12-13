if(!("here" %in% installed.packages()[,"Package"])){ install.packages("here") }
library("here")
params <- read.csv2(here("config", "main_config.csv"), stringsAsFactors = F)
# # Init helper methods
RESULT_VERSION = params$value[params$parameter == "RESULT_VERSION"]
source(here("scripts", "Helper.R"))

# # Configuration Of *relative* paths
STAMMDATEN_FILEPATH = here("aux", params$value[params$parameter == "STAMMDATEN_FILEPATH"])
FAHRLAGEN_FILEPATH = here("aux", params$value[params$parameter == "FAHRLAGEN_FILEPATH"])
FINVEBTS_FILEPATH = here("aux", params$value[params$parameter == "FINVEBTS_FILEPATH"])

# Init running time calculations
source(here("scripts", "a-v-calculations.R"))
source(here("scripts", "T10kmCalculator.R"))

zch <- read.csv2(file = "./check90/checkZCH.csv", stringsAsFactors = F)
stas <- read.csv2(file = "./check90/STA.csv", stringsAsFactors = F)
if(length(zch$TFZ) > 3 || length(zch$TFZ) < 1){ stop("number of TFZ in input file is wrong: 0 or more than 3 ZCH in checkZCH.csv!") }
if(length(stas$STA_NAME) < 1){ stop("at least one STA needs to be defined in STA.csv") }

helper.log(paste("\n\ncheck ZCH in", helper.getResultPath(""), "for STA", paste(stas$STA_NAME, collapse = " "), "\n"))
helper.log("\n\n################### Input ZCH ######################")
print(zch)


