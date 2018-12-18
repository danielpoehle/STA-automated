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
STAGROUPS_FILEPATH = params$value[params$parameter == "STAGROUPS_FILEPATH"]
STA_RESULT_FOLDER = params$value[params$parameter == "STA_RESULT_FOLDER"]
A_FRAME_RESULT_FOLDER = params$value[params$parameter == "A_FRAME_RESULT_FOLDER"]
OUT_TFZ_LIST_DTSTA = params$value[params$parameter == "TFZ_LIST_DTSTA"]

# Init running time calculations
source(here("scripts", "a-v-calculations.R"))
source(here("scripts", "T10kmCalculator.R"))

# Get Model Trains
fullFrame <- read.csv2(file = helper.getResultPath(OUT_TFZ_LIST_DTSTA), stringsAsFactors = F)
dt <- (fullFrame[!duplicated(fullFrame[,c("TOTALWEIGHT", "TFZ", "NUM_TFZ")]), ])

# Get ZCH for all STA
zch <- read.csv2(file = here("check90", "optimizedSelected.csv"), stringsAsFactors = F)

# Get STA Groups for overlapping STAs
staGroups <- read.csv2(file = helper.getResultPath(STAGROUPS_FILEPATH), stringsAsFactors = F)
staGroups$PARTNER[is.na(staGroups$PARTNER)] <- ""


for(i in 1:length(staGroups$ID)){
  tempFrame <- read.csv2(file = paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_", staGroups$ID[i], ".csv"), stringsAsFactors = F)
  if(staGroups$PARTNER[i] != ""){
    fi <- paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_",
                 staGroups$ID[staGroups$PARTNER == staGroups$PARTNER[i] & staGroups$ID != staGroups$ID[i]], ".csv")
    for(f in fi){
      tempFrame <- rbind(tempFrame, read.csv2(file = f, stringsAsFactors = F))
    }
  }
  aFrame <- read.csv2(paste0(helper.getResultPath(A_FRAME_RESULT_FOLDER), "/", staGroups$ID[i], ".csv"), stringsAsFactors = F)[,-1]

  idx <- which(zch$STA == staGroups$ID[i])


  a1 <- aFrame[which(dt$TFZ == zch$TZF_F1[idx] & dt$TOTALWEIGHT == zch$TOTALWEIGHT_F1[idx] & dt$NUM_TFZ == zch$NUM_TFZ_F1[idx]),][1,]
  a2 <- aFrame[which(dt$TFZ == zch$TZF_F2[idx] & dt$TOTALWEIGHT == zch$TOTALWEIGHT_F2[idx] & dt$NUM_TFZ == zch$NUM_TFZ_F2[idx]),][1,]

  v1 <- tempFrame$VMAX >= zch$VMAX_F1[idx]
  l1 <- tempFrame$LZB >= zch$LZB_F1[idx]
  b1 <- tempFrame$BrH >= zch$BrH_F1[idx]
  res1 <- integer(0)
  if(zch$BREAKCLASS_F1[idx] != "G"){
    res1 <- as.numeric(a1 * v1 * l1 * b1 * as.integer(tempFrame$BREAKCLASS != "G"))
  }else{
    res1 <- as.numeric(a1 * v1 * l1 * b1)
  }

  v2 <- tempFrame$VMAX >= zch$VMAX_F2[idx]
  l2 <- tempFrame$LZB >= zch$LZB_F2[idx]
  b2 <- tempFrame$BrH >= zch$BrH_F2[idx]
  res2 <- integer(0)
  if(zch$BREAKCLASS_F2[idx] != "G"){
    res2 <- as.numeric(a2 * v2 * l2 * b2 * as.integer(tempFrame$BREAKCLASS != "G"))
  }else{
    res2 <- as.numeric(a2 * v2 * l2 * b2)
  }

  if(!is.na(zch$ID_F3[idx])){
    a3 <- aFrame[which(dt$TFZ == zch$TZF_F3[idx] & dt$TOTALWEIGHT == zch$TOTALWEIGHT_F3[idx] & dt$NUM_TFZ == zch$NUM_TFZ_F3[idx]),][1,]
    v3 <- tempFrame$VMAX >= zch$VMAX_F3[idx]
    l3 <- tempFrame$LZB >= zch$LZB_F3[idx]
    b3 <- tempFrame$BrH >= zch$BrH_F3[idx]
    res3 <- integer(0)
    if(zch$BREAKCLASS_F3[idx] != "G"){
      res3 <- as.numeric(a3 * v3 * l3 * b3 * as.integer(tempFrame$BREAKCLASS != "G"))
    }else{
      res3 <- as.numeric(a3 * v3 * l3 * b3)
    }
    val <- round(100.0 * sum(as.numeric(res1 | res2 | res3)) / nrow(tempFrame), 2)
    alert <- ""
    if(val < 90){
      alert <- "  !!!! less than 90% !!!!"
    }
    print(paste0("STA ", staGroups$ID[i], ": ", val, "%", alert))
  }else{
    val <- round(100.0 * sum(as.numeric(res1 | res2)) / nrow(tempFrame), 2)
    alert <- ""
    if(val < 90){
      alert <- "  !!!! less than 90% !!!!"
    }
    print(paste0("STA ", staGroups$ID[i], ": ", val, "%", alert))

  }


}




