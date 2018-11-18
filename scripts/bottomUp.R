# # # # # # # # # # # # # #
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #
#source("a-v-calculations.R")
#source("T10kmCalculator.R")

# STA
staFolder <- helper.getResultPath(STA_RESULT_FOLDER)
helper.safeCreateFolder(staFolder)

files <- list.files(path = staFolder, full.names = T, pattern = ".csv$")
fileNames <- list.files(path = staFolder, full.names = F, pattern = ".csv$")

staGroups <- read.csv2(file = helper.getResultPath(STAGROUPS_FILEPATH), stringsAsFactors = F)
staGroups$PARTNER[is.na(staGroups$PARTNER)] <- ""

dt <- read.csv2(file = TEMP_TFZ_FRAME_FILEPATH, stringsAsFactors = F)
dt <- dt[dt$TOTALWEIGHT >= 500,]
#dt <- dt[!duplicated(dt[,c("TFZ", "TOTALWEIGHT", "NUM_TFZ")]),c("TFZ", "TOTALWEIGHT", "NUM_TFZ", "VMAX")]

i_STA <- sort(unique(staGroups$i))
dtSTA <- data.frame()
for(j in 1:length(i_STA)){
  d1 <- cbind(dt, rep(i_STA[j], length(dt$TOTALWEIGHT)), stringsAsFactors = F)
  names(d1) <- c("TOTALWEIGHT", "BREAKCLASS", "VMAX", "TFZ", "NUM_TFZ", "I")
  dtSTA <- rbind(dtSTA, d1)
}
dt <- dt[!duplicated(dt[, c("TOTALWEIGHT", "TFZ", "NUM_TFZ")]),]

dt$T10WithI <- NULL
dtSTA$T10WithI <- NULL
avList <- list()
helper.log("start calculate T10km dtSTA")
for(j in 1:nrow(dt)){
  if(j %% 100 == 0){
    helper.log(paste(j, "/", nrow(dt)))
  }
    elem <- tfzNames[tfzNames$name == dt$TFZ[j], ]
    avModel = getAVModel(elem$i, elem$j, dt$TOTALWEIGHT[j], dt$NUM_TFZ[j], addTfzMass = T)
    avList <- c(avList, list(avModel))

    ind <- which(dtSTA$TOTALWEIGHT == dt$TOTALWEIGHT[j] & dtSTA$TFZ == dt$TFZ[j] & dtSTA$NUM_TFZ == dt$NUM_TFZ[j])

    for(i in ind){
      dtSTA$T10WithI[i] <- 0.5* calculate10kmWithI(avModel, dtSTA$VMAX[i], dtSTA$BREAKCLASS[i], dtSTA$I[i]) +
                           0.5* calculate10km(avModel, dtSTA$VMAX[i], dtSTA$BREAKCLASS[i])
    }
}

write.csv2(dt, file = helper.getResultPath(OUT_TFZ_LIST_FOR_A_FRAME_FILEPATH), row.names = F)
write.csv2(dtSTA, file = helper.getResultPath(OUT_TFZ_LIST_DTSTA), row.names = F)
helper.log("finished dtSTA")

tempFrame <- data.frame()
for(i in 1:length(files)){
  #cat (i, "/", length(files),"\n")
  tempFrame <- rbind(tempFrame, read.csv2(file = files[i], stringsAsFactors = F))
}

ds <- tempFrame[!duplicated(tempFrame[,c("TFZ", "TOTALWEIGHT", "NUM_TFZ")]),c("TFZ", "TOTALWEIGHT", "NUM_TFZ", "VMAX")]
#ds <- rbind(dt, data.frame(TRACTION = c("189-2", "185-2", "185-2", "185-2", "203-1"), TOTALMASS = c(2945, 4000, 5000, 5500, 1950), DOUBLETRACTION = F, VMAX = 100, stringsAsFactors = F))


avSTA <- list()
for(j in 1:nrow(ds)){
  #cat (j, "/", nrow(ds),"\n")
  elem <- tfzNames[tfzNames$name == ds$TFZ[j], ]
  avSTA <- c(avSTA, list(getAVModel(elem$i, elem$j, ds$TOTALWEIGHT[j], ds$NUM_TFZ[j], addTfzMass = F)))
}

# Prepare Paths
sta_resultfile_prefix = paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_")
#helper.safeCreateFolder(helper.getResultPath(BOTTOMUP_RESULT_FOLDER))
aframe_resultfile_prefix = paste0(helper.getResultPath(A_FRAME_RESULT_FOLDER), "/")
helper.safeCreateFolder(helper.getResultPath(A_FRAME_RESULT_FOLDER))

# Settting up Parallel Computing
cl <- makeCluster(NUMBER_OF_CORES)
registerDoParallel(cl)
list_of_done = logical(nrow(staGroups))

# Execute a_frame / T10 calculation as a parrallized loop
helper.log("start calculate a_frames")
res = foreach(i = 1:nrow(staGroups),
              .export = c("getReduction", "helper.getResultPath", "calculate10kmAcceleration",
                          "calculate10km", "calculate10kmWithI", "getAVModel", "helper.log" ),
              .packages = c("XML")) %dopar% {
                # Init message
                msg = paste0("Started STA ", staGroups$ID[i], " (", i, "/", nrow(staGroups), ") \n")
                helper.log(msg)

                # Load Data
                tempFrame <- read.csv2(file = paste0(sta_resultfile_prefix, staGroups$ID[i], ".csv"), stringsAsFactors = F)
                if(staGroups$PARTNER[i] != ""){
                  fi <- paste0(sta_resultfile_prefix, staGroups$ID[staGroups$PARTNER == staGroups$PARTNER[i] & staGroups$ID != staGroups$ID[i]], ".csv")
                  for(f in fi){
                    tempFrame <- rbind(tempFrame, read.csv2(file = f, stringsAsFactors = F))
                  }
                }
                tempFrame$VMAX[is.na(tempFrame$VMAX)] <- min(tempFrame$VMAX, na.rm = T)


                ninety <- ceiling(0.9*length(tempFrame$TRAINRUN))
                lng <- length(tempFrame$X)

                v <- unique(sort(tempFrame$VMAX, decreasing = T)[ninety:lng])
                b <- unique(sort(tempFrame$BrH, decreasing = T)[ninety:lng])
                # 1 - class P or better, 0 - class G
                c <- ifelse(unique(sort(tempFrame$BREAKCLASS, decreasing = T)[ninety:lng]) != "G", "P", "G")


                # Check compatibility tempFrame <-> avList
                # Result is the "a_frame"

                a_frame <- data.frame(tr = seq(1,length(tempFrame$X)))

                for(j in 1:length(avList)){
                  ind <- max(which(avList[[j]]$a >=0 & !is.na(avList[[j]]$s_kum)))

                  # get tolerance of a(v)
                  reduce <- getReduction(avList[[j]]$a[1], avList[[j]]$a[ind])
                  a_tol <- avList[[j]]$a - seq(0.1,1,0.009)* reduce
                  check_a <- logical(length(tempFrame$X))

                  for(k in 1:length(tempFrame$X)){
                    p <- which(tempFrame$TFZ[k] == ds$TFZ & tempFrame$TOTALWEIGHT[k] == ds$TOTALWEIGHT & tempFrame$NUM_TFZ[k] == ds$NUM_TFZ)
                    ind_tf <- max(which(avSTA[[p]]$a >=0 & !is.na(avSTA[[p]]$s_kum)))
                    vm <- min(avSTA[[p]]$v[ind_tf], tempFrame$VMAX[k])
                    ind <- min(ind, which(avSTA[[p]]$v == vm))
                    check_a[k] <- sum(avSTA[[p]]$a[1:ind] >= a_tol[1:ind]) == ind
                  }
                  a_frame <- cbind(a_frame, check_a)
                }
                names(a_frame) <- c("tr", seq(1:length(avList)))

                write.csv2(t(a_frame)[-1,], file = paste0(aframe_resultfile_prefix, staGroups$ID[i], ".csv"), row.names = T)
}
stopCluster(cl) # shut down the cluster
helper.log("finished calculate a_frames")

################ STOP HERE AND CONTINUE WITH NEXT FILE #############################################################

