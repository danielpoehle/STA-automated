# # # # # # # # # # # # # #
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #

# Contents
# . Preparing STAs and Fahrlagen for further calculations
# # # # # # # # # # # # # #
# Maping STA and BTS

if (DO_MAPPING_STA_BTS) { # Checking Block if Mapping enabled

files <- list.files(STA_ROUTES_FOLDER, full.names = T, pattern = ".csv$")
allSta <- unlist(lapply(strsplit(files, "_"), function(x){x[4]}))
staNumber <- sort(unique(allSta))


# Map which BTS belong to a STA
sta <- data.frame(ID <- integer(0), BTS = integer(0), stringsAsFactors = F)
mapping <- read.csv2(file = STA_MAPPING_FILE, stringsAsFactors = F)

for(s in staNumber){
    #print(s)
    ind <- which(s == allSta)
    if(length(ind) > 2){stop("more than 2 STA!")}

    f <- files[ind]
    bts <- integer(0)
    for(i in 1:length(f)){
        bts <- unique(c(bts, read.csv2(f[i], stringsAsFactors = F)$RIL100))
    }
    bts <- bts[bts != "Path Cost " & bts != "Total Priority " & bts != "Total Priority/No.Of Fahrwege "]
    # take only bts with no numbers within
    #bts <- bts[!grepl(".*\\d+.*", bts)]
    old <- mapping$alt[mapping$neu == s]
    sta <- rbind(sta, data.frame(NEW_ID = s, ID = unlist(strsplit(old, "_"))[1],
                                 OLD_STA = old, BTS = bts, stringsAsFactors = F))
}

# Manual Refactorings
id <- c("001", "005", "9A", "173C")
new_id <- c("201", "205", "203", "631")
old_sta <- c("1", "005", "9A", "173C")
bts <- c("AWLB", "AWLB", "AWLB", "RSG")
sta <- rbind(sta, data.frame(NEW_ID = new_id, ID = id, OLD_STA = old_sta, BTS = bts, stringsAsFactors = F))


write.csv2(sta, file = helper.getResultPath(OUT_BTS2STA_FILEPATH), row.names = F)
helper.log("finished mapping BTS to STA")
} else { # Checking Block if Mapping enabled

  sta <- read.csv2(file = helper.getResultPath(OUT_BTS2STA_FILEPATH), stringsAsFactors = F)

}

###### copy staGroups file from aux to current project folder
file.copy(from = here("aux", STAGROUPS_FILEPATH), to = helper.getResultPath(STAGROUPS_FILEPATH), overwrite = T)
helper.requireFile(helper.getResultPath(STAGROUPS_FILEPATH), "Try Copy from Input Folder.")
staGroups <- read.csv2(file = helper.getResultPath(STAGROUPS_FILEPATH), stringsAsFactors = F)



# # # # # # # # # # # # # #
# Meassure share (STAFIT) Fahrlage on STA

if (DO_STAFIT) { # Checking Block if STAFIT enabled

# Load data
data <- read.csv2(file = FAHRLAGEN_FILEPATH, stringsAsFactors = F)
finveBTS <- read.csv2(file = FINVEBTS_FILEPATH, stringsAsFactors = F)

wayPoints <- strsplit(data$WAY, "#")

data$STA_FIT <- integer(nrow(data))
grp <- sort(unique(staGroups$GROUP))

for(i in 1:nrow(data)){
  if(i %% 100 == 0){
    cat (i, " / ", nrow(data)," FLG assigned to STAs \n")
  }
    if(length(wayPoints[[i]]) < 5){next()}
    for(j in grp){
        staNames <- staGroups$ID[staGroups$GROUP == j]
        fit <- double(length(staNames))
        for(k in 1:length(staNames)){
            tBTS <- sta$BTS[sta$ID == staNames[k]]
            fit[k] <- 1.0 * sum(wayPoints[[i]] %in% tBTS) / length(tBTS)
            #if(wayPoints[[i]][1] %in% tBTS){fit[k] <- fit[k] + 0.2}
            #if(wayPoints[[i]][length(wayPoints[[i]])] %in% tBTS){fit[k] <- fit[k] + 0.2}
        }
        id <- which.max(fit)
        if(length(id) == 0 | fit[id] < 0.25){next()}
        data$STA_FIT[i] <- ifelse(data$STA_FIT[i] == 0, staNames[id], paste(data$STA_FIT[i], staNames[id], sep = "#"))
    }
}

print(paste("Number of FLG assigned to a STA:", sum(data$STA_FIT != 0), "of total", length(data$STA_FIT)))
write.csv2(data, file = helper.getResultPath(OUT_FAHRLAGEN_STAFIT_FILEPATH), row.names = F)

} else { # Checking Block if Overlapping enabled

  data <- read.csv2(file = helper.getResultPath(FAHRLAGEN_STAFIT_FILEPATH), stringsAsFactors = F)

}

# # # # # # # # # # # # # #
# Map Fahrlagen to STA (...groups)

helper.safeCreateFolder(helper.getResultPath(STA_RESULT_FOLDER))
sta_resultfile_prefix = paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_")

# Exclude Wolken-BTS from STA trainpaths
wolken <- read.csv2(file = WOLKEN_FILEPATH, stringsAsFactors = F)
sta = sta[!(sta$BTS %in% wolken$BSTID) ,]

staList <- strsplit(data$STA_FIT, "#")
staNumber <- sort(unique(sta$ID))

for(i in 1:length(staNumber)){
    ind <- which(unlist(lapply(staList, function(x) staNumber[i] %in% x)))
    if(length(ind) < 1){
        print(paste(i , "Kein Zug auf STA", staNumber[i],"!"))
        df <- data.frame(TRAINRUN = integer(0), TFZ = integer(0), NUM_TFZ = integer(0),
                         VMAX= integer(0), TOTALLENGTH = integer(0),
                         TOTALWEIGHT = integer(0), BREAKCLASS = integer(0),
                         BrH = integer(0), LZB = integer(0), ELECTRIC = integer(0),
                         TRAINCLASS = integer(0), stringsAsFactors = F)
        write.csv2(df, file = paste0(sta_resultfile_prefix, staNumber[i], ".csv"))
        next()
        }

    trainrun <- data$ANFORDERUNGNAME[ind]



    vmax <- as.integer(data$VMAX[ind])
    brh <- as.integer(data$BrH[ind])
    tfzg <- data$TFZ[ind]
    totalLength <- as.integer(data$TOTALLENGTH[ind])
    totalMass <- as.integer(data$TOTALWEIGHT[ind])
    breakclass <- data$BREAKCLASS[ind]
    num_tfz <- data$NUM_TFZ[ind]
    lzb <- as.logical(data$LZB[ind])
    trainclass <- data$TRAINCLASS[ind]

    currentBTSinSTA <- sta$BTS[sta$ID == staNumber[i]]
    for(j in 1:length(trainrun)){
        changes <- unlist(strsplit(data$CHARARCTERISTIC_CHANGE[ind[j]], "#"))
        if(length(changes) < 1){next()}
        for(m in 1:length(changes)){
            chg <- unlist(strsplit(changes[m], ":"))
            ### get index position of tfzchange and first BTS of STA
            trainPath <- unlist(strsplit(data$WAY[ind[j]], "#"))

            idChange <- min(which(trainPath %in% chg[1]))
            idSTA <- min(unlist(lapply(lapply(currentBTSinSTA, function(x) trainPath %in% x), which)))

            if(is.infinite(idChange) | is.infinite(idSTA)){
                x <- unlist(strsplit(data$WAY[ind[j]], "#"))
                x[which(x == paste0(chg[1],chg[1]))] <- chg[1]
                data$WAY[ind[j]] <- paste(x, collapse = "#")

                trainPath <- unlist(strsplit(data$WAY[ind[j]], "#"))
                idChange <- min(which(trainPath %in% chg[1]))

                if(is.infinite(idChange) | is.infinite(idSTA)){stop("BTS in trainPath not found")}

            }

            ### if tfzChange is on or after first BTS of STA then ignore it
            if (idChange > idSTA){ # replaced '>=' with '>' to match changes on the first BTS
                #print(paste("change on or after STA --> ignore it:", trainrun[j]))
                next()
            }
            ### if tfzChange is before first BTS of STA then change
            ### replace the characteristic
            #print(paste("change before STA --> replace:", trainrun[j], chg[2]))
            diffChar <- unlist(strsplit(chg[2], "\\$"))
            for(k in 1:length(diffChar)){
                val <- unlist(strsplit(diffChar[k], " "))
                if(val[1] == "TFZ"){
                    if(length(val) > 2){
                        val[2] <- paste(val[2:length(val)], collapse = " ")
                    }
                    if(val[2] == "28. Jan"){val[2] <- "28-1"}
                    tfzg[j] <- val[2]
                    next()
                }
                if(val[1] == "TOTALLENGTH"){
                    totalLength[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "WEIGHT"){
                    totalMass[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "LZB"){
                    lzb[j] <- as.logical(val[2])
                    next()
                }
                if(val[1] == "BrH"){
                    brh[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "VMAX"){
                    vmax[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "NUM_TFZ"){
                    num_tfz[j] <- as.integer(val[2])
                    next()
                }
                if(val[1] == "BREAKCLASS"){
                    breakclass[j] <- val[2]
                    next()
                }
            }
        }
    }

    df <- data.frame(TRAINRUN = trainrun, TFZ = tfzg, NUM_TFZ = num_tfz,
                     VMAX= vmax, TOTALLENGTH = totalLength,
                     TOTALWEIGHT = totalMass, BREAKCLASS = breakclass,
                     BrH = brh, LZB = lzb, ELECTRIC = integer(length(lzb)),
                     TRAINCLASS = trainclass, stringsAsFactors = F)

    for(p in 1:length(df$TRAINRUN)){
        #print(p)
        elem <- tfzNames[tfzNames$name == df$TFZ[p], ]
        df$ELECTRIC[p] <- as.integer(xmlGetAttr(tfz[[elem$i]][["Triebfahrzeugbaureihenvarianten"]][[elem$j]][["Stromartausruestungen"]][["Stromartausruestung"]][["Stromart"]],
                                                "Schluessel", "0")) != 0
    }

    write.csv2(df, file = paste0(sta_resultfile_prefix, staNumber[i], ".csv"))

    helper.log(paste0(" STA ", staNumber[i], ": number of trains assigned: ", length(df$TRAINRUN)))
}



################ STOP HERE AND CONTINUE WITH NEXT FILE #############################################################

