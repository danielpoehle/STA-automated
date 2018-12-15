# # # # # # # # # # # # # #
# ! # Change Working directory: Session > Set Working Directory > To Source File Location
# # #

#library(ggplot2)

staGroups <- read.csv2(file = helper.getResultPath(STAGROUPS_FILEPATH), stringsAsFactors = F)
grp <- sort(unique(staGroups$GROUP))

data <- read.csv2(file= helper.getResultPath(OUT_FAHRLAGEN_STAFIT_FILEPATH), stringsAsFactors = F)
sta <- read.csv2(file = helper.getResultPath(OUT_BTS2STA_FILEPATH), stringsAsFactors = F)
staMapping <- read.csv2(file = STA_MAPPING_FILE, stringsAsFactors = F)
staMapping$oldNames <- gsub("_H", "", gsub("_R", "", staMapping$alt))
staRoutes <- list.files(path = STA_ROUTES_FOLDER, pattern = ".csv$", full.names = T)



working_folder = helper.getResultPath(OPT_FOLDER)

staFiles <- list.files(path = working_folder, full.names = T, pattern = ".csv$")
staNames <- gsub(".csv", "", list.files(path = working_folder, full.names = F, pattern = ".csv$"))

wayPoints <- strsplit(data$WAY, "#")
timePoints <- strsplit(data$DEP, "#")
helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang"))

for(i in 1:length(staFiles)){
    helper.log(paste("generate TGL for STA", staNames[i]))
    newSTA <- staMapping$neu[staMapping$oldNames == staNames[i]]
    ind <- which(grepl(pattern = "_H", x = staMapping$alt[staMapping$neu %in% newSTA]))
    if(length(newSTA) != 2){stop("not two STA!")}
    tBTS <- read.csv2(file = staRoutes[which(grepl(pattern = paste0("_", newSTA[ind], "_"), staRoutes))], stringsAsFactors = F)$RIL100
    tempFrame <- read.csv2(file = staFiles[i], stringsAsFactors = F)
    sys <- unique(tempFrame$NAME)
    sys <- sys[sys!="no"]
    result <- data.frame()
    for(j in 1:length(sys)){
        timeStamps <- integer(0)
        direction <- integer(0)
        trNumbers <- tempFrame$TRAINRUN[tempFrame$NAME == sys[j]]
        relation <- integer(0)
        for(k in 1:length(trNumbers)){
            idx <- which(data$ANFORDERUNGNAME %in% trNumbers[k])
            if(length(idx) > 1){
              cover <- integer(length(idx))
              for(v in 1:length(idx)){
                for(n in 1:length(tBTS)){
                  cover[v] <- cover[v] + sum(wayPoints[[idx[v]]] %in% tBTS[n])
                }
              }
              idx <- idx[which.max(cover)]
            }
            w <- wayPoints[[idx]]
            for(n in 1:length(tBTS)){
                found <- which(w %in% tBTS[n])
                if(length(found) > 1){
                    found <- found[1]
                    #print(paste(n, "more than one"))
                }
                if(length(found) == 1 && timePoints[[idx]][found] != "NA"){
                    newTimeStamp <- timePoints[[idx]][found]
                    break()
                }
            }

            first <- integer(0)
            for(n in 1:length(tBTS)){
                found <- which(w %in% tBTS[n])
                if(length(found) > 1){
                    found <- found[1]
                }
                if(length(found) == 1 && length(first)==0){
                    first <- found
                }else if(length(found) == 1 && length(first)==1){
                    if(found > first){
                        direction <- paste(direction, as.character(newSTA[ind]), sep = "#")
                        relation <- paste(relation, round(1.0* found / length(w), 2), sep = "#")
                        timeStamps <- paste(timeStamps, newTimeStamp, sep = "#")
                    }else{
                        direction <- paste(direction, as.character(newSTA[-ind]), sep = "#")
                        relation <- paste(relation, 1- round(1.0* found / length(w), 2), sep = "#")
                        timeStamps <- paste(timeStamps, newTimeStamp, sep = "#")
                    }

                    break()
                }
                if(n == length(tBTS)){
                    #print(paste("no match found", staNames[i], trNumbers[k]))
                  next()
                }
            }
        }
        if(length(direction) < 1){
          #result <- rbind(result, data.frame(SYS = sys[j], TIME = "00:00:00.00#00:00:00.00",
          #                                   DIRECTION = paste(newSTA[ind], newSTA[-ind], sep = "#"), RELATION = "0#0",
          #                                   stringsAsFactors = F))
        }else{
          ri <- unlist(strsplit(direction, "#"))[-1]
          time <- unlist(strsplit(timeStamps, "#"))[-1]
          if(length(ri) != length(time)){
            stop("ba")
            rep("Ri", length(time)- length(ri))
          }
          result <- rbind(result, data.frame(SYS = sys[j], TIME = timeStamps,
                                             DIRECTION = direction, RELATION = relation,
                                             stringsAsFactors = F))
        }
    }
    write.csv2(result, file = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/", staNames[i], ".csv"), row.names = F)
}


staFiles <- list.files(path = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang"), full.names = T, pattern = ".csv$")
staNames <- gsub(".csv", "", list.files(path = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang"), full.names = F, pattern = ".csv$"))

data <- read.csv2(file= helper.getResultPath(OUT_FAHRLAGEN_STAFIT_FILEPATH), stringsAsFactors = F)
sta <- read.csv2(file = helper.getResultPath(OUT_BTS2STA_FILEPATH), stringsAsFactors = F)


for(i in 1:length(staFiles)){
    helper.log(paste("split Ri and GRi and time buckets for STA", staNames[i]))
    tempFrame <- read.csv2(file= staFiles[i], stringsAsFactors = F)
    resultFrame <- data.frame()
    for(j in 1:length(tempFrame$SYS)){
        ri <- unlist(strsplit(tempFrame$DIRECTION[j], "#"))[-1]
        time <- unlist(strsplit(tempFrame$TIME[j], "#"))[-1]
        secondsDeparture <- integer(0)
        for(t in time){
            temp <- as.integer(unlist(strsplit(unlist(strsplit(t, ":")), "\\.")))
            secondsDeparture <- c(secondsDeparture,
                                  ifelse(temp[1] >= 100, 3600*(temp[1]-100+24), temp[1]*3600) + temp[2]*60 + temp[3])
        }
        resultFrame <- rbind(resultFrame, data.frame(SYS = tempFrame$SYS[j], DIRECTION = ri,
                                                     TIME = secondsDeparture, stringsAsFactors = F))

    }
    if(length(unique(resultFrame$DIRECTION)) < 2){
        resultFrame <- rbind(resultFrame, data.frame(SYS = unique(resultFrame$SYS)[grepl("G", unique(resultFrame$SYS))][1],
                                                     DIRECTION = c("Ri", "GRi")[c("Ri", "GRi") != unique(resultFrame$DIRECTION)],
                                                     TIME = 1000,
                                                     stringsAsFactors = F))
    }

    resultFrame$TIMESLOT30 <- cut(resultFrame$TIME, seq(0,172800, 1800), right=F)
    tmp <- paste(gsub(pattern = ",5", replacement = ":30", x = gsub("\\.", ",", c(as.character(seq(0,48,0.5)))) ))
    tmp[!grepl(":", tmp)] <- paste0(gsub(" ", "", tmp[!grepl(":", tmp)]), ":00")
    levels(resultFrame$TIMESLOT30) <- tmp

    resultFrame$TIMESLOT60 <- cut(resultFrame$TIME, seq(0,172800, 3600), right=F)
    levels(resultFrame$TIMESLOT60) <- paste0(c(as.character(seq(0,48,1))), ":00")

    resultFrame$TIMESLOT120 <- cut(resultFrame$TIME, seq(0,172800, 7200), right=F)
    levels(resultFrame$TIMESLOT120) <- paste0(c(as.character(seq(0,48,2))), ":00")

    resultFrame$TIMESLOT240 <- cut(resultFrame$TIME, seq(0,172800, 14400), right=F)
    levels(resultFrame$TIMESLOT240) <- paste0(c(as.character(seq(0,48,4))), ":00")

    resultFrame$DIRECTION <- factor(resultFrame$DIRECTION, levels = as.character(unique(resultFrame$DIRECTION)))
    resultFrame$SYS <- factor(resultFrame$SYS, levels = as.character(unique(resultFrame$SYS)))

    # helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/plot120/"))
    # helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/plot60/"))
    # helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/plot30/"))
    #
    # e <- ggplot(resultFrame, aes(x=TIMESLOT120, fill=SYS)) + ggtitle(paste("STA ", staNames[i])) + geom_bar() +
    #   theme_minimal() + scale_fill_brewer(palette="Set1") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #   scale_x_discrete(drop=FALSE) + facet_grid(DIRECTION~.)
    # ggsave(filename=paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/plot120/", "STA_", staNames[i], "_120.png"),
    #        plot=e, width = 19, height = 9)
    #
    # e <- ggplot(resultFrame, aes(x=TIMESLOT60, fill=SYS)) + ggtitle(paste("STA ", staNames[i])) + geom_bar() +
    #     theme_minimal() + scale_fill_brewer(palette="Set1") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     scale_x_discrete(drop=FALSE) + facet_grid(DIRECTION~.)
    # ggsave(filename=paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/plot60/", "STA_", staNames[i], "_60.png"),
    #        plot=e, width = 19, height = 9)
    #
    # e <- ggplot(resultFrame, aes(x=TIMESLOT30, fill=SYS)) + ggtitle(paste("STA ", staNames[i])) + geom_bar() +
    #     theme_minimal() + scale_fill_brewer(palette="Set1") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    #     scale_x_discrete(drop=FALSE) + facet_grid(DIRECTION~.)
    # ggsave(filename=paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/plot30/", "STA_", staNames[i], "_30.png"),
    #        plot=e, width = 19, height = 9)

    #staBTS <- unique(sta$BTS[sta$ID==staNames[i]])
    #fBTS <- staBTS[1]
    #lBTS <- staBTS[length(staBTS)]
    helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables60/"))
    helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables120/"))
    helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables240/"))

    lv <- unique(levels(resultFrame$DIRECTION))

    write.csv2(table(resultFrame$TIMESLOT60[resultFrame$DIRECTION == lv[1]],
                     resultFrame$SYS[resultFrame$DIRECTION == lv[1]]),
               file = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables60/", "STA_", staNames[i], "_NEWSTA_", lv[1], ".csv"),
               row.names = T)

    write.csv2(table(resultFrame$TIMESLOT60[resultFrame$DIRECTION == lv[2]],
                     resultFrame$SYS[resultFrame$DIRECTION == lv[2]]),
               file = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables60/", "STA_", staNames[i], "_NEWSTA_", lv[2], ".csv"),
               row.names = T)

    write.csv2(table(resultFrame$TIMESLOT120[resultFrame$DIRECTION == lv[1]],
                     resultFrame$SYS[resultFrame$DIRECTION == lv[1]]),
               file = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables120/", "STA_", staNames[i], "_NEWSTA_", lv[1], ".csv"),
               row.names = T)

    write.csv2(table(resultFrame$TIMESLOT120[resultFrame$DIRECTION == lv[2]],
                     resultFrame$SYS[resultFrame$DIRECTION == lv[2]]),
               file = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables120/", "STA_", staNames[i], "_NEWSTA_", lv[2], ".csv"),
               row.names = T)

    write.csv2(table(resultFrame$TIMESLOT240[resultFrame$DIRECTION == lv[1]],
                     resultFrame$SYS[resultFrame$DIRECTION == lv[1]]),
               file = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables240/", "STA_", staNames[i], "_NEWSTA_", lv[1], ".csv"),
               row.names = T)

    write.csv2(table(resultFrame$TIMESLOT240[resultFrame$DIRECTION == lv[2]],
                     resultFrame$SYS[resultFrame$DIRECTION == lv[2]]),
               file = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/tables240/", "STA_", staNames[i], "_NEWSTA_", lv[2], ".csv"),
               row.names = T)
}



helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/FILLTABLE60/"))
helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/FILLTABLE120/"))
helper.safeCreateFolder(paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/FILLTABLE240/"))

for(e in 1:3){
  nm <- c("FILLTABLE60/", "FILLTABLE120/", "FILLTABLE240/")
  tb <- c("tables60/", "tables120/", "tables240/")
  staFiles <- list.files(path = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/", tb[e]), full.names = T, pattern = ".csv$")
  staNames <- gsub(".csv", "", list.files(path = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/", tb[e]), full.names = F, pattern = ".csv$"))

  weightSTA <- integer(length(staFiles))
  for(i in 1:length(staFiles)){
    tempFrame <- read.csv2(file = staFiles[i], stringsAsFactors = F)[,-1]
    #get the total number of systemtrassen for weight of STA
    if(is.null(dim(tempFrame))){
      weightSTA[i] <- quantile(tempFrame, 0.75)+e
    }else{
      weightSTA[i] <- quantile(apply(tempFrame, 1, sum), 0.75)+e
    }

  }


  for(i in 1:length(staFiles)){
    #print(staNames[i])
    tempFrame <- read.csv2(file = staFiles[i], stringsAsFactors = F)
    sysTrains <- colnames(tempFrame)[-1]
    # x trains per 2 hours from weightSTA
    totalTrains <- weightSTA[i]
    remainingTrains <- rep(totalTrains, length(tempFrame$X))
    if(length(sysTrains) == 3){
      #split number of trains per hour to different systemtrassen
      remainingTrains <- remainingTrains - tempFrame[,4]
    }
    f <- 2
    firstCharacteristic <- remainingTrains

    if(length(sysTrains) >= 2){
      if(sum(tempFrame[,2]) > sum(tempFrame[,3])){
        f <- 2
        s <- 3
      }else{
        f <- 3
        s <- 2
      }
      firstCharacteristic <- tempFrame[,f]
      secondCharacteristic <- tempFrame[,s]

      # increase second characteristic by 1 if next time slot hast at least 1 second characteristic train
      for(j in 1:(length(secondCharacteristic)-1)){
        if(secondCharacteristic[j+1] > 0){secondCharacteristic[j] <- secondCharacteristic[j]+1}
      }

      firstCharacteristic <- remainingTrains - secondCharacteristic
      firstCharacteristic[firstCharacteristic < 0] <- 0
    }

    resultFrame <- tempFrame
    resultFrame[,f] <- firstCharacteristic
    if(length(sysTrains) >= 2){
      resultFrame[,s] <- secondCharacteristic
    }

    write.csv2(resultFrame, file = paste0(helper.getResultPath(OPT_FOLDER), "/tagesgang/", nm[e], staNames[i],".csv"), row.names = F)
  }
}




############################## end file ########################################

# files <- list.files(path = "./bottomup/merge_a(v)_v2/assignment/", full.names = T, pattern = ".csv$")
# fileNames <- list.files(path = "./bottomup/merge_a(v)_v2/assignment/", full.names = F, pattern = ".csv$")
#
# fahrlagen <- read.csv2(file = "./2013_Fahrlagen/Fahrlagen_14.11.2013_final_v02.csv", stringsAsFactors = F)
#
#
# for(i in 1:length(files)){
#     print(i)
#     tempFrame <- read.csv2(file = files[i], stringsAsFactors = F)
#
#     laufpunkte <- fahrlagen[which(fahrlagen$ANFORDERUNGNAME %in% tempFrame$TRAINRUN),c("STARTZEIT", "ZIELZEIT", "WAY", "DEP")]
#     laufpunkte$T_FIRST <- -1
#     laufpunkte$T_BTS <- -1
#
#     for(k in 1:length(laufpunkte$STARTZEIT)){
#         bts <- unlist(strsplit(laufpunkte$WAY[k], "#"))
#         ind <- which(bts %in% gsub(".csv", "", fileNames[i]))[1]
#
#         dep <- unlist(strsplit(laufpunkte$DEP[k], "#"))
#         d <- dep[ind]
#         f <- dep[1]
#
#         while(d == "NA"){
#             ind <- ind - 1
#             if(ind < 1){break()}
#             d <- dep[ind]
#         }
#
#         while(d == "NA"){
#             ind <- ind + 1
#             if(ind > length(dep)){stop(paste(i, k, "all stations are NA"))}
#             d <- dep[ind]
#         }
#
#         btsTime <- as.integer(unlist(strsplit(unlist(strsplit(d, ":")), "\\.")))
#         firstTime <- as.integer(unlist(strsplit(unlist(strsplit(f, ":")), "\\.")))
#
#         t1 <- ifelse(btsTime[1] >= 100, 3600*(btsTime[1]-100+24), btsTime[1]*3600)
#         t2 <- btsTime[2]*60
#         laufpunkte$T_BTS[k] <- t1+t2+btsTime[3]
#
#         t1 <- ifelse(firstTime[1] >= 100, 3600*(firstTime[1]-100+24), firstTime[1]*3600)
#         t2 <- firstTime[2]*60
#         laufpunkte$T_FIRST[k] <- t1+t2+firstTime[3]
#
#         if(laufpunkte$T_BTS[k] < laufpunkte$T_FIRST[k]){
#             laufpunkte$T_BTS[k] <- laufpunkte$T_BTS[k] + 24*3600
#         }
#     }
#     tempFrame$T_FIRST <- laufpunkte$T_FIRST
#     tempFrame$T_BTS <- laufpunkte$T_BTS
#     tempFrame$CORRECTED_BTS <- floor(tempFrame$T_FIRST + 0.8 * (tempFrame$T_BTS - tempFrame$T_FIRST))
#     tempFrame$TIMESLOT60 <- cut(tempFrame$CORRECTED_BTS, seq(0,172800, 3600), right=F)
#     levels(tempFrame$TIMESLOT60) <- paste(c(as.character(seq(0,48,1))), "Uhr")
#     tempFrame$TIMESLOT30 <- cut(tempFrame$CORRECTED_BTS, seq(0,172800, 1800), right=F)
#     levels(tempFrame$TIMESLOT30) <- paste(gsub(pattern = ".5", replacement = ":30", x = c(as.character(seq(0,48,0.5)))), "Uhr")
#
#     write.csv2(tempFrame, file = paste0("./bottomup/merge_a(v)_v2/tagesgang/", fileNames[i]), row.names = F)
#     e <- qplot(x = tempFrame$TIMESLOT60, fill = tempFrame$SYS_ID) + ggtitle(fileNames[i]) +
#         theme_minimal() + scale_fill_brewer(palette="Set1") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#     ggsave(filename=paste0("./bottomup/merge_a(v)_v2/tagesgang/plot60/", fileNames[i], "_60.png"), plot=e, width = 19, height = 9)
#     e <- qplot(x = tempFrame$TIMESLOT30, fill = tempFrame$SYS_ID) + ggtitle(fileNames[i]) +
#         theme_minimal() + scale_fill_brewer(palette="Set1") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
#     ggsave(filename=paste0("./bottomup/merge_a(v)_v2/tagesgang/plot30/", fileNames[i], "_30.png"), plot=e, width = 19, height = 9)
# }
# }
