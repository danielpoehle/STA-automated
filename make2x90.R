#source("./T10kmCalculator.R")
#source("./a-v-calculations.R")

fullFrame <- read.csv2(file = "~/Dokumente/STA-ZugChar-Generator/result_detail_v117/dtSTA.csv", stringsAsFactors = F)
#fullFrame <- fullFrame[fullFrame$T10WithI <=1000,]
#dt <- read.csv2(file = "~/Dokumente/STA-ZugChar-Generator/result_detail_v999/TFZ_Frame_for_a_frames.csv", stringsAsFactors = F)
dt <- (fullFrame[!duplicated(fullFrame[,c("TOTALWEIGHT", "TFZ", "NUM_TFZ")]), ])

#sta90Files <- list.files("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/all90", full.names = T)
staGroups <- read.csv2(file = "~/Dokumente/STA-ZugChar-Generator/result_detail_v117/STAGROUPS_v11.csv", stringsAsFactors = F)
staGroups$PARTNER[is.na(staGroups$PARTNER)] <- ""
#fNames <- list.files("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/all90", full.names = F)
aFrameFiles <- list.files("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/a_frame", full.names = T)
tempFrameFiles <- list.files("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/STAs", full.names = T)
zch3x90 <- c("195A", "065")

cl <- makeCluster(NUMBER_OF_CORES)
registerDoParallel(cl)

find2x90 <- function(tempFrame, aFrame, find3x90 = F){
  resultFrame <- data.frame()

  t10 <- 7200

  z <- 0.3
  w <- 0.9

  idx <- which(rowSums(aFrame) >= z*ncol(aFrame) & rowSums(aFrame) <= w*ncol(aFrame))
  if(length(idx) < 1){
    w <- 1
    idx <- which(rowSums(aFrame) >= z*ncol(aFrame) & rowSums(aFrame) <= w*ncol(aFrame))
  }
  idx1 <- integer(0)
  for(j in 1:length(idx)){
    first <- fullFrame[fullFrame$TFZ == dt$TFZ[idx[j]] & fullFrame$TOTALWEIGHT == dt$TOTALWEIGHT[idx[j]] &
                         fullFrame$VMAX >= 40 & fullFrame$I == staGroups$i[i],]
    maxT <- 1800
    if(find3x90){ maxT <- 1000}
    if(all(first$T10WithI > min(t10, maxT))){ next() }
    idx1 <- c(idx1, idx[j])
  }
  if(length(idx1) < 1){
    write.csv2(resultFrame, file = paste0("~/Dokumente/STA-ZugChar-Generator/result_detail_v283/", staGroups$ID[i], ".csv"), row.names = F)
    next()
  }

  z <- 0.6
  w <- 1.0
  idx <- which(rowSums(aFrame) >= z*ncol(aFrame) & rowSums(aFrame) <= w*ncol(aFrame))
  idx2 <- integer(0)
  for(j in 1:length(idx)){
    first <- fullFrame[fullFrame$TFZ == dt$TFZ[idx[j]] & fullFrame$TOTALWEIGHT == dt$TOTALWEIGHT[idx[j]] &
                         fullFrame$VMAX >= 40 & fullFrame$I == staGroups$i[i],]
    #print(paste(first$TFZ[1], first$TOTALWEIGHT[1], min(first$T10WithI)))
    if(all(first$T10WithI > maxT)){ next() }
    idx2 <- c(idx2, idx[j])
  }
  if(length(idx2) < 1){
    write.csv2(resultFrame, file = paste0("~/Dokumente/STA-ZugChar-Generator/result_detail_v283/", staGroups$ID[i], ".csv"), row.names = F)
    next()
  }


  bestRatio = 0.99
  if(t10 > 20000){ t10 <- 7200 }
  for(j in 1:length(idx1)){
    timestamp()
    print(paste("task", i, j, "best ratio:", bestRatio, "candidates:", length(resultFrame$INDEX_DT_1)))
    first <- fullFrame[fullFrame$TFZ == dt$TFZ[idx1[j]] & fullFrame$TOTALWEIGHT == dt$TOTALWEIGHT[idx1[j]] &
                         fullFrame$VMAX >= 40 & fullFrame$I == staGroups$i[i],]
    first <- first[first$T10WithI <= 1800,]

    if(min(first$T10WithI) > bestRatio * t10){ next() }
    for(k in 1:length(idx2)){
      if(idx1[j] == idx2[k]){ next() }
      #print(paste("task", i, j, k, "best ratio:", bestRatio, "candidates:", length(resultFrame$INDEX_DT_1)))
      #build first aframe
      a1 <- aFrame[idx1[j],]
      #bulid second aframe
      a2 <- aFrame[idx2[k],]
      border <- 0.9
      if(find3x90){
        border <- 0.65
      }

      if(sum(as.numeric(a1 | a2)) < border*ncol(aFrame)){
        #print(paste(j, k, idx1[j], idx2[k], "less than 90% (", sum(as.numeric(a1 | a2)), "/", 0.9*nrow(tempFrame), ")"))
        next()
      }

      second <- fullFrame[fullFrame$TFZ == dt$TFZ[idx2[k]] & fullFrame$TOTALWEIGHT == dt$TOTALWEIGHT[idx2[k]] &
                            fullFrame$VMAX >= 40 & fullFrame$I == staGroups$i[i],]
      second <- second[second$T10WithI <= 1800,]
      y <- a2 - a1
      y[y<0] <- 0

      if((rowSums(a1) * min(first$T10WithI) + rowSums(y) * min(second$T10WithI)) / (rowSums(a1) + rowSums(y)) > bestRatio * t10) {next()}


      mat_first <- matrix(data = 0, nrow = length(first$TFZ), ncol = ncol(a1))
      mat_second <- matrix(data = 0, nrow = length(second$TFZ), ncol = ncol(a2))

      for(n in 1:length(first$TFZ)){
        if(first$BREAKCLASS[n] != "G"){
          mat_first[n, ] <- as.numeric(a1 * as.integer(tempFrame$VMAX >= first$VMAX[n]) * as.integer(tempFrame$BREAKCLASS != "G"))
        }else{
          mat_first[n, ] <- as.numeric(a1 * as.integer(tempFrame$VMAX >= first$VMAX[n]))
        }
      }
      for(n in 1:length(second$TFZ)){
        if(second$BREAKCLASS[n] != "G"){
          mat_second[n, ] <- as.numeric(a2 * as.integer(tempFrame$VMAX >= second$VMAX[n]) * as.integer(tempFrame$BREAKCLASS != "G"))
        }else{
          mat_second[n, ] <- as.numeric(a2 * as.integer(tempFrame$VMAX >= second$VMAX[n]))
        }
      }

      for(m in 1:length(first$TFZ)){
        for(n in 1:length(second$TFZ)){
          if(sum(mat_first[m,] | mat_second[n,]) >= border*ncol(aFrame)){
            y <- mat_second[n,] - mat_first[m,]
            y[y < 0] <- 0

            f1 <- fullFrame[fullFrame$TFZ == first$TFZ[m] & fullFrame$TOTALWEIGHT == first$TOTALWEIGHT[m] &
                              fullFrame$BREAKCLASS == first$BREAKCLASS[m] & fullFrame$VMAX == first$VMAX[m] &
                              fullFrame$NUM_TFZ == first$NUM_TFZ[m] & fullFrame$I == staGroups$i[i],]
            f2 <- fullFrame[fullFrame$TFZ == second$TFZ[n] & fullFrame$TOTALWEIGHT == second$TOTALWEIGHT[n] &
                              fullFrame$BREAKCLASS == second$BREAKCLASS[n] & fullFrame$VMAX == second$VMAX[n] &
                              fullFrame$NUM_TFZ == second$NUM_TFZ[n] & fullFrame$I == staGroups$i[i],]
            avg <- mat_first[m, ] * f1$T10WithI + y * f2$T10WithI
            if(mean(avg[avg>0]) < min(1.005 * bestRatio*t10, t10)){
              r <- round(1.0* mean(avg[avg>0]) / t10, 3)
              if(r < bestRatio){
                bestRatio <- r
              }
              #print(paste(j, k, idx1[j], idx2[k], "success first train", round(100.0* mean(avg[avg>0]) / t10, 1), "%"))
              resultFrame <- rbind(resultFrame, data.frame(INDEX_DT_1 = as.integer(row.names(f1)), VMAX1 = f1$VMAX,
                                                           INDEX_DT_2 = as.integer(row.names(f2)), VMAX2 = f2$VMAX,
                                                           WEIGHTED_T10WITHI = mean(avg[avg>0]),
                                                           ANZ1 = sum(mat_first[m,]), ANZ2 = sum(y),
                                                           I = staGroups$i[i], stringsAsFactors = F))
            }else{
              #print(paste(j, k, idx1[j], idx2[k], "no success", round(100.0* mean(avg[avg>0]) / t10, 1), "%"))
            }
          }
        }
      }
    }
  }
  return(resultFrame)
}

#foreach(i = c(3,7,10,13,14,31,32,86,87,102,104,106,111,56,118,143,148,149,154,99,40,100,38,47,116,77,54,19,81)) %dopar% {
#foreach(i = 1:length(staGroups$ID)) %dopar% {
#for(i in 1:length(staGroups$ID)){
for(i in c(65,90,91)){
  print(i)
  tempFrame <- read.csv2(file = paste0("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/STAs/STA_", staGroups$ID[i], ".csv"), stringsAsFactors = F)
  if(staGroups$PARTNER[i] != ""){
    fi <- paste0("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/STAs/STA_",
                 staGroups$ID[staGroups$PARTNER == staGroups$PARTNER[i] & staGroups$ID != staGroups$ID[i]], ".csv")
    for(f in fi){
      tempFrame <- rbind(tempFrame, read.csv2(file = f, stringsAsFactors = F))
    }
  }
  aFrame <- read.csv2(paste0("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/a_frame/", staGroups$ID[i], ".csv"), stringsAsFactors = F)[,-1]
  if(length(tempFrame$X) != ncol(aFrame)){ stop("aFrame not suitable to tempFrame") }
  if(nrow(dt) != nrow(aFrame)){ stop("aFrame not suitable to dt") }

  resultFrame <- find2x90(tempFrame, aFrame, find3x90 = staGroups$ID[i] %in% zch3x90)

  write.csv2(resultFrame, file = paste0("~/Dokumente/STA-ZugChar-Generator/result_detail_v283/", staGroups$ID[i], ".csv"), row.names = F)
}

stopCluster(cl)
#write.csv2(resultFrame, file = "./result_detail_v280/101.csv", row.names = F)
#summary(resultFrame$WEIGHTED_T10WITHI)


######################### STOP #########################

## update T10km weighted for STA-ZugChar-Generator files
twoX90Files <- list.files("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/STA-ZugChar-Generator", full.names = T)
i1 <- -1
i2 <- -1
t10 <- -10

for(i in 1:length(twoX90Files)){
  print(twoX90Files[i])
  mtry <- try(read.csv2(file = twoX90Files[i], stringsAsFactors = F), silent = TRUE)
  if (class(mtry) == "try-error") { next() }
  tempFrame <- read.csv2(twoX90Files[i], stringsAsFactors = F)
  #for(j in 1:6){
  for(j in 1:length(tempFrame$INDEX_DT_1)){
    if(i1 == tempFrame$INDEX_DT_1[j] && i2 == tempFrame$INDEX_DT_2[j]){
      tempFrame[j,"WEIGHTED_T10WITHI"] <- round(t10, 1)
    }else{
      i1 <- tempFrame$INDEX_DT_1[j]
      i2 <- tempFrame$INDEX_DT_2[j]
      t10 <- 1.0 * (tempFrame$ANZ1[j] * fullFrame$T10WithI[i1] + tempFrame$ANZ2[j] * fullFrame$T10WithI[i2]) / (tempFrame$ANZ1[j] + tempFrame$ANZ2[j])
      tempFrame[j,"WEIGHTED_T10WITHI"] <- round(t10, 1)
    }
  }
  print(paste0("min for i=", i, ": ", min(tempFrame$WEIGHTED_T10WITHI), " sec"))
  write.csv2(tempFrame, file = twoX90Files[i], row.names = F)
}

## update T10km weighted for all90 files
sta90Files <- list.files("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/all90", full.names = T)

for(i in 1:length(sta90Files)){
  print(sta90Files[i])
  mtry <- try(read.csv2(file = sta90Files[i], stringsAsFactors = F), silent = TRUE)
  if (class(mtry) == "try-error") { next() }
  tempFrame <- read.csv2(sta90Files[i], stringsAsFactors = F)
  #for(j in 1:6){
  for(j in 1:length(tempFrame$a)){
    #print(j)
    v <- tempFrame$v[j]
    ind <- which(fullFrame$TFZ == tempFrame$TFZ[j] & fullFrame$NUM_TFZ == tempFrame$NUM_TFZ[j] &
                   fullFrame$TOTALWEIGHT == tempFrame$TOTALWEIGHT[j] &
                   fullFrame$BREAKCLASS == tempFrame$BREAKCLASS[j] & fullFrame$VMAX == v)
    while(length(ind) < 1){
      v <- v - 10
      ind <- which(fullFrame$TFZ == tempFrame$TFZ[j] & fullFrame$NUM_TFZ == tempFrame$NUM_TFZ[j] &
                     fullFrame$TOTALWEIGHT == tempFrame$TOTALWEIGHT[j] &
                     fullFrame$BREAKCLASS == tempFrame$BREAKCLASS[j] & fullFrame$VMAX == v)
      if(v < 0){ v <- 110}
    }
    tempFrame[j, "T10"] <- round(fullFrame$T10WithI[ind], 4)
  }
  print(paste0("min for i=", i, ": ", min(tempFrame$T10), " sec"))
  write.csv2(tempFrame, file = sta90Files[i], row.names = F)
}


########## make STA-ZugChar-Generator files readable #################

files <- list.files(path = "~/Dokumente/STA-ZugChar-Generator/result_detail_v117/STA-ZugChar-Generator", full.names = T)
fName <- list.files(path = "~/Dokumente/STA-ZugChar-Generator/result_detail_v117/STA-ZugChar-Generator", full.names = F)
for(i in 1:length(files)){
  print(files[i])
  tmp <- read.csv2(files[i], stringsAsFactors = F)
  tmp$Z1 <- ""
  tmp$Z2 <- ""
  for(j in 1:length(tmp$INDEX_DT_1)){
    ind <- tmp$INDEX_DT_1[j]
    tmp[j, "Z1"] <- paste(fullFrame$TFZ[ind], fullFrame$TOTALWEIGHT[ind], fullFrame$VMAX[ind], fullFrame$BREAKCLASS[ind], sep = "#")
    ind <- tmp$INDEX_DT_2[j]
    tmp[j, "Z2"] <- paste(fullFrame$TFZ[ind], fullFrame$TOTALWEIGHT[ind], fullFrame$VMAX[ind], fullFrame$BREAKCLASS[ind], sep = "#")
  }
  write.csv2(tmp, file = paste0("~/Dokumente/STA-ZugChar-Generator/result_detail_v117/filled_STA-ZugChar-Generator/", fName[i]), row.names = F)
}

########## print a-v diagrams ###################

elem <- tfzNames[tfzNames$name == "185-2", ]
avModel <- getAVModel(elem$i, elem$j, 1300, 1, addTfzMass = T)
avModel2 <- avModel
reduce <- getReduction(avModel$a[1], avModel$a[91])
avModel2$a <- avModel2$a - (seq(0.1,1,0.009)* reduce)[1:91]
avModel2$tfz <- "tolerance"
avModel <- rbind(avModel, avModel2)
elem <- tfzNames[tfzNames$name == "294-3", ]
avModel <- rbind(avModel, getAVModel(elem$i, elem$j, 950, 1, addTfzMass = T))
elem <- tfzNames[tfzNames$name == "140-1", ]
avModel <- rbind(avModel, getAVModel(elem$i, elem$j, 2000, 1, addTfzMass = T))
qplot(avModel$v, avModel$a, color = avModel$tfz)
qplot(avModel$s, -avModel$t_kum, color = avModel$tfz)


calculate10kmWithI(avModel, 100, "P", 20)
