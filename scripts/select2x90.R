#source("a-v-calculations.R")
#source("T10kmCalculator.R")

## calculate T10km for all FLG in STA files
staFiles <- list.files(helper.getResultPath(STA_RESULT_FOLDER), full.names = T)

helper.log("start calc T10km for STAs")

#foreach(i = 1:length(staFiles)) %dopar% {
for(i in 1:length(staFiles)){
  #print(staFiles[i])
  mtry <- try(read.csv2(file = staFiles[i], stringsAsFactors = F), silent = TRUE)
  if (class(mtry) == "try-error") { next() }
  tempFrame <- read.csv2(staFiles[i], stringsAsFactors = F)
  if(length(tempFrame$X) < 1){ next() }
  #for(j in 1:6){
  for(j in 1:length(tempFrame$X)){
    elem <- tfzNames[tfzNames$name == tempFrame$TFZ[j], ]
    avModel = getAVModel(elem$i, elem$j, tempFrame$TOTALWEIGHT[j], tempFrame$NUM_TFZ[j], addTfzMass = F)
    tempFrame[j, "T10withI7"] <- round(0.5* calculate10kmWithI(avModel, min(100, tempFrame$VMAX[j]), tempFrame$BREAKCLASS[j], 7) +
                                        0.5* calculate10km(avModel, min(100, tempFrame$VMAX[j]), tempFrame$BREAKCLASS[j]), 1)
  }
  # print(paste0("min ", min(tempFrame$T10withI7), " sec, median ", median(tempFrame$T10withI7), " sec, max ",
  #              max(tempFrame$T10withI7), " sec"))
  write.csv2(tempFrame, file = staFiles[i], row.names = F)
}


helper.log("finished calc T10km for STAs")

fullFrame <- read.csv2(file = helper.getResultPath(OUT_TFZ_LIST_DTSTA), stringsAsFactors = F)
dt <- (fullFrame[!duplicated(fullFrame[,c("TOTALWEIGHT", "TFZ", "NUM_TFZ")]), ])

files <- list.files(path = helper.getResultPath(OUT_2x90_FOLDER), full.names = T, pattern = ".csv$")
fNames <- list.files(path = helper.getResultPath(OUT_2x90_FOLDER), full.names = F, pattern = ".csv$")
staGroups <- read.csv2(file = helper.getResultPath(STAGROUPS_FILEPATH), stringsAsFactors = F)
staGroups$PARTNER[is.na(staGroups$PARTNER)] <- ""

two90s <- data.frame()
for(i in 1:length(files)){
#for(i in 1:10){
  #print(paste(i, length(two90s$INDEX_DT_1)))
  mtry <- try(read.csv2(file = files[i], stringsAsFactors = F), silent = TRUE)
  if (class(mtry) == "try-error") { next() }
  tmp <- read.csv2(file = files[i], stringsAsFactors = F)
  tmp <- tmp[!duplicated(tmp[,c("INDEX_DT_1", "INDEX_DT_2")]),]
  tmp$STA <- unlist(strsplit(fNames[i], "\\."))[1]
  tmp$i <- staGroups$i[staGroups$ID == tmp$STA[1]]
  anz <- min(30, length(tmp$INDEX_DT_1))
  idx <- which(tmp$WEIGHTED_T10WITHI <= sort(tmp$WEIGHTED_T10WITHI)[anz])
  idx <- idx[1:anz]
  two90s <- rbind(two90s, tmp[idx,])
}

two90s$IND_RED_1 <- 0
two90s$IND_RED_2 <- 0
two90s$BC_1 <- "G"
two90s$BC_2 <- "G"
for(i in 1:length(two90s$INDEX_DT_1)){
  #print(i)
  two90s$IND_RED_1[i] <- which(dt$TFZ == fullFrame$TFZ[two90s$INDEX_DT_1[i]] & dt$NUM_TFZ == fullFrame$NUM_TFZ[two90s$INDEX_DT_1[i]] &
                               dt$TOTALWEIGHT ==fullFrame$TOTALWEIGHT[two90s$INDEX_DT_1[i]])
  two90s$BC_1[i] <- fullFrame$BREAKCLASS[two90s$INDEX_DT_1[i]]
  two90s$IND_RED_2[i] <- which(dt$TFZ == fullFrame$TFZ[two90s$INDEX_DT_2[i]] & dt$NUM_TFZ == fullFrame$NUM_TFZ[two90s$INDEX_DT_2[i]] &
                               dt$TOTALWEIGHT ==fullFrame$TOTALWEIGHT[two90s$INDEX_DT_2[i]])
  two90s$BC_2[i] <- fullFrame$BREAKCLASS[two90s$INDEX_DT_2[i]]
}

all2x90 <- paste(two90s$INDEX_DT_1, two90s$INDEX_DT_2, sep = "#")
all2x90 <- unique(all2x90)

# get i type of the trains from the STA group i type

iAll90 <- integer(length(all2x90))
for(j in 1:length(all2x90)){
  id1 <- unlist(strsplit(all2x90[j], "#"))
  id2 <- id1[2]
  id1 <- id1[1]
  iAll90[j] <- max(two90s$i[two90s$INDEX_DT_1 == id1 & two90s$INDEX_DT_2 == id2])
}

aFrameFiles <- list.files(helper.getResultPath(A_FRAME_RESULT_FOLDER), full.names = T)
tempFrameFiles <- list.files(helper.getResultPath(STA_RESULT_FOLDER), full.names = T)

# manipulate aFrame such that 15 permille STA can only use 15 permille ZCH
# and 10 permille STA can only use 10 and 15 permille ZCH
# and 9 permille STA can only use 9, 10 and 15 permille ZCH
x <- as.integer(unique(unlist(strsplit(all2x90[which(iAll90 > 7)], "#"))))
y <- integer(length(x))
for(i in 1:length(x)){
  y[i] <- which(dt$TOTALWEIGHT == fullFrame$TOTALWEIGHT[x[i]] & dt$TFZ == fullFrame$TFZ[x[i]] & dt$NUM_TFZ == fullFrame$NUM_TFZ[x[i]])
}

helper.safeCreateFolder(helper.getResultPath(OUT_MAN_AFRAME_FOLDER))
helper.log("correct aframes because of i_min of STA")
for(i in 1:length(staGroups$ID)){
  aFrame <- read.csv2(file = paste0(helper.getResultPath(A_FRAME_RESULT_FOLDER), "/", staGroups$ID[i], ".csv"), stringsAsFactors = F)
  #print(staGroups$ID[i])
  if(staGroups$i[i] > 7){
    ind <- as.integer(unique(unlist(strsplit(all2x90[which(iAll90 >= staGroups$i[i])], "#"))))
    dt_ind <- integer(0)
    for(j in 1:length(ind)){
      dt_ind <- c(dt_ind, y[ind[j] == x])
      dt_ind <- unique(dt_ind)
    }
    print(paste(staGroups$ID[i], staGroups$i[i], "permille: ", length(dt_ind), "remaining trains"))
    aFrame[-dt_ind,-1] <- matrix(0, nrow = (nrow(aFrame) - length(dt_ind)), ncol = (ncol(aFrame)-1))
  }
  write.csv2(aFrame, file = paste0(helper.getResultPath(OUT_MAN_AFRAME_FOLDER), "/", staGroups$ID[i], ".csv"), row.names = F)


}

cl <- makeCluster(NUMBER_OF_CORES)
registerDoParallel(cl)

helper.safeCreateFolder(helper.getResultPath(paste0(OUT_2x90_FOLDER, "/",OUT_PAIRFRAME_FOLDER)))
helper.safeCreateFolder(helper.getResultPath(paste0(OUT_2x90_FOLDER, "/",OUT_GAINFRAME_FOLDER)))

helper.log("calculate gain for each all2x90 train")
foreach(i = 1:length(staGroups$ID)) %dopar% {
#for(i in 1:length(staGroups$ID)){
  #timestamp()
  #print(paste0(i, ": STA_",staGroups$ID[i]))
  tempFrame <- read.csv2(file = paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_", staGroups$ID[i], ".csv"), stringsAsFactors = F)
  lng <- length(tempFrame$X) # length of this unique STA for res1 and y
  if(staGroups$PARTNER[i] != ""){
    fi <- paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_",
                 staGroups$ID[staGroups$PARTNER == staGroups$PARTNER[i] & staGroups$ID != staGroups$ID[i]], ".csv")
    for(f in fi){
      tempFrame <- rbind(tempFrame, read.csv2(file = f, stringsAsFactors = F))
    }
  }

  aFrame <- read.csv2(paste0(helper.getResultPath(OUT_MAN_AFRAME_FOLDER), "/", staGroups$ID[i], ".csv"), stringsAsFactors = F)[,-1]
  pairFrame <- integer(length(all2x90))
  totalgain <- integer(length(all2x90))
  border <- 0.9
  if(staGroups$ID[i] %in% zch3x90){
    border <- 0.65
  }
  for(j in 1:length(all2x90)){
    #print(j)
    id1 <- unlist(strsplit(all2x90[j], "#"))
    id2 <- id1[2]
    id1 <- id1[1]

    f1 <- fullFrame[id1,]
    f2 <- fullFrame[id2,]

    a1 <- aFrame[which(dt$TFZ == f1$TFZ & dt$TOTALWEIGHT == f1$TOTALWEIGHT & dt$NUM_TFZ == f1$NUM_TFZ),]
    a1 <- a1[which.max(rowSums(a1)),]
    if(rowSums(a1) < 1){ next() }
    v1 <- tempFrame$VMAX >= f1$VMAX
    if(f1$BREAKCLASS != "G"){
      res1 <- as.numeric(a1 * v1 * as.integer(tempFrame$BREAKCLASS != "G"))
    }else{
      res1 <- as.numeric(a1 * v1)
    }
    #only for f1 and STAs that do not have pairs --> addition List
    # if sum(a1) > sum(a1*v1) --> generate new f1 for better fit with reduced v1
    # if sum(a1*v1) > sum(res1) --> generate new f1 with breakclass G for better fit

    a2 <- aFrame[which(dt$TFZ == f2$TFZ & dt$TOTALWEIGHT == f2$TOTALWEIGHT & dt$NUM_TFZ == f2$NUM_TFZ),]
    a2 <- a2[which.max(rowSums(a2)),]
    if(rowSums(a2) < 1){ next() }
    v2 <- tempFrame$VMAX >= f2$VMAX
    if(f2$BREAKCLASS != "G"){
      res2 <- as.numeric(a2 * v2 * as.integer(tempFrame$BREAKCLASS != "G"))
    }else{
      res2 <- as.numeric(a2 * v2)
    }

    pairFrame[j] <- as.integer(sum(res1 | res2) >= border*ncol(aFrame))
    if(pairFrame[j] == 1){
      y <- res2 - res1
      y[y < 0] <- 0
      z <- 1+integer(length(y)) - res1 - y

      # shorten res1 and y by length of original tempFrame without partners
      res1 <- res1[1:lng]
      y <- y[1:lng]
      z <- z[1:lng]

      # calculate median BFQ of STA
      avg <- median(c(1.0 * f1$T10WithI / tempFrame$T10withI7[res1 == 1], 1.0 * f2$T10WithI / tempFrame$T10withI7[y == 1], z[z==1]))

      # gain>0 is good, gain <0 is bad
      if((3.0-avg) < 1.2){
        pairFrame[j] <- 0
      }else{
        totalgain[j] <- round(3.0 - avg, 3)
      }
    }
  }

  tf <- data.frame(pairFrame)
  colnames(tf) <- "pair"
  write.csv2(tf, file = paste0(helper.getResultPath(paste0(OUT_2x90_FOLDER, "/",OUT_PAIRFRAME_FOLDER)), "/", staGroups$ID[i], ".csv"), row.names = F)
  tf <- data.frame(totalgain)
  colnames(tf) <- "gain"
  write.csv2(tf, file = paste0(helper.getResultPath(paste0(OUT_2x90_FOLDER, "/",OUT_GAINFRAME_FOLDER)), "/", staGroups$ID[i], ".csv"), row.names = F)
}
helper.log("finished gain for each all2x90 train")

# put together all pairframes and totalgains
resultFrame <- data.frame(matrix(0,length(staGroups$ID), 1+length(all2x90)))
colnames(resultFrame)[1] <- "STA"
gainFrame <- data.frame(matrix(0,length(staGroups$ID), 1+length(all2x90)))
colnames(gainFrame)[1] <- "STA"
for(i in 1:length(staGroups$ID)){
  g <- read.csv2(file = paste0(helper.getResultPath(paste0(OUT_2x90_FOLDER, "/",OUT_GAINFRAME_FOLDER)), "/", staGroups$ID[i], ".csv"))
  gainFrame[i,1] <- staGroups$ID[i]
  gainFrame[i,2:(length(g$gain)+1)] <- g$gain
  p <- read.csv2(file = paste0(helper.getResultPath(paste0(OUT_2x90_FOLDER, "/",OUT_PAIRFRAME_FOLDER)), "/", staGroups$ID[i], ".csv"))
  resultFrame[i,1] <- staGroups$ID[i]
  resultFrame[i,2:(length(p$pair)+1)] <- p$pair
}

#head(gainFrame[,1:15])

write.csv2(resultFrame, file = helper.getResultPath(OUT_COMP_OPTI), row.names = F)
write.csv2(gainFrame, file = helper.getResultPath(OUT_GAIN_OPTI), row.names = F)


#rowSums(gainFrame[,-1])
#sum(colSums(gainFrame[,-1])>0)
t10 <- integer(length(gainFrame$STA))
for(i in 1:length(gainFrame$STA)){
  t10[i] <- mean(read.csv2(paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_", gainFrame$STA[i], ".csv"), stringsAsFactors = F)$T10withI7)
  helper.log(paste0("STA_", gainFrame$STA[i], ", gain: ",sum(gainFrame[i,-1] > 0), " not covered: ", sum(resultFrame[i,-1] == 0), " mean T10_90: ", t10[i]))
}

if(any((resultFrame[, -1] == 0)*gainFrame[,-1] != 0)){stop("resultFrame = 0 but gainFrame != 0")}


helper.log("start heuristic assignment of pairs to STA")
currentGainFrame <- gainFrame[,-1]
notCovered <- integer(nrow(currentGainFrame)) + 1
# calculate avg gain of all columns
sumplus <- 1.0 * colSums(currentGainFrame*(currentGainFrame > 0)) / colSums(currentGainFrame>0)

selectedList <- integer(0)
cover <- matrix(0, length(staGroups$ID), 30)
for(i in 1:30){
  #print(paste(i, "not covered", sum(notCovered)))
  if(sum(notCovered) <1){
    print("finished")
    break()}
  candidate <- which.max(sumplus)
  if(sumplus[candidate] <= 0){
    print("less than 0")
    break()
    }
  selectedList <- c(selectedList, candidate)
  co <- as.integer(currentGainFrame[,candidate] > 0)
  cover[,i] <- co
  notCovered <- notCovered - co
  notCovered[notCovered < 0] <- 0
  currentGainFrame[which(co == 1),] <- integer(ncol(currentGainFrame))
  ctr <- colSums(currentGainFrame>0)
  ctr[ctr < 1] <- 1
  if(i < 22){
    sumplus <- 1.0 * colSums(currentGainFrame*(currentGainFrame > 0)) / ctr
  }else{
    sumplus <- 1.0 * colSums(currentGainFrame*(currentGainFrame > 0)) #/ ctr
  }
  sumplus[sumplus <= 0] <- 0
  if(sum(is.nan(sumplus)) > 0){ stop("nan introduced")}
}

#selectedList <- c(selectedList, 387)

getBestSelected <- function(resultFrame, gainFrame, selectedList, all2x90){
  bestSelected <- integer(0)
  gainSum <- 0
  #apply(gainFrame[, (selectedList + 1)], 1, FUN=which.max)
  for(i in 1:length(resultFrame$STA)){
    if(sum(resultFrame[i, 1+ selectedList] == 1) > 0){
      tmp <- which.max(gainFrame[i, (selectedList + 1)][,resultFrame[i, 1+ selectedList] == 1])
      bestSelected[i] <- all2x90[selectedList][resultFrame[i, 1+ selectedList] == 1][tmp]
      gainSum <- gainSum + max(gainFrame[i, (selectedList + 1)][,resultFrame[i, 1+ selectedList] == 1])
    }else{
      bestSelected[i] <- "no"
    }
  }
  list(bestSelected, gainSum)
}

beginSelected <- data.frame(STA = resultFrame$STA,
                            SEL = getBestSelected(resultFrame, gainFrame, selectedList, all2x90)[[1]],
                            stringsAsFactors = F)

beginSelected$SEL[12] <- "11408#7509"

if(any(beginSelected$SEL == "no")){stop("at least one STA not covered!")}

write.csv2(beginSelected, file = helper.getResultPath(OUT_PRE_OPTI), row.names = F)
write.csv2(all2x90, file = helper.getResultPath(OUT_ALL2x90), row.names = F)
helper.log("finished heuristic assignment of pairs to STA")

safeSelected <- beginSelected
#beginSelected <- safeSelected

####### optimize selection here #######
helper.log("start optimization")
helper.log(paste("NUM of cores:", NUMBER_OF_CORES))
filteredCol <- integer(length(all2x90))
bestIndices <- integer(0)
for(i in 1:length(all2x90)){
  if(sum(filteredCol) >= length(all2x90)){break()}
  if(filteredCol[i] == 1){next()}
  pool <- i
  filteredCol[i] <- 1
  for(j in (i+1):length(all2x90)){
    if(filteredCol[j] == 1){next()}
    if(sum(resultFrame[,i+1] == resultFrame[,j+1]) == length(staGroups$ID)){
      pool <- c(pool, j)
      filteredCol[j] <- 1
    }
  }
  if(length(pool) < 2){
    bestIndices <- c(bestIndices, pool)
  }else{
    bestIndices <- c(bestIndices, pool[which.max(colSums(gainFrame[,pool+1]))])
  }
}

cur_sel <- as.integer(selectedList)
cur_gain <- getBestSelected(resultFrame, gainFrame, cur_sel, all2x90)[[2]]
currentSelected <- list()
currentGainValue <- list()
for(n in 1:NUMBER_OF_CORES){
  currentSelected[[n]] <- cur_sel
  currentGainValue[[n]] <-cur_gain
}

set.seed(28)
first <- sample(bestIndices, 100*(NUMBER_OF_CORES +2), replace = T)
second <- sample(bestIndices, 100*(NUMBER_OF_CORES +2), replace = T)
rem <- first!=second
first <- first[rem]
second <- second[rem]

cl <- makeCluster(NUMBER_OF_CORES)
registerDoParallel(cl)

helper.safeCreateFolder(helper.getResultPath(TMP_OPT_FOLDER))
foreach(n = 1:NUMBER_OF_CORES) %dopar% {
  strt <- 100*(n-1)+1
  end <- 100*n
  for(i in strt:end){
    timestamp()
    switchInd <- c(-1, -1)
    gain <- -1
    for(j in 1:(length(currentSelected[[n]])-1)){
      for(k in (j+1):length(currentSelected[[n]])){
        #print(paste(j,k))
        tempSelected <- currentSelected[[n]][-c(j,k)]
        tempSelected <- c(tempSelected, first[i], second[i])
        tempGain <- getBestSelected(resultFrame, gainFrame, tempSelected, all2x90)[[2]]
        if(tempGain > gain){
          switchInd <- c(j, k)
          gain <- tempGain
        }
      }
    }
    if(gain > currentGainValue[[n]]){
      print(paste("i:", i, "      gain increase:", currentGainValue[[n]], "-->", gain))
      currentSelected[[n]] <- currentSelected[[n]][-switchInd]
      currentSelected[[n]] <- c(currentSelected[[n]], first[i], second[i])
      currentGainValue[[n]] <- gain
      nm <- paste0("OP_", currentGainValue[[n]], "_", n, "_currentSelected")
      write.csv2(currentSelected[[n]], file = paste0(helper.getResultPath(TMP_OPT_FOLDER), "/", nm, ".csv"))
    }else{
      print(paste("i:", i, "      no gain increase:", currentGainValue[[n]], "<--", gain))
    }
  }
}

stopCluster(cl)

f <- list.files(helper.getResultPath(TMP_OPT_FOLDER), full.names = T)

#tempI <- 238
optimizedSelected <- read.csv2(f[length(f)], stringsAsFactors = F)$x
write.csv2(optimizedSelected, file = paste0(helper.getResultPath(""),"selectedList_optimized.csv"), row.names = F)

beginSelected <- data.frame(STA = resultFrame$STA,
                            SEL = getBestSelected(resultFrame, gainFrame, optimizedSelected, all2x90)[[1]],
                            stringsAsFactors = F)

beginSelected$SEL[12] <- "11408#7509"

if(any(beginSelected$SEL == "no")){stop("at least one STA not covered!")}
helper.log("finished optimization")
###### finish optimization #######

######## get third ZCH ############


for(i in 1:length(zch3x90)){
  iRow <- which(beginSelected$STA == zch3x90[i])
  zch_id <- as.integer(unlist(strsplit(beginSelected$SEL[iRow], "#")))
  f1 <- fullFrame[zch_id[1],]
  f2 <- fullFrame[zch_id[2],]


  aFrame <- read.csv2(paste0(helper.getResultPath(A_FRAME_RESULT_FOLDER), "/", zch3x90[i], ".csv"), stringsAsFactors = F)[,-1]
  a1 <- aFrame[which(dt$TFZ == f1$TFZ & dt$TOTALWEIGHT == f1$TOTALWEIGHT & dt$NUM_TFZ == f1$NUM_TFZ),][1,]
  a2 <- aFrame[which(dt$TFZ == f2$TFZ & dt$TOTALWEIGHT == f2$TOTALWEIGHT & dt$NUM_TFZ == f2$NUM_TFZ),][1,]

  v1 <- tempFrame$VMAX >= f1$VMAX
  res1 <- integer(0)
  if(f1$BREAKCLASS != "G"){
    res1 <- as.numeric(a1 * v1 * as.integer(tempFrame$BREAKCLASS != "G"))
  }else{
    res1 <- as.numeric(a1 * v1)
  }

  v2 <- tempFrame$VMAX >= f2$VMAX
  res2 <- integer(0)
  if(f2$BREAKCLASS != "G"){
    res2 <- as.numeric(a2 * v2 * as.integer(tempFrame$BREAKCLASS != "G"))
  }else{
    res2 <- as.numeric(a2 * v2)
  }

  availableTrains <- integer(0)
  for(j in 1:length(optimizedSelected)){
    f3 <- fullFrame[optimizedSelected[j],]
    a3 <- aFrame[which(dt$TFZ == f3$TFZ & dt$TOTALWEIGHT == f3$TOTALWEIGHT & dt$NUM_TFZ == f3$NUM_TFZ),][1,]
    v3 <- tempFrame$VMAX >= f3$VMAX
    res3 <- integer(0)
    if(f3$BREAKCLASS != "G"){
      res3 <- as.numeric(a3 * v3 * as.integer(tempFrame$BREAKCLASS != "G"))
    }else{
      res3 <- as.numeric(a3 * v3)
    }
    if(sum(res1 | res2 | res3) >= 0.9*ncol(aFrame)){
      availableTrains <- c(availableTrains, optimizedSelected[j])
    }
  }
  if(length(availableTrains) < 1){stop("no third ZCH available!")}
  id <- availableTrains[which.min(fullFrame[availableTrains,"T10WithI"])]
  beginSelected$SEL[iRow] <- paste(beginSelected$SEL[iRow], id, sep = "#")
}

calcT10SYS <- F
helper.safeCreateFolder(helper.getResultPath(OPT_FOLDER))
helper.log("generate optimized_selected")
for(i in 1:length(staGroups$ID)){
  #timestamp()
  #print(paste0(i, ": STA_",staGroups$ID[i]))
  tempFrame <- read.csv2(file = paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_", staGroups$ID[i], ".csv"), stringsAsFactors = F)
  if(staGroups$PARTNER[i] != ""){
    fi <- paste0(helper.getResultPath(STA_RESULT_FOLDER), "/STA_",
                 staGroups$ID[staGroups$PARTNER == staGroups$PARTNER[i] & staGroups$ID != staGroups$ID[i]], ".csv")
    for(f in fi){
      tempFrame <- rbind(tempFrame, read.csv2(file = f, stringsAsFactors = F))
    }
  }
  idx <- which(beginSelected$STA == staGroups$ID[i])
  zch_id <- as.integer(unlist(strsplit(beginSelected$SEL[idx], "#")))
  f1 <- fullFrame[zch_id[1],]
  f2 <- fullFrame[zch_id[2],]


  aFrame <- read.csv2(paste0(helper.getResultPath(A_FRAME_RESULT_FOLDER), "/", staGroups$ID[i], ".csv"), stringsAsFactors = F)[,-1]
  a1 <- aFrame[which(dt$TFZ == f1$TFZ & dt$TOTALWEIGHT == f1$TOTALWEIGHT & dt$NUM_TFZ == f1$NUM_TFZ),][1,]
  a2 <- aFrame[which(dt$TFZ == f2$TFZ & dt$TOTALWEIGHT == f2$TOTALWEIGHT & dt$NUM_TFZ == f2$NUM_TFZ),][1,]

  v1 <- tempFrame$VMAX >= f1$VMAX
  res1 <- integer(0)
  if(f1$BREAKCLASS != "G"){
    res1 <- as.numeric(a1 * v1 * as.integer(tempFrame$BREAKCLASS != "G"))
  }else{
    res1 <- as.numeric(a1 * v1)
  }

  v2 <- tempFrame$VMAX >= f2$VMAX
  res2 <- integer(0)
  if(f2$BREAKCLASS != "G"){
    res2 <- as.numeric(a2 * v2 * as.integer(tempFrame$BREAKCLASS != "G"))
  }else{
    res2 <- as.numeric(a2 * v2)
  }

  if(staGroups$ID[i] %in% zch3x90){
    f3 <- fullFrame[zch_id[3],]
    a3 <- aFrame[which(dt$TFZ == f3$TFZ & dt$TOTALWEIGHT == f3$TOTALWEIGHT & dt$NUM_TFZ == f3$NUM_TFZ),][1,]
    v3 <- tempFrame$VMAX >= f3$VMAX
    res3 <- integer(0)
    if(f3$BREAKCLASS != "G"){
      res3 <- as.numeric(a3 * v3 * as.integer(tempFrame$BREAKCLASS != "G"))
    }else{
      res3 <- as.numeric(a3 * v3)
    }
  }

  # res1 is selection for f1, y is selection for f2, z is selection for f3
  y <- res2 - res1
  y[y < 0] <- 0
  z <- 0
  if(staGroups$ID[i] %in% zch3x90){
    z <- res3 - res2 - res1
    z[z < 0] <- 0
  }

  tempFrame$NAME <- "no"
  tempFrame$NAME[res1 == 1] <- zch_id[1]
  tempFrame$NAME[y == 1] <- zch_id[2]
  tempFrame$NAME[z == 1] <- zch_id[3]

  f1$BrH <- min(tempFrame$BrH[res1 == 1])
  f2$BrH <- min(tempFrame$BrH[y == 1])
  f1$LZB <- "FALSE"
  f2$LZB <- "FALSE"
  if(staGroups$ID[i] %in% zch3x90){
    f3$BrH <- min(tempFrame$BrH[z == 1])
    f3$LZB <- "FALSE"
  }

  if(sum(tempFrame$LZB[(res1+y+z) == 1] == "TRUE") >= ceiling(0.9*nrow(tempFrame))){
    f1$LZB <- "TRUE"
    f2$LZB <- "TRUE"
    if(staGroups$ID[i] %in% zch3x90){
      f3$LZB <- "TRUE"
    }
  }else{
    if(sum(tempFrame$LZB[(res1) == 1] == "TRUE") == sum(res1)){
      f1$LZB <- "TRUE"
    }
    if(sum(tempFrame$LZB[(y) == 1] == "TRUE") == sum(y)){
      f2$LZB <- "TRUE"
    }
    if(staGroups$ID[i] %in% zch3x90 && sum(tempFrame$LZB[(z) == 1] == "TRUE") == sum(z)){
      f3$LZB <- "TRUE"
    }
  }

  beginSelected$ID_F1[idx] <- zch_id[1]
  beginSelected$TZF_F1[idx] <- f1$TFZ
  beginSelected$NUM_TFZ_F1[idx] <- f1$NUM_TFZ
  beginSelected$TOTALWEIGHT_F1[idx] <- f1$TOTALWEIGHT
  beginSelected$VMAX_F1[idx] <- f1$VMAX
  beginSelected$BREAKCLASS_F1[idx] <- f1$BREAKCLASS
  beginSelected$BrH_F1[idx] <- f1$BrH
  beginSelected$LZB_F1[idx] <- f1$LZB
  beginSelected$T10WithI_F1[idx] <- f1$T10WithI

  beginSelected$ID_F2[idx] <- zch_id[2]
  beginSelected$TZF_F2[idx] <- f2$TFZ
  beginSelected$NUM_TFZ_F2[idx] <- f2$NUM_TFZ
  beginSelected$TOTALWEIGHT_F2[idx] <- f2$TOTALWEIGHT
  beginSelected$VMAX_F2[idx] <- f2$VMAX
  beginSelected$BREAKCLASS_F2[idx] <- f2$BREAKCLASS
  beginSelected$BrH_F2[idx] <- f2$BrH
  beginSelected$LZB_F2[idx] <- f2$LZB
  beginSelected$T10WithI_F2[idx] <- f2$T10WithI

  if(staGroups$ID[i] %in% zch3x90){
    beginSelected$ID_F3[idx] <- zch_id[3]
    beginSelected$TZF_F3[idx] <- f3$TFZ
    beginSelected$NUM_TFZ_F3[idx] <- f3$NUM_TFZ
    beginSelected$TOTALWEIGHT_F3[idx] <- f3$TOTALWEIGHT
    beginSelected$VMAX_F3[idx] <- f3$VMAX
    beginSelected$BREAKCLASS_F3[idx] <- f3$BREAKCLASS
    beginSelected$BrH_F3[idx] <- f3$BrH
    beginSelected$LZB_F3[idx] <- f3$LZB
    beginSelected$T10WithI_F3[idx] <- f3$T10WithI
  }else{
    beginSelected$ID_F3[idx] <- ""
    beginSelected$TZF_F3[idx] <- ""
    beginSelected$NUM_TFZ_F3[idx] <- ""
    beginSelected$TOTALWEIGHT_F3[idx] <- ""
    beginSelected$VMAX_F3[idx] <- ""
    beginSelected$BREAKCLASS_F3[idx] <- ""
    beginSelected$BrH_F3[idx] <- ""
    beginSelected$LZB_F3[idx] <- ""
    beginSelected$T10WithI_F3[idx] <- ""
  }

  if(calcT10SYS){
    tempFrame$T10SYSwithI <- 0

    elem <- tfzNames[tfzNames$name == f1$TFZ, ]
    avModel = getAVModel(elem$i, elem$j, f1$TOTALWEIGHT, f1$NUM_TFZ, addTfzMass = F)
    tempFrame$T10SYSwithI[res1 == 1] <- round(0.5* calculate10kmWithI(avModel, min(100, f1$VMAX), f1$BREAKCLASS, 7) +
                                                0.5* calculate10km(avModel, min(100, f1$VMAX), f1$BREAKCLASS), 1)

    elem <- tfzNames[tfzNames$name == f2$TFZ, ]
    avModel = getAVModel(elem$i, elem$j, f2$TOTALWEIGHT, f2$NUM_TFZ, addTfzMass = F)
    tempFrame$T10SYSwithI[y == 1] <- round(0.5* calculate10kmWithI(avModel, min(100, f2$VMAX), f2$BREAKCLASS, 7) +
                                             0.5* calculate10km(avModel, min(100, f2$VMAX), f2$BREAKCLASS), 1)
    if(staGroups$ID[i] %in% zch3x90){
      elem <- tfzNames[tfzNames$name == f3$TFZ, ]
      avModel = getAVModel(elem$i, elem$j, f3$TOTALWEIGHT, f3$NUM_TFZ, addTfzMass = F)
      tempFrame$T10SYSwithI[z == 1] <- round(0.5* calculate10kmWithI(avModel, min(100, f3$VMAX), f3$BREAKCLASS, 7) +
                                               0.5* calculate10km(avModel, min(100, f3$VMAX), f3$BREAKCLASS), 1)
    }
  }else{
    tempFrame$T10SYSwithI <- tempFrame$T10withI7
    tempFrame$T10SYSwithI[res1 == 1] <- round(f1$T10WithI, 1)
    tempFrame$T10SYSwithI[y == 1] <- round(f2$T10WithI, 1)
    if(staGroups$ID[i] %in% zch3x90){
      tempFrame$T10SYSwithI[z == 1] <- round(f3$T10WithI, 1)
    }
  }

  tempFrame$STA_BFQ <- round(1.0*tempFrame$T10SYSwithI/tempFrame$T10withI7, 3)
  write.csv2(tempFrame, file = paste0(helper.getResultPath(OPT_FOLDER), "/", staGroups$ID[i], ".csv"), row.names = F)
}

beginSelected$BrH_F1[which(beginSelected$BrH_F1 == Inf)] <- 70
beginSelected$BrH_F2[which(beginSelected$BrH_F2 == Inf)] <- 60
beginSelected$BrH_F3[which(beginSelected$BrH_F3 == Inf)] <- 60

write.csv2(beginSelected, file = helper.getResultPath(OUT_POST_OPTI), row.names = F)

# show user all2x90
#table(beginSelected$SEL)


# get statistics BFQ
x <- data.frame(Quantile = c(seq(0, 1, 0.1)), stringsAsFactors = F)
for(i in 1:length(staGroups$ID)){
  tempFrame <- read.csv2(paste0(helper.getResultPath(OPT_FOLDER), "/", staGroups$ID[i], ".csv"), stringsAsFactors = F)
  x <- cbind(x, data.frame(Q = quantile(tempFrame$STA_BFQ, c(seq(0, 1, 0.1))), stringsAsFactors = F))
}
colnames(x) <- c("Quantile", staGroups$ID)
helper.log("########## STATISTICS ##############")
helper.log(paste("Median of STA_BFQ", rowSums(x[6,-1]) / ncol(x[,-1])))
helper.log(paste("Max of STA_BFQ", rowSums(x[11,-1]) / ncol(x[,-1])))


# show selected ZCH
# for(i in 1:length(unique(beginSelected$SEL))){
#   idx <- unlist(strsplit(unique(beginSelected$SEL)[i], "#"))
#   print(unique(beginSelected$SEL)[i])
#   print(fullFrame[idx,])
# }
#
# # statistics
# gainFrame$STA[notCovered == 1]
# t10[notCovered == 1]
# all2x90[selectedList]
#
# ####### END #######
#
# # correct T10km for beginSelected
# for(j in 1:length(beginSelected$SEL)){
#   elem <- tfzNames[tfzNames$name == beginSelected$TZF_F1[j], ]
#   avModel = getAVModel(elem$i, elem$j, beginSelected$TOTALWEIGHT_F1[j], beginSelected$NUM_TFZ_F1[j], addTfzMass = F)
#   beginSelected$T10WithI_F1[j] <- round(0.5* calculate10kmWithI(avModel, min(100, beginSelected$VMAX_F1[j]), beginSelected$BREAKCLASS_F1[j], 7) +
#                                           0.5* calculate10km(avModel, min(100, beginSelected$VMAX_F1[j]), beginSelected$BREAKCLASS_F1[j]), 1)
#
#   elem <- tfzNames[tfzNames$name == beginSelected$TZF_F2[j], ]
#   avModel = getAVModel(elem$i, elem$j, beginSelected$TOTALWEIGHT_F2[j], beginSelected$NUM_TFZ_F2[j], addTfzMass = F)
#   beginSelected$T10WithI_F2[j] <- round(0.5* calculate10kmWithI(avModel, min(100, beginSelected$VMAX_F2[j]), beginSelected$BREAKCLASS_F2[j], 7) +
#                                           0.5* calculate10km(avModel, min(100, beginSelected$VMAX_F2[j]), beginSelected$BREAKCLASS_F2[j]), 1)
# }
#
# write.csv2(beginSelected, file = "./result_detail_v117/optimizedSelected_v06.csv", row.names = F)
