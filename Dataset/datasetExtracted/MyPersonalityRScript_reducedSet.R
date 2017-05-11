# Web Analysis and Monitoring (FULL VERSION)
# =========================================

# Read in the DB
setwd("/home/tomasz/Documents/webanalysis/Dataset/datasetExtracted")
myData <- read.csv(file="mypersonality_final_utf8_ok_cleaned.csv",head=TRUE,sep=",")

# remove everything but x
# rm(list=setdiff(ls(), "x"))

# remove everything
# rm(list=ls())


# First round of correlation will ignore the messages and therefore reduce
# redundancy by extracting only unique rows based on X.AUTHID
myDataUniqueID <- subset(myData, !duplicated(myData$X.AUTHID))

############################################################################
# REDUCE SET TO ONLY CONTAIN USERS THAT POSTED MORE THAN 48 POSTS PER YEAR #
#                               OR 4 PER MONTH                            ##
############################################################################

library(data.table)
dt <- data.table(myData)
dt2 <- dt[,list(freq = .N), by = c("X.AUTHID")]
colnames(dt2) <- c("X.AUTHID", "freq")

frequencyTable <- data.frame(table(myData$X.AUTHID)) 
colnames(frequencyTable) <- c("X.AUTHID", "postsTotal") 
m1 <- merge(myDataUniqueID, frequencyTable, by.x = "X.AUTHID") 
m1_reduced <- m1[!(m1$postsTotal<48),]
m1_toBeReduced <- m1[(m1$postsTotal<48),]
m1_toBeReducedByID <- m1_toBeReduced[c("X.AUTHID")]

# myData_reduced <- myData[!(myData$X.AUTHID == m1_toBeReducedByID$X.AUTHID)]
for(i in 1:nrow(m1_toBeReducedByID)) {
  #print(m1_toBeReducedByID[i,"X.AUTHID"])
  myData <- myData[!myData$X.AUTHID == m1_toBeReducedByID[i,"X.AUTHID"],]
}

myDataUniqueID <- subset(myData, !duplicated(myData$X.AUTHID))


# ===================================
# Correlation Matrix of the Big Five
# ===================================

myDataFrameBigFive <- data.frame(myDataUniqueID)

keepsBigFive <- c("sEXT", "sNEU", "sAGR", "sCON", "sOPN")
myDataFrameBigFive <- myDataFrameBigFive[keepsBigFive]


M <- cor(myDataFrameBigFive)
corrplot(M, method="circle")

# =====================================
# Correlation Matrix of the Ego Network
# =====================================

myDataFrameEgoNetwork <- data.frame(myDataUniqueID)

keepsEgoNetwork <- c("NETWORKSIZE", "BETWEENNESS", "NBETWEENNESS", "DENSITY", "BROKERAGE", "NBROKERAGE", "TRANSITIVITY")
myDataFrameEgoNetwork <- myDataFrameEgoNetwork[keepsEgoNetwork]

M <- cor(myDataFrameEgoNetwork)
corrplot(M, method="circle")

# ================================
# Correlation Matrix of everything
# ================================

myDataFrameTotal <- data.frame(myDataUniqueID)

keepsAll <- c(keepsBigFive,keepsEgoNetwork)
myDataFrameTotal <- myDataFrameTotal[keepsAll]

M <- cor(myDataFrameTotal)
corrplot(M, method="circle")




# ======================================================
# Aggregation of existing information into new variables
# ======================================================

source("LetterLibrary.R")

# Get the number of posts relative to the time of day
# *************************************************** DONE

for(i in 1:nrow(myData)) {
  myData[i,'month'] <- convertToMonth(myData[i,'DATE'])
  myData[i,'timeOfDay'] <- convertToTimeOfDay(myData[i,'DATE'])
}

keepsAll <- c(keepsBigFive,keepsEgoNetwork, "month", "timeOfDay")
myData <- myData[keepsAll]

unique(myData$timeOfDay)

M <- cor(myData)
corrplot(M, method="circle")

# Get the average length of the messages and add this new variable: "avgLength"
# ***************************************************************************** DONE


myData[[paste('avgLength')]] <- str_length(myData$STATUS)

keepsBigFive <- c("sEXT", "sNEU", "sAGR", "sCON", "sOPN")
keepsEgoNetwork <- c("NETWORKSIZE", "BETWEENNESS", "NBETWEENNESS", "DENSITY", "BROKERAGE", "NBROKERAGE", "TRANSITIVITY")

library(data.table)
dt <- data.table(myData)
dt2 <- dt[,list(totalLength = sum(`avgLength`), freq = .N), by = c("X.AUTHID")]
dt2 <- transform(dt2, avgLength = totalLength / freq)
colnames(dt2) <- c("X.AUTHID", "totalLength", "freq", "avgLength")

frequencyTable <- data.frame(table(myData$X.AUTHID)) 
colnames(frequencyTable) <- c("X.AUTHID", "postsTotal") 
m1 <- merge(myDataUniqueID, frequencyTable, by.x = "X.AUTHID") 

m1 <- merge(m1, dt2, by.x = "X.AUTHID")

keepsAll <- c(keepsBigFive,keepsEgoNetwork, 'postsTotal', 'totalLength', 'avgLength')
m1 <- m1[keepsAll]

M <- cor(m1)
corrplot(M, method="circle")

# Get the number of exclamations marks and add this new variable: "exMarks"
# ************************************************************************* DONE
source("LetterLibrary.R")
for(i in 1:nrow(myData)) {
  myData[i,'exMarks'] <- findAllExMarks(myData[i,'STATUS'])
}

keepsBigFive <- c("sEXT", "sNEU", "sAGR", "sCON", "sOPN")
keepsEgoNetwork <- c("NETWORKSIZE", "BETWEENNESS", "NBETWEENNESS", "DENSITY", "BROKERAGE", "NBROKERAGE", "TRANSITIVITY")

keepsAll <- c("X.AUTHID",keepsBigFive,keepsEgoNetwork, "exMarks")

dt <- data.table(myData[keepsAll])
dt2 <- dt[,list(totalExMarks = sum(`exMarks`), freq = .N), by = c("X.AUTHID")]
dt2 <- transform(dt2, avgExMarks = totalExMarks / freq)


keepsAll <- c("X.AUTHID",keepsBigFive,keepsEgoNetwork)
myData[keepsAll]
myData <- subset(myData, !duplicated(myData$X.AUTHID))

m1 <- merge(myData, dt2, by.x = "X.AUTHID")

keepsAll <- c(keepsBigFive,keepsEgoNetwork, "totalExMarks", "avgExMarks")
m1 <- m1[keepsAll]

M <- cor(m1)
corrplot(M, method="circle")


# Get the ration of Capital Letters over regular letters
# ****************************************************** DONE
source("LetterLibrary.R")
keepsBigFive <- c("sEXT", "sNEU", "sAGR", "sCON", "sOPN")
keepsEgoNetwork <- c("NETWORKSIZE", "BETWEENNESS", "NBETWEENNESS", "DENSITY", "BROKERAGE", "NBROKERAGE", "TRANSITIVITY")

for(i in 1:nrow(myData)) {
  myData[i,'ratioUL'] <- findRatioUpperLowerLetters(myData[i,'STATUS'])
}

keepsAll <- c("X.AUTHID",keepsBigFive,keepsEgoNetwork, "ratioUL")

dt <- data.table(myData[keepsAll])
dt2 <- dt[,list(totalRatioUL = sum(`ratioUL`), freq = .N), by = c("X.AUTHID")]
dt2 <- transform(dt2, avgRatioUL = totalRatioUL / freq)

keepsAll <- c("X.AUTHID",keepsBigFive,keepsEgoNetwork)
myData <- myData[keepsAll]
myData <- subset(myData, !duplicated(myData$X.AUTHID))

m1 <- merge(myData, dt2, by.x = "X.AUTHID")

keepsAll <- c(keepsBigFive,keepsEgoNetwork, "avgRatioUL")
m1 <- m1[keepsAll]

M <- cor(m1)
corrplot(M, method="circle")

