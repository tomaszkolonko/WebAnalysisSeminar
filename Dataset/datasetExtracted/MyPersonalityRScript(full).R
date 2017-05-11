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

# Add Corrplot library for correlation matrxi
library(corrplot)
# Add String manipulation library
library(stringr)

# ===============================================
# BIG FIVE "sEXT"; "sNEU"; "sAGR"; "sCON"; "sOPN"
# ===============================================

# Plot: Neuroticism ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"sNEU"]
c <- cor(x,y)
marks <- 1:5
plot(x,y, main=paste("Big Five: Neuroticism explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Neuroticism", xlab="Extraversion")



# Plot: Agreeableness ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"sAGR"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Agreeableness explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Agreeableness", xlab="Extraversion")

# Plot: Conscientiousness ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"sCON"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Conscientiousness explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Conscientiousness", xlab="Extraversion")

# Plot: Openness ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"sOPN"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Openness explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Openness", xlab="Extraversion")

# Plot: Agreeableness ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"sAGR"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Agreeableness explained by Neuroticism\n(cor: ", round(c, digits=4), ")"), ylab="Agreeableness", xlab="Neuroticism")
mod<-lm(y ~ x)
x_Regression <- mod$coefficients[1]
y_Regression <- mod$coefficients[2]
abline(x_Regression, y_Regression)


# Plot: Conscientiousness ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"sCON"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Conscientiousness explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="Conscientiousness", xlab="Neuroticism")

# Plot: Openness ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"sOPN"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Openness explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="Openness", xlab="Neuroticism")


# Plot: Conscientiousness ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"sCON"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Conscientiousness explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="Conscientiousness", xlab="Agreeableness")

# Plot: Openness ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"sOPN"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Openness explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="Openness", xlab="Agreeableness")


# Plot: Openness ~ Conscientiousness
x <- myDataUniqueID[,"sCON"]
y <- myDataUniqueID[,"sOPN"]
c <- cor(x,y)
plot(x,y, main=paste("Big Five: Openness explained by Conscientiousness \n(cor: ", round(c, digits=4), ")"), ylab="Openness", xlab="Conscientiousness")





# ==================================
# BIG FIVE compared to "NETWORKSIZE"
# ==================================

# Plot: Networksize ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"NETWORKSIZE"]
c <- cor(x,y)
plot(x,y, main=paste("Networksize explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Networksize", xlab="Extraversion")
mod<-lm(y ~ x)
x_Regression <- mod$coefficients[1]
y_Regression <- mod$coefficients[2]
abline(x_Regression, y_Regression)

# Plot: Networksize ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"NETWORKSIZE"]
c <- cor(x,y)
plot(x,y, main=paste("Networksize explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="Networksize", xlab="Neuroticism")

# Plot: Networksize ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"NETWORKSIZE"]
c <- cor(x,y)
plot(x,y, main=paste("Networksize explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="Networksize", xlab="Agreeableness")

# Plot: Networksize ~ Conscientiousness
x <- myDataUniqueID[,"sCON"]
y <- myDataUniqueID[,"NETWORKSIZE"]
c <- cor(x,y)
plot(x,y, main=paste("Networksize explained by Conscientiousness \n(cor: ", round(c, digits=4), ")"), ylab="Networksize", xlab="Conscientiousness")

# Plot: Networksize ~ Openness
x <- myDataUniqueID[,"sOPN"]
y <- myDataUniqueID[,"NETWORKSIZE"]
c <- cor(x,y)
plot(x,y, main=paste("Networksize explained by Openness \n(cor: ", round(c, digits=4), ")"), ylab="Networksize", xlab="Openness")





# ==================================
# BIG FIVE compared to "BETWEENNESS"
# ==================================

# Plot: Betweenness ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"BETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("Betweenness explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Betweenness", xlab="Extraversion")

# Plot: Betweenness ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"BETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("Betweenness explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="Betweenness", xlab="Neuroticism")

# Plot: Betweenness ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"BETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("Betweenness explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="Betweenness", xlab="Agreeableness")

# Plot: Betweenness ~ Conscientiousness
x <- myDataUniqueID[,"sCON"]
y <- myDataUniqueID[,"BETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("Betweenness explained by Conscientiousness \n(cor: ", round(c, digits=4), ")"), ylab="Betweenness", xlab="Conscientiousness")

# Plot: Betweenness ~ Openness
x <- myDataUniqueID[,"sOPN"]
y <- myDataUniqueID[,"BETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("Betweenness explained by Openness \n(cor: ", round(c, digits=4), ")"), ylab="Betweenness", xlab="Openness")





# ===================================
# BIG FIVE compared to "NBETWEENNESS"
# ===================================

# Plot: N-Betweenness ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"NBETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("N-Betweenness explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="N-Betweenness", xlab="Extraversion")

# Plot: N-Betweenness ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"NBETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("N-Betweenness explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="N-Betweenness", xlab="Neuroticism")

# Plot: N-Betweenness ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"NBETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("N-Betweenness explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="N-Betweenness", xlab="Agreeableness")

# Plot: N-Betweenness ~ Conscientiousness
x <- myDataUniqueID[,"sCON"]
y <- myDataUniqueID[,"NBETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("N-Betweenness explained by Conscientiousness \n(cor: ", round(c, digits=4), ")"), ylab="N-Betweenness", xlab="Conscientiousness")

# Plot: N-Betweenness ~ Openness
x <- myDataUniqueID[,"sOPN"]
y <- myDataUniqueID[,"NBETWEENNESS"]
c <- cor(x,y)
plot(x,y, main=paste("N-Betweenness explained by Openness \n(cor: ", round(c, digits=4), ")"), ylab="N-Betweenness", xlab="Openness")




# ==============================
# BIG FIVE compared to "DENSITY"
# ==============================

# Plot: Density ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"DENSITY"]
c <- cor(x,y)
plot(x,y, main=paste("Density explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Density", xlab="Extraversion")

# Plot: Density ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"DENSITY"]
c <- cor(x,y)
plot(x,y, main=paste("Density explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="Density", xlab="Neuroticism")

# Plot: Density ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"DENSITY"]
c <- cor(x,y)
plot(x,y, main=paste("Density explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="Density", xlab="Agreeableness")

# Plot: Density ~ Conscientiousness
x <- myDataUniqueID[,"sCON"]
y <- myDataUniqueID[,"DENSITY"]
c <- cor(x,y)
plot(x,y, main=paste("Density explained by Conscientiousness \n(cor: ", round(c, digits=4), ")"), ylab="Density", xlab="Conscientiousness")

# Plot: Density ~ Openness
x <- myDataUniqueID[,"sOPN"]
y <- myDataUniqueID[,"DENSITY"]
c <- cor(x,y)
plot(x,y, main=paste("Density explained by Openness \n(cor: ", round(c, digits=4), ")"), ylab="Density", xlab="Openness")





# ================================
# BIG FIVE compared to "BROKERAGE"
# ================================

# Plot: Brokerage ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"BROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("Brokerage explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Brokerage", xlab="Extraversion")

# Plot: Brokerage ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"BROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("Brokerage explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="Brokerage", xlab="Neuroticism")

# Plot: Brokerage ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"BROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("Brokerage explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="Brokerage", xlab="Agreeableness")

# Plot: Brokerage ~ Conscientiousness
x <- myDataUniqueID[,"sCON"]
y <- myDataUniqueID[,"BROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("Brokerage explained by Conscientiousness \n(cor: ", round(c, digits=4), ")"), ylab="Brokerage", xlab="Conscientiousness")

# Plot: Brokerage ~ Openness
x <- myDataUniqueID[,"sOPN"]
y <- myDataUniqueID[,"BROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("Brokerage explained by Openness \n(cor: ", round(c, digits=4), ")"), ylab="Brokerage", xlab="Openness")






# ===================================
# BIG FIVE compared to "NBROKERAGE"
# ===================================

# Plot: N-Brokerage ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"NBROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("N-Brokerage explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="N-Brokerage", xlab="Extraversion")

# Plot: N-Brokerage ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"NBROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("N-Brokerage explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="N-Brokerage", xlab="Neuroticism")

# Plot: N-Brokerage ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"NBROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("N-Brokerage explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="N-Brokerage", xlab="Agreeableness")

# Plot: N-Brokerage ~ Conscientiousness
x <- myDataUniqueID[,"sCON"]
y <- myDataUniqueID[,"NBROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("N-Brokerage explained by Conscientiousness \n(cor: ", round(c, digits=4), ")"), ylab="N-Brokerage", xlab="Conscientiousness")

# Plot: N-Brokerage ~ Openness
x <- myDataUniqueID[,"sOPN"]
y <- myDataUniqueID[,"NBROKERAGE"]
c <- cor(x,y)
plot(x,y, main=paste("N-Brokerage explained by Openness \n(cor: ", round(c, digits=4), ")"), ylab="N-Brokerage", xlab="Openness")




# ===================================
# BIG FIVE compared to "TRANSITIVITY"
# ===================================

# Plot: Transitivity ~ Extraversion
x <- myDataUniqueID[,"sEXT"]
y <- myDataUniqueID[,"TRANSITIVITY"]
c <- cor(x,y)
plot(x,y, main=paste("Transitivity explained by Extraversion \n(cor: ", round(c, digits=4), ")"), ylab="Transitivity", xlab="Extraversion")

# Plot: Transitivity ~ Neuroticism
x <- myDataUniqueID[,"sNEU"]
y <- myDataUniqueID[,"TRANSITIVITY"]
c <- cor(x,y)
plot(x,y, main=paste("Transitivity explained by Neuroticism \n(cor: ", round(c, digits=4), ")"), ylab="Transitivity", xlab="Neuroticism")

# Plot: Transitivity ~ Agreeableness
x <- myDataUniqueID[,"sAGR"]
y <- myDataUniqueID[,"TRANSITIVITY"]
c <- cor(x,y)
plot(x,y, main=paste("Transitivity explained by Agreeableness \n(cor: ", round(c, digits=4), ")"), ylab="Transitivity", xlab="Agreeableness")

# Plot: Transitivity ~ Conscientiousness
x <- myDataUniqueID[,"sCON"]
y <- myDataUniqueID[,"TRANSITIVITY"]
c <- cor(x,y)
plot(x,y, main=paste("Transitivity explained by Conscientiousness \n(cor: ", round(c, digits=4), ")"), ylab="Transitivity", xlab="Conscientiousness")

# Plot: Transitivity ~ Openness
x <- myDataUniqueID[,"sOPN"]
y <- myDataUniqueID[,"TRANSITIVITY"]
c <- cor(x,y)
plot(x,y, main=paste("Transitivity explained by Openness \n(cor: ", round(c, digits=4), ")"), ylab="Transitivity", xlab="Openness")


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

myDataMonthAndDay <- read.csv(file="mypersonality_final_utf8_ok_cleaned.csv",head=TRUE,sep=",")

for(i in 1:nrow(myDataMonthAndDay)) {
  myDataMonthAndDay[i,'month'] <- convertToMonth(myDataMonthAndDay[i,'DATE'])
  myDataMonthAndDay[i,'timeOfDay'] <- convertToTimeOfDay(myDataMonthAndDay[i,'DATE'])
}

keepsAll <- c(keepsBigFive,keepsEgoNetwork, "month", "timeOfDay")
myDataMonthAndDay <- myDataMonthAndDay[keepsAll]

unique(myDataMonthAndDay$timeOfDay)

M <- cor(myDataMonthAndDay)
corrplot(M, method="circle")

# Get the average length of the messages and add this new variable: "avgLength"
# ***************************************************************************** DONE

#myDataWithAvgLength$avgLength <- with(myData, str_length(myData[[STATUS]]))
myDataPostLength <- read.csv(file="mypersonality_final_utf8_ok_cleaned.csv",head=TRUE,sep=",")
myDataPostLength[[paste('avgLength')]] <- str_length(myDataPostLength$STATUS)

library(data.table)
dt <- data.table(myDataPostLength)
dt2 <- dt[,list(totalLength = sum(`avgLength`), freq = .N), by = c("X.AUTHID")]
dt2 <- transform(dt2, avgLength = totalLength / freq)
colnames(dt2) <- c("X.AUTHID", "totalLength", "freq", "avgLength")

frequencyTable <- data.frame(table(myDataPostLength$X.AUTHID)) 
colnames(frequencyTable) <- c("X.AUTHID", "postsTotal") 
m1 <- merge(myDataUniqueID, frequencyTable, by.x = "X.AUTHID") 

m1 <- merge(m1, dt2, by.x = "X.AUTHID")

keepsAll <- c(keepsBigFive,keepsEgoNetwork, 'postsTotal', 'totalLength', 'avgLength')
m1 <- m1[keepsAll]

M <- cor(m1)
corrplot(M, method="circle")

# Get the number of exclamations marks and add this new variable: "exMarks"
# ************************************************************************* DONE

myDataExclamationMarks <- read.csv(file="mypersonality_final_utf8_ok.csv",head=TRUE,sep=",")
for(i in 1:nrow(myDataExclamationMarks)) {
  myDataExclamationMarks[i,'exMarks'] <- findAllExMarks(myDataExclamationMarks[i,'STATUS'])
}

keepsAll <- c("X.AUTHID",keepsBigFive,keepsEgoNetwork, "exMarks")

dt <- data.table(myDataExclamationMarks[keepsAll])
dt2 <- dt[,list(totalExMarks = sum(`exMarks`), freq = .N), by = c("X.AUTHID")]
dt2 <- transform(dt2, avgExMarks = totalExMarks / freq)


keepsAll <- c("X.AUTHID",keepsBigFive,keepsEgoNetwork)
myDataExclamationMarks[keepsAll]
myDataExclamationMarks <- subset(myDataExclamationMarks, !duplicated(myDataExclamationMarks$X.AUTHID))

m1 <- merge(myDataExclamationMarks, dt2, by.x = "X.AUTHID")

keepsAll <- c(keepsBigFive,keepsEgoNetwork, "totalExMarks", "avgExMarks")
m1 <- m1[keepsAll]

M <- cor(m1)
corrplot(M, method="circle")


# Get the ration of Capital Letters over regular letters
# ****************************************************** DONE

myDataRatioUL <- read.csv(file="mypersonality_final_utf8_ok.csv",head=TRUE,sep=",")
for(i in 1:nrow(myDataRatioUL)) {
  myDataRatioUL[i,'ratioUL'] <- findRatioUpperLowerLetters(myDataRatioUL[i,'STATUS'])
}

keepsAll <- c("X.AUTHID",keepsBigFive,keepsEgoNetwork, "ratioUL")

dt <- data.table(myDataRatioUL[keepsAll])
dt2 <- dt[,list(totalRatioUL = sum(`ratioUL`), freq = .N), by = c("X.AUTHID")]
dt2 <- transform(dt2, avgRatioUL = totalRatioUL / freq)

keepsAll <- c("X.AUTHID",keepsBigFive,keepsEgoNetwork)
myDataRatioUL <- myDataRatioUL[keepsAll]
myDataRatioUL <- subset(myDataRatioUL, !duplicated(myDataRatioUL$X.AUTHID))

m1 <- merge(myDataRatioUL, dt2, by.x = "X.AUTHID")

keepsAll <- c(keepsBigFive,keepsEgoNetwork, "avgRatioUL")
m1 <- m1[keepsAll]

M <- cor(m1)
corrplot(M, method="circle")

