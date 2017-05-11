# Web Analysis and Monitoring (FULL VERSION)
# =========================================

# Read in the DB
setwd("/home/tomasz/Documents/webanalysis/Dataset/datasetExtracted")
myData <- read.csv(file="mypersonality_final_utf8_ok_cleaned.csv",head=TRUE,sep=",")

# remove everything but x
# rm(list=setdiff(ls(), "x"))


# First round of correlation will ignore the messages and therefor reduce
# redundancy by extracting only unique rows based on X.AUTHID
myDataUniqueID <- subset(myData, !duplicated(myData$X.AUTHID))

# Add Corrplot library for correlation matrxi
library(corrplot)
# Add String manipulation library
library(stringr)

# ===================================
# Correlation - Big Five
# ===================================

myDataFrameBigFive <- data.frame(myDataUniqueID)

keepsBigFive <- c("sEXT", "sNEU", "sAGR", "sCON", "sOPN")
myDataFrameBigFive <- myDataFrameBigFive[keepsBigFive]



# =====================================
# Correlation - Ego Network
# =====================================

myDataFrameEgoNetwork <- data.frame(myDataUniqueID)

keepsEgoNetwork <- c("NETWORKSIZE", "BETWEENNESS", "NBETWEENNESS", "DENSITY", "BROKERAGE", "NBROKERAGE", "TRANSITIVITY")
myDataFrameEgoNetwork <- myDataFrameEgoNetwork[keepsEgoNetwork]


# ================================
# Correlation Matrix of everything
# ================================

myDataFrameTotal <- data.frame(myDataUniqueID)

keepsAll <- c(keepsBigFive,keepsEgoNetwork)
myDataFrameTotal <- myDataFrameTotal[keepsAll]


# Find the best model for ***sEXT*** with the forwading strategy EVERTHING INCLUDED
step(lm(sEXT~1., data=myDataFrameTotal), direction="forward", scope=~sNEU+sAGR+sCON+sOPN+
       NETWORKSIZE+BETWEENNESS+NBETWEENNESS+DENSITY+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = sEXT ~ sNEU + NETWORKSIZE + BROKERAGE + sOPN + NBETWEENNESS, 
# data = myDataFrameTotal)
summary(lm(formula = sEXT ~ sNEU + NETWORKSIZE + BROKERAGE + sOPN + NBETWEENNESS, 
           data = myDataFrameTotal))

# Find the best model for ***sNEU*** with the forwading strategy EVERTHING INCLUDED
step(lm(sNEU~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sAGR+sCON+sOPN+
       NETWORKSIZE+BETWEENNESS+NBETWEENNESS+DENSITY+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = sNEU ~ sAGR + sEXT + sCON + NBETWEENNESS + TRANSITIVITY, 
# data = myDataFrameTotal)
summary(lm(formula = sNEU ~ sAGR + sEXT + sCON + NBETWEENNESS + TRANSITIVITY, 
            data = myDataFrameTotal))

# Find the best model for ***sAGR*** with the forwading strategy EVERTHING INCLUDED
step(lm(sAGR~1., data=myDataFrameTotal), direction="forward", scope=~sNEU+sEXT+sCON+sOPN+
       NETWORKSIZE+BETWEENNESS+NBETWEENNESS+DENSITY+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = sAGR ~ sNEU + TRANSITIVITY + sOPN, data = myDataFrameTotal)
summary(lm(formula = sAGR ~ sNEU + TRANSITIVITY + sOPN, data = myDataFrameTotal))

# Find the best model for ***sCON*** with the forwading strategy EVERTHING INCLUDED
step(lm(sCON~1., data=myDataFrameTotal), direction="forward", scope=~sNEU+sAGR+sEXT+sOPN+
       NETWORKSIZE+BETWEENNESS+NBETWEENNESS+DENSITY+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = sCON ~ sNEU + DENSITY + NBROKERAGE, data = myDataFrameTotal)
summary(lm(formula = sCON ~ sNEU + DENSITY + NBROKERAGE, data = myDataFrameTotal))

# Find the best model for ***sOPN*** with the forwading strategy EVERTHING INCLUDED
step(lm(sOPN~1., data=myDataFrameTotal), direction="forward", scope=~sNEU+sAGR+sCON+sEXT+
       NETWORKSIZE+BETWEENNESS+NBETWEENNESS+DENSITY+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = sOPN ~ sEXT + sAGR + NBETWEENNESS + TRANSITIVITY, 
# data = myDataFrameTotal)
summary(lm(formula = sOPN ~ sEXT + sAGR + NBETWEENNESS + TRANSITIVITY, 
           data = myDataFrameTotal))





# Find the best model for ***NETWORKSIZE*** with the forwading strategy EVERTHING INCLUDED
step(lm(NETWORKSIZE~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN+
       BETWEENNESS+NBETWEENNESS+DENSITY+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = NETWORKSIZE ~ BROKERAGE + DENSITY + NBROKERAGE + 
# sEXT + BETWEENNESS + sOPN, data = myDataFrameTotal)
summary(lm(formula = NETWORKSIZE ~ BROKERAGE + DENSITY + NBROKERAGE + 
             sEXT + BETWEENNESS + sOPN, data = myDataFrameTotal))


# Find the best model for ***BETWEENNESS*** with the forwading strategy EVERTHING INCLUDED
step(lm(BETWEENNESS~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN+
       NETWORKSIZE+NBETWEENNESS+DENSITY+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = BETWEENNESS ~ BROKERAGE + NBETWEENNESS + DENSITY + 
# NETWORKSIZE, data = myDataFrameTotal)
summary(lm(formula = BETWEENNESS ~ BROKERAGE + NBETWEENNESS + DENSITY + 
             NETWORKSIZE, data = myDataFrameTotal))

# Find the best model for ***NBETWEENNESS*** with the forwading strategy EVERTHING INCLUDED
step(lm(NBETWEENNESS~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN+
       BETWEENNESS+NETWORKSIZE+DENSITY+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = NBETWEENNESS ~ NBROKERAGE, data = myDataFrameTotal)
summary(lm(formula = NBETWEENNESS ~ NBROKERAGE, data = myDataFrameTotal))

# Find the best model for ***DENSITY*** with the forwading strategy EVERTHING INCLUDED
step(lm(DENSITY~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN+
       BETWEENNESS+NBETWEENNESS+NETWORKSIZE+BROKERAGE+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = DENSITY ~ NBROKERAGE + NETWORKSIZE + BETWEENNESS + 
# sCON + BROKERAGE + NBETWEENNESS, data = myDataFrameTotal)
summary(lm(formula = DENSITY ~ NBROKERAGE + NETWORKSIZE + BETWEENNESS + 
             sCON + BROKERAGE + NBETWEENNESS, data = myDataFrameTotal))

# Find the best model for ***BROKERAGE*** with the forwading strategy EVERTHING INCLUDED
step(lm(BROKERAGE~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN+
       BETWEENNESS+NBETWEENNESS+NETWORKSIZE+DENSITY+NBROKERAGE+TRANSITIVITY)
# Best solution: lm(formula = BROKERAGE ~ BETWEENNESS + NBETWEENNESS + DENSITY + 
#NETWORKSIZE, data = myDataFrameTotal)
summary(lm(formula = BROKERAGE ~ BETWEENNESS + NBETWEENNESS + DENSITY + 
             NETWORKSIZE, data = myDataFrameTotal))

# Find the best model for ***NBROKERAGE*** with the forwading strategy EVERTHING INCLUDED
step(lm(NBROKERAGE~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN+
       BETWEENNESS+NBETWEENNESS+NETWORKSIZE+BROKERAGE+DENSITY+TRANSITIVITY)
# Best solution: lm(formula = NBROKERAGE ~ DENSITY + NBETWEENNESS + TRANSITIVITY + 
# NETWORKSIZE + BROKERAGE + sCON, data = myDataFrameTotal)
summary(lm(formula = NBROKERAGE ~ DENSITY + NBETWEENNESS + TRANSITIVITY + 
             NETWORKSIZE + BROKERAGE + sCON, data = myDataFrameTotal))

# Find the best model for ***TRANSITIVITY*** with the forwading strategy EVERTHING INCLUDED
step(lm(TRANSITIVITY~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN+
       BETWEENNESS+NBETWEENNESS+NETWORKSIZE+BROKERAGE+NBROKERAGE+DENSITY)
# Best solution: lm(formula = TRANSITIVITY ~ NBROKERAGE + sAGR + BETWEENNESS + 
# BROKERAGE + sNEU, data = myDataFrameTotal)
summary(lm(formula = TRANSITIVITY ~ NBROKERAGE + sAGR + BETWEENNESS + 
             BROKERAGE + sNEU, data = myDataFrameTotal))






# ==========================================================================================
# Checking Ego Network ONLY by Big Five
# ==========================================================================================



# Find the best model for ***NETWORKSIZE*** with the forwading strategy EVERTHING INCLUDED
step(lm(NETWORKSIZE~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN)
# Best solution: lm(formula = NETWORKSIZE ~ sEXT + sCON, data = myDataFrameTotal)
summary(lm(formula = NETWORKSIZE ~ sEXT + sCON, data = myDataFrameTotal))


# Find the best model for ***BETWEENNESS*** with the forwading strategy EVERTHING INCLUDED
step(lm(BETWEENNESS~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN)
# Best solution: lm(formula = BETWEENNESS ~ sEXT, data = myDataFrameTotal)
summary(lm(formula = BETWEENNESS ~ sEXT, data = myDataFrameTotal))

# Find the best model for ***NBETWEENNESS*** with the forwading strategy EVERTHING INCLUDED
step(lm(NBETWEENNESS~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN)
# Best solution: lm(formula = NBETWEENNESS ~ sEXT + sOPN, data = myDataFrameTotal)
summary(lm(formula = NBETWEENNESS ~ sEXT + sOPN, data = myDataFrameTotal))

# Find the best model for ***DENSITY*** with the forwading strategy EVERTHING INCLUDED
step(lm(DENSITY~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN)
# Best solution: lm(formula = DENSITY ~ sEXT + sCON + sOPN, data = myDataFrameTotal)
summary(lm(formula = DENSITY ~ sEXT + sCON + sOPN, data = myDataFrameTotal))

# Find the best model for ***BROKERAGE*** with the forwading strategy EVERTHING INCLUDED
step(lm(BROKERAGE~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN)
# Best solution: lm(formula = BROKERAGE ~ sEXT, data = myDataFrameTotal)
summary(lm(formula = BROKERAGE ~ sEXT, data = myDataFrameTotal))

# Find the best model for ***NBROKERAGE*** with the forwading strategy EVERTHING INCLUDED
step(lm(NBROKERAGE~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN)
# Best solution: lm(formula = NBROKERAGE ~ sEXT + sOPN, data = myDataFrameTotal)
summary(lm(formula = NBROKERAGE ~ sEXT + sOPN, data = myDataFrameTotal))

# Find the best model for ***TRANSITIVITY*** with the forwading strategy EVERTHING INCLUDED
step(lm(TRANSITIVITY~1., data=myDataFrameTotal), direction="forward", scope=~sEXT+sNEU+sAGR+sCON+sOPN)
# Best solution: lm(formula = TRANSITIVITY ~ sEXT + sAGR, data = myDataFrameTotal)
summary(lm(formula = TRANSITIVITY ~ sEXT + sAGR, data = myDataFrameTotal))





