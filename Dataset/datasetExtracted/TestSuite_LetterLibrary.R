# TEST SUITE FOR LETTER LIBRARY
# =============================

# Read in the DB
setwd("/home/tomasz/Documents/webanalysis/Dataset/datasetExtracted")

print("TESTING: findAllExMarks function")
print("================================")

source("LetterLibrary.R")

sentenceNoExMark = "Hello, my name is Peter."
sentenceOneExMark = "Hello, my name is Peter!"
sentenceTwoExMark = "Hello, my name is Peter!!"

if(findAllExMarks(sentenceNoExMark) == 0) {
  print("PASS")
} else {
  print("FAIL: findAllExMarks")
}

if(findAllExMarks(sentenceOneExMark) == 1) {
  print("PASS")
} else {
  print("FAIL: findAllExMarks")
}

if(findAllExMarks(sentenceTwoExMark) == 2) {
  print("PASS")
} else {
  print("FAIL: findAllExMarks")
}


print("TESTING: findRatioUpperLowerLetters function")
print("===========================================")

source("LetterLibrary.R")

sentenceToBeCleaned = "has a serious crush on *PROPNAME*. <3"

if(findRatioUpperLowerLetters(sentenceNoExMark) == 0.125) {
  print("PASS")
} else {
  print("FAIL: findRatioUpperLowerLetters")
}

if(findRatioUpperLowerLetters("239487%&/") == 0) {
  print("PASS")
} else {
  print("FAIL: findRatioUpperLowerLetters")
}

if(findRatioUpperLowerLetters(sentenceTwoExMark) == 0.125) {
  print("PASS")
} else {
  print("FAIL: findRatioUpperLowerLetters")
}

if(findRatioUpperLowerLetters(sentenceToBeCleaned) == 0) {
  print("PASS")
} else {
  print("FAIL: findRatioUpperLowerLetters")
}


print("TESTING: convertToMonth function")
print("================================")

source("LetterLibrary.R")

timeOne = "10/04/09 23:04"
timeTwo = "07/17/09 06:08 AM"
timeThree = "13/26/09 10:58 PM"

if(convertToMonth(timeOne) == 10) {
  print("PASS")
} else {
  print("FAIL: convertToMonth")
}

if(convertToMonth(timeTwo) == 7) {
  print("PASS")
} else {
  print("FAIL: convertToMonth")
}

if(convertToMonth(timeThree) == 0) {
  print("PASS")
} else {
  print("FAIL: convertToMonth")
}

print("TESTING: convertToTimeOfDay function")
print("====================================")

source("LetterLibrary.R")

timeOne = "10/04/09 23:04"
timeTwo = "07/17/09 06:08 AM"
timeThree = "13/26/09 10:58 PM"
timeFour = "07/09/09 14:58"

if(convertToTimeOfDay(timeOne) == 23) {
  print("PASS")
} else {
  print("FAIL: convertToMonth")
}

if(convertToTimeOfDay(timeTwo) == 6) {
  print("PASS")
} else {
  print("FAIL: convertToMonth")
}

if(convertToTimeOfDay(timeThree) == 22) {
  print("PASS")
} else {
  print("FAIL: convertToMonth")
}

if(convertToTimeOfDay(timeFour) == 14) {
  print("PASS")
} else {
  print("FAIL: convertToMonth")
}

