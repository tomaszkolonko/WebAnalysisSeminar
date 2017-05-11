# Library for computations on Letters retrieved from fb-posts
# ===========================================================

# This function takes a post as a parameter and counts
# all the exclamation marks in it. It returns the number
# if one or more exclamation marks werde found, and 0 if
# no exclamation marks were found
findAllExMarks <- function(facebookPost) {
  allExMarks <- 0
  # length(gregexpr("[.?!*_]", x)[[1]])
  allExMarks <- gregexpr("!", facebookPost)[[1]]
  if(allExMarks[1] < 0) {
    return (0)
  }
  allExMarks <- length(gregexpr("!", facebookPost)[[1]])
  #print(allExMarks)
  if(allExMarks[[1]] > 0) {
    return (allExMarks[[1]])
  }
  return (0)
}

# This function takes a post as a parameter and counts
# all upper case and lower case letters and returns the
# ratio of upper over lower case.
findRatioUpperLowerLetters <- function(facebookPost) {
  upperLetters <- 0
  lowerLetters <- 0
  allLetters <- 0
  
  # clean facebookPost of *PROPNAME*
  facebookPost <- gsub("*PROPNAME*", "", facebookPost, fixed=TRUE)
  
  
  upperLetters <- gregexpr("[A-Z]", facebookPost)[[1]]
  lowerLetters <- gregexpr("[a-z]", facebookPost)[[1]]
  allLetters <- gregexpr("[a-zA-Z]", facebookPost)[[1]]
  
  if(allLetters[[1]] < 0 || upperLetters[[1]] < 0) {
    return (0)
  }
  
  if(lowerLetters[[1]] < 0) {
    lowerLetters = 1
  }
  return (length(upperLetters)/length(lowerLetters))
}

# This function takes a time string and converts it into
# full hours and returns a number between 0 and 23
convertToMonth <- function(facebookPost) {
  month <- 0
  month <- as.numeric(substr(facebookPost, 0, 2))
  if(month <= 12 && month > 0) {
    return (month)
  }
  return(0)
}

# This function takes a time string and converts it into
# full hours and returns a number between 0 and 23
convertToTimeOfDay <- function(facebookPost) {
  PM <- 0
  time <- 0
  PM <- gregexpr(pattern ='PM',facebookPost)
  
  if(PM > 0) {
    time <- as.numeric(substr(facebookPost, 9,11))
    time <- time + 12
  } else {
    time <- as.numeric(substr(facebookPost, 9,11))
  }
  
  return (time)
  
}

