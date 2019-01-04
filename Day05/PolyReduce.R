library(readr)
library(stringr)
library(purrr)

testPoly <- "dabAcCaCBAcCcaDA" %>%
  strsplit("") %>%
  unlist()


data <- read_lines("Input05.txt") %>%
  strsplit("") %>%
  unlist()

#### Part 1 ----

# returns true if x, y are upper and lower case versions of the same letter
Are.CasePair <- function(x, y) {

  (length(x) == 1 && # check to maks sure x isn't empty
     length(y) == 1 && # check to maks sure y isn't empty
     c(x,y) %in% c(letters, LETTERS) && # check both are single letters
     abs(utf8ToInt(x) - utf8ToInt(y)) == 32) # check utf8 codes differ by 32
  
}

# determines if x y are case pairs
# if TRUE, returns pair of replace characters
# if FALSE, returns x y
EvalPair <- function(x, y, re.char = NULL){
  
  if(Are.CasePair(x,y)) c(re.char, re.char) else c(x, y)
  
}

# Appends evaluation of last chr of stream and b to end of stream
StitchEval <- function(stream, b){
  
  lst <- length(stream)
  
  c(stream[-lst], EvalPair(stream[lst], b))
  
}

# Takes string and removes case pairs recursively
RemoveCasePairs <- function(stream){
  
  if(length(stream) == 0) return(stream)
  
  reduce(stream, StitchEval)

}

# calculated of the string after all reactions take place
ReducedLength <- function(stream, callN = NULL){
  
  if(!is.null(callN)) print(sprintf("Call Number: %i", callN))
  
  length(RemoveCasePairs(stream))
  
}

test1Ans <- ReducedLength(testPoly)
TEST1CHK <- 10

if(test1Ans != TEST1CHK){
  stop("Incorrect amount of units remaining in test case. ", 
       sprintf("Result is %s instead of %s", test1Ans, TEST1CHK),
       call. = FALSE)
} else {
  message("Test 1 Passed")
}

print(sprintf("Answer for part 1: %i", ReducedLength(data)))

#### Part 2 ----
modTest <- map2(letters[1:4], LETTERS[1:4], 
                ~str_subset(testPoly, sprintf("[^%s%s]", .x, .y)))

modData <- map2(letters, LETTERS, 
                ~str_subset(data, sprintf("[^%s%s]", .x, .y)))

test2Ans <- modTest %>%
  map_int(ReducedLength) %>%
  min()
TEST2CHK <- 4

if(test2Ans != TEST2CHK){
  stop("Incorrect amount of units remaining in min test case 2. ", 
       sprintf("Result is %s instead of %s", test2Ans, TEST2CHK),
       call. = FALSE)
} else {
  message("Test 2 Passed")
}

part2Ans <- modData %>%
  # imap_int(ReducedLength) %>% # use imap to print progress
  map_int(ReducedLength) %>% # use map to calculate silently
  min()

print(sprintf("Answer for part 2: %i", part2Ans))
