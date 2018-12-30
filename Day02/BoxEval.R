library(stringr)
library(dplyr)
library(purrr)
library(readr)

#### Part 1 ----
test <- c("abcdef", "bababc",
          "abbcde", "abcccd",
          "aabcdd", "abcdee",
          "ababab")
TESTCHKSUM <- 12

data <- read_csv("Input02.txt", col_names = FALSE, col_types = "c")

# counts the number of strings in a vector with exactly two repeating letters
TwoMatch <- function(input) {
  
  strsplit(input, "") %>%
    map(table) %>%
    map(as.integer) %>%
    map_lgl(~any(.x == 2)) %>%
    sum()
  
}

# counts the number of strings in a vector with exactly three repeating letters
ThreeMatch <- function(input) {
  
  strsplit(input, "") %>%
    map(table) %>%
    map(as.integer) %>%
    map_lgl(~any(.x == 3)) %>%
    sum()
  
}

# calculates checksum using TwoMatch and ThreeMatch
BoxCheckSum <- function(input){
  TwoMatch(input) * ThreeMatch(input)
}

# run "unit" test for checksum functions
chkTest <- BoxCheckSum(test)

if( chkTest != TESTCHKSUM) {
  stop("Checksum does not work for the test case. ", 
       sprintf("Checksum is %i instead of %i", chkTest, TESTCHKSUM),
       call. = FALSE)
} else {
  message("Test Checksum Passed")
}

# calculate checksum for test data
print(sprintf("Answer for part 1: %i", BoxCheckSum(data[[1]])))

#### Part 2 ----

test2 <- c("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")

PrimStrDist <- function(input){
  
  strsplit(input, "") %>%
    map(factor, levels = letters) %>%
    map(table) %>%
    reduce(bind_rows) %>% 
    dist(method = "euclidean", diag = TRUE, upper = TRUE) %>%
    .^2
  
}

# return pair of codes differing by one letter
FindPair <- function(input){
  
  distMat <- PrimStrDist(input) %>% 
    as.matrix() %>%
    as.integer() %>%
    matrix(nrow = length(input))
  
  shrtLst <-which(distMat == 2, arr.ind = TRUE) %>%
    as.vector() %>%
    unique() %>%
    input[.] 
  
  shrtDist <- shrtLst %>%
    strsplit("") %>%
    map(function(y) map_dbl(., ~sum(.x != y))) %>%
    unlist() %>%
    matrix(nrow = length(shrtLst)) 
  
  which(shrtDist  == 1, arr.ind = TRUE) %>%
    as.vector() %>%
    unique() %>%
    shrtLst[.] 
  
}

# return the letters matching between closest codes
MatchLetters <- function(input, print.pair = FALSE){
  
  p <- FindPair(input) %>% 
    strsplit("") 
  
  if(print.pair){
    message(sprintf("The matching pair is %s and %s",
            str_c(p[[1]], collapse = ""), str_c(p[[2]], collapse = "")))
  }
  
  p[[1]][p[[1]] == p[[2]]] %>%
    str_c(collapse = "")
  
}

# run "unit" test for matching functions
test2Ans <- MatchLetters(test2)
TEST2CHK <- "fgij"
  
if(test2Ans != TEST2CHK){
  stop("Letter matching does not work for the test case. ", 
       sprintf("Result is %s instead of %s", test2Ans, TEST2CHK),
       call. = FALSE)
} else {
  message("Test 2 Passed")
}


print(sprintf("Answer for part 2: %s", 
              MatchLetters(data[[1]], print.pair = TRUE)))