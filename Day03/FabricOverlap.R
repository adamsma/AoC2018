library(readr)
library(stringr)
library(purrr)
library(magrittr)

#### Part 1 ----

AddClaim <- function(sheet, claim) {
  
  colSeq <- seq(from = claim$wStart + 1, to = claim$wEnd)
  rowSeq <- seq(from = claim$lStart + 1, to = claim$lEnd)
  
  sheet[rowSeq, colSeq] <- sheet[rowSeq, colSeq] +1
  sheet
  
}

# rawData <- c("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2") # test data
rawData <- readLines("Input03.txt")

# extract relevant dims from raw data
parsedData <- rawData %>%
  str_sub(start = 2L) %>%
  str_split_fixed("( @ )|(,)|(: )|(x)", n = 5) %>% # parse line
  as.integer() %>%
  matrix(nrow = length(rawData)) %>%
  as.data.frame() %>%
  set_names(c("ID", "wStart", "lStart", "w", "l")) %>%
  mutate(wEnd = wStart + w, lEnd = lStart + l)
  

# determine min size of total fabric
maxW <- max(parsedData$wEnd)
maxL <- max(parsedData$lEnd)

listData <- split(parsedData, seq(length(rawData)))
listData$base <- matrix(0, nrow = maxL, ncol = maxW)

coverage <- reduce_right(listData, AddClaim)

print(sprintf("The answer to Part 1 is: %i", 
              sum(coverage > 1)))

#### Part 2 ----

CheckConflict <- function(checkSheet, claim){
  
  colSeq <- seq(from = claim$wStart + 1, to = claim$wEnd)
  rowSeq <- seq(from = claim$lStart + 1, to = claim$lEnd)
  
  sum(checkSheet[rowSeq, colSeq] > 1) > 0 
  
}

checkLst <- map_lgl(split(parsedData, seq(length(rawData))), CheckConflict,
        checkSheet = coverage)

print(sprintf("The answer to Part 2 is: %i", which(!checkLst)))