library(purrr)
library(stringr)
library(dplyr)

testData <- "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2" %>%
  strsplit(" ") %>%
  unlist()

data <- readLines("Input08.txt", n = 1) %>%
  strsplit(" ") %>%
  unlist()

#### Part 1 ----

BuildTree <- function(nodeData){
  
  # pop header
  header <- as.numeric(nodeData[1:2])
  childCt <- header[1]
  metaCt <- header[2]
  nodeData <- nodeData[-c(1:2)]

  # initialize empty child list
  children <- list()
  
  if(childCt == 0){
    
    # if no children take next n as meta data
    metaData <- as.numeric(nodeData[seq(metaCt)])
    
  } else {
    
    # Process child nodes
    for(i in seq(childCt)){
      
      children[[i]] <- BuildTree(nodeData)
      
      # determine sequence for children
      chldLen <- length(unlist(children[[i]]))
      
      # remove child sequence from node data + 
      nodeData <- nodeData[-seq(chldLen)]
    }
    
    # meta data is numbers following child nodes
    metaData <- as.numeric(nodeData[seq(metaCt)])

  }
  
  # build node
  list(childCt = childCt, metaCt = metaCt, 
       children = children, metaData = metaData)
  
}

# determines check sum of the meta data after building the tree
MetaCheckSum <- function(tree){
  
  labeledInput <- unlist(tree)
  metaEntries <- str_detect(names(labeledInput), "metaData")
  
  sum(labeledInput[metaEntries])

}

testTree <- BuildTree(testData)
test1Ans <- MetaCheckSum(testTree)
TEST1CHK <- "138"

if(test1Ans != TEST1CHK){
  stop("Incorrect check sum for for test case. ",
       sprintf("Result is %s instead of %s", test1Ans, TEST1CHK),
       call. = FALSE)
} else {
  message("Test 1 Passed")
}

dataTree <- BuildTree(data)
print(sprintf("Answer for part 1: %s", MetaCheckSum(dataTree)))
  
#### Part 2 ----

NodeCheckSum <- function(node){
  
  if(node$childCt == 0){
    
    nodeSum <- sum(node$metaData)
    
  } else {
    
    nodeSum <- 0
    
    for(chld in node$metaData){
      if(chld <=  node$childCt && chld > 0){
        nodeSum <- nodeSum + NodeCheckSum(node$children[[chld]])
      }
    }
    
  }
  
  nodeSum
  
}


test2Ans <- NodeCheckSum(testTree)
TEST2CHK <- "66"

if(test2Ans != TEST2CHK){
  stop("Incorrect root check sum for for test case. ",
       sprintf("Result is %s instead of %s", test2Ans, TEST2CHK),
       call. = FALSE)
} else {
  message("Test 2 Passed")
}

print(sprintf("Answer for part 2: %s", NodeCheckSum(dataTree)))