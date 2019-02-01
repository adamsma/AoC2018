library(purrr)
library(microbenchmark)

testData <- data.frame(players = as.integer(c(10, 13, 17, 21, 30)), 
                       maxMarble = as.integer(c(1618, 7999, 1104, 6111, 5807)))

data <- as.integer(c(458, 72019))

#### Part 1 ----

# inserts number (x) into a vector (vec) 
# at given index (i) and shifts right
# drops last value
# optional arguement l is length of vec
InsertShiftRight <- function(vec, i, x, l = NULL) {
  
  if(is.null(l)){
    l <- length(vec)
  }
  
  vec[(i+1):l] <- vec[i:(l-1)]
  vec[i] <- x
  vec
  
}

# pops value at index (i) and shifts vector (vec) left
# optional arguement l is length of vec
PopShiftLeft <- function(vec, i, l = NULL){
  
  if(is.null(l)){
    l <- length(vec)
  }
  
  if(i != l){
    vec[i:(l-1)] <- vec[(i+1):l]
  }
  
  vec[l] <- 0
  vec
  
}

# modulus division, if zero, value is the modulus
ClockNZ <- function(x, modul){
  
  val <- x %% modul
  val <- ifelse(val == 0, modul, val)
  
}

# simulates the game and returns the scores of all the players
PlayGame <- function(players, maxMarble, 
                     show_moves= FALSE, show_progress = FALSE){
  
  # initialize board and scores
  board <- integer(maxMarble + 1) # add one for marble 0
  # board[2:3] <- c(2,1) # make large enough board
  scores <- map(seq(players), ~list(totScore = integer(1),
                                    autoX = integer(0),
                                    rmX = integer(0))
                )
  
  # initialize current marbel index and board leng
  curId <- 1
  curLen <- 1
  
  if(show_moves) print(board[seq(1)])
  
  for(x in seq(1, maxMarble)){
    
    if(x %% 23 == 0){
      # when divisible by 23 score points
      
      # determine whose move it is
      curPlayer <- ClockNZ(x, players)
      
      # determine position of marble to remove
      rmMar <- ClockNZ(curId - 7, curLen)
      
      # add score
      scores[[curPlayer]]$autoX <- c(scores[[curPlayer]]$autoX, x)
      scores[[curPlayer]]$rmX <-  c(scores[[curPlayer]]$rmX, board[rmMar])
      scores[[curPlayer]]$totScore <- sum(scores[[curPlayer]]$autoX) +
        sum(scores[[curPlayer]]$rmX)
      
      # update board  
      board <- PopShiftLeft(board, rmMar, curLen)
      
      # adjust current marble and length
      curLen <- curLen - 1 
      curId <- rmMar
      
    } else {
      
      # find index of marble one away from current
      # board will have x stones at this point
      oneAway <- ClockNZ(curId + 1, curLen)
      
      # find index of move
      # board will have + 1 stones now
      curLen <- curLen + 1
      move <- ClockNZ(oneAway + 1, curLen)
      
      # insert into board
      board <- InsertShiftRight(board, move, x, curLen)
      curId <- move
      
    }
    
    if(show_moves) {
      # boardLen <- (x + 1) - 2 * (x %/% 23)
      print( c(x, board[seq(min(curLen, 50))]) )
    }
   
    if(((x %% 1000) == 0) && show_progress) print(x)
     
  }
  
  scores
    
}

# returns the max score achieved for a simulated game
MaxScore <- function(players, maxMarble, 
                     ...){
  
  PlayGame(players, maxMarble, ...) %>%
    map_dbl("totScore") %>%
    max() %>%
    as.integer()
  
}

test1Ans <- map2_int(testData$players, testData$maxMarble, MaxScore)
TEST1CHK <- c(8317, 146373, 2764, 54718, 37305)

if(any(test1Ans != TEST1CHK)){
  stop("Incorrect scores for for test case. ",
       sprintf("Result is [%s] instead of [%s]",
               paste(test1Ans, collapse = ", "),
               paste(TEST1CHK, collapse = ", ")
              ),
       call. = FALSE)
} else {
  message("Test 1 Passed")
}

print(sprintf("Answer for part 1: %s", MaxScore(data[1], data[2])))


#### Part 2 ----

ScoreMoves <- function(maxMarble, show_moves= FALSE, show_progress = FALSE){
  
  # initialize board and scores
  board <- integer(maxMarble + 1) # add one for marble 0
  
  scoreMar <- integer(maxMarble %/% 23)
  
  # initialize current marbel index and board leng
  curId <- 1
  curLen <- 1
  curScores <- 0
  
  if(show_moves) print(board[seq(1)])
  
  for(x in seq(1, maxMarble)){
    
    if(x %% 23 == 0){
      # when divisible by 23 score points
      curScores <- curScores + 1
      
      # determine position of marble to remove
      rmMar <- ClockNZ(curId - 7, curLen)
      
      # add score
      scoreMar[curScores] <- board[rmMar]
      
      # update board  
      board <- PopShiftLeft(board, rmMar, curLen)
      
      # adjust current marble and length
      curLen <- curLen - 1 
      curId <- rmMar 
      
    } else {
      
      # find index of marble one away from current
      # board will have x stones at this point
      oneAway <- ClockNZ(curId + 1, curLen)
      
      # find index of move
      # board will have + 1 stones now
      curLen <- curLen + 1
      move <- ClockNZ(oneAway + 1, curLen)
      
      # insert into board
      board <- InsertShiftRight(board, move, x, curLen)
      curId <- move
      
    }
    
    if(show_moves) {
      print( c(x, board[seq(min(curLen, 50))]) )
    }
    
    if(((x %% 50000) == 0) && show_progress) print(x)
    
  }
  
  data.frame(move = seq(from = 23, to = maxMarble, by = 23), 
             exScore = scoreMar)
  
}

# alternative simulation that takes generalized look up table and parses
# it among the players; for speed can take cached play list
PlayGame2 <- function(players, maxMarbles, playList = NULL, ...){
  
  if(is.null(playList) || (max(playList$move) < maxMarbles)){
    playList <- ScoreMoves(maxMarbles, ...)
  } else {
    playList <- dplyr::filter(playList, move <= maxMarbles)
  }
  
  playList %>%
    mutate(player = ClockNZ(move, players)) %>%
    group_by(player) %>%
    summarize(totScore = sum(move + exScore))
  
}

# Alternative function   utilizing PlayGame2
MaxScore2 <- function(players, maxMarbles, playList = NULL, ...){
  
  PlayGame2(players, maxMarbles, playList, ...) %>%
    select(totScore) %>%
    max() #%>%
    # as.integer()
  
}

pL <- ScoreMoves(8e6, show_progress = TRUE) # took ~4 days to run
mS100 <- MaxScore2(data[1], data[2]*100, pL)

test2Ans <- map2(testData$players, testData$maxMarble, MaxScore2, pL)
TEST2CHK <- c(8317, 146373, 2764, 54718, 37305)

if(any(test2Ans != TEST2CHK)){
  stop("Incorrect scores for for test case 2. ",
       sprintf("Result is [%s] instead of [%s]",
               paste(test1Ans, collapse = ", "),
               paste(TEST1CHK, collapse = ", ")
       ),
       call. = FALSE)
} else {
  message("Test 2 Passed")
}

print(sprintf("Answer for part 2: %s", ms100))


#### benchmarking ----
microbenchmark(
  MaxScore(13, 7999), 
  # MaxScore2(13, 7999),
  MaxScore2(13, 7999, pL),
  times = 50)