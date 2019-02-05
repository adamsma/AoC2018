testSN <- c(18, 42)
TEST1CHK <- list(c(33, 45), c(21, 61))
testPow <- c(29, 30)

gridSN <- 7511

#### Part 1 ----

# calculates the grid of power levels for a given serial number
PowerLevels <- function(sn, gridSize = 300){
  
  X <- matrix(seq(gridSize), ncol = gridSize)
  Y <- matrix(seq(gridSize), nrow = gridSize)
  
  rID <- X + 10
  rIDGrid <- matrix(rep(rID[1,seq(gridSize)], gridSize), 
                    ncol = gridSize, byrow = TRUE)
  
  ((((Y %*% rID + sn) * rIDGrid) %% 1000) %/% 100) - 5
  
}

# Function takes power grid and returns 3x3 block starting with x, y coords.
Block <- function(pow, x, y, n = 3){
  
  if(x < 0 || y < 0) return(NULL)
  
  yrng <- seq(y, y + n - 1)
  xrng <- seq(x, x + n - 1)
  
  pow[yrng, xrng]
  
}

# Function returns total power for a 3x3 block starting with x, y coords
TotalPower <- function(pow, x, y, block_size = 3){
  
  sum(Block(pow, x, y, block_size))
  
}

# Determines what the max power output is for a power grid
MaxPower <- function(pg, block_size = 3){
  
  size <- NROW(pg)
  pTrack <- matrix(numeric(size^2), nrow = size)
  
  for(i in seq(size - block_size + 1)){
    for(j in seq(size - block_size + 1)){
      pTrack[i, j] <- TotalPower(pg, j, i, block_size)
    }
  }
  
  pMax <- max(pTrack)

  rcInd <- which(pTrack == pMax, arr.ind = TRUE)
  
  list(power = as.integer(pMax), 
       coords = as.integer(c(rcInd[2], rcInd[1])))
  
}

# Determines indices (x,y) of max power for given serial number
which.MaxPower <- function(sn, size = 300){
  
  pg <- PowerLevels(sn, size)
  
  pMax <- MaxPower(pg)
  
  pMax$coords
  
}


test1Ans <- map(testSN, which.MaxPower)


if(all(unlist(test1Ans) != unlist(TEST1CHK))){
  stop("Incorrect check sum for for test case. ",
       sprintf("Result is %s instead of %s", test1Ans, TEST1CHK),
       call. = FALSE)
} else {
  message("Test 1 Passed")
}

gridMaxCoor <- which.MaxPower(gridSN)
print(sprintf("Answer for part 1: %s",gridMaxCoor))

#### Part 2 ----
# Function returns max power based on variable grid size
which.MoreMaxPower <- function(sn, max_size = 300, grid = 300, status = FALSE){
  
  pg <- PowerLevels(sn, grid)
  sizes <-seq(max_size)
  
  pMax <- map(sizes, list())
  
  # set up progess bar if needed
  if(status){
    pb <- txtProgressBar(min = 0, max = max_size, style = 3)
  }
  
  for(i in sizes) {
    
    pMax[[i]] <- MaxPower(pg, i)
    
    if(status && (i %% 5 == 0)) setTxtProgressBar(pb, i)
    
  }
  
  
  mInd <- which.max(map_int(pMax, "power"))
  
  c(pMax[[mInd]]$coords, mInd)
  
}


TEST2CHK <- list(c(90, 269, 16), c(232, 251, 12))
test2Ans <- map(testSN, which.MoreMaxPower, max_size = 20, status = TRUE)


if(all(unlist(test2Ans) != unlist(TEST2CHK))){
  stop("Incorrect check sum for for test case 2. ",
       sprintf("Result is %s instead of %s", test2Ans, TEST2CHK),
       call. = FALSE)
} else {
  message("Test 2 Passed")
}

moreGridMax <- which.MoreMaxPower(gridSN, status = TRUE)
print(sprintf("Answer for part 2: %s,%s,%s", 
              moreGridMax[1], moreGridMax[2], moreGridMax[3]))
