library(purrr)
library(stringr)
library(magrittr)
library(dplyr)
library(ggplot2)

#### Part 1 ----

testData <- c("1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9") %>%
  str_split_fixed(", ", n = 2) %>%
  tibble::as.tibble() %>%
  map_dfc(as.integer) %>%
  set_names(c("x", "y"))

data <- readr::read_csv("Input06.txt", col_names = c("x", "y"),
                        col_types = "ii")

# Find distance from points x to line
lineDist <- function(p, lPar){
  
  map2(p$x, p$y, ~abs(lPar$k + lPar$m*.x - .y)/sqrt(1 + lPar$m^2)) %>%
    unlist()
  
}

# Find Manhattan Distance from point to set of 
ManDist <- function(p1, p2){
    
  data.frame(abs(p1$x - p2$x), abs(p1$y - p2$y)) %>%
           apply(1, sum)
  
}

# F closest neighbor to point p in list lst
FindPointNN <- function(p, lst){
  
  dist <- ManDist(p, lst) 
  which(dist == min(dist))
  
}

# find nearest neighbor(s) to a line
FindLineNN <- function(p, lPar) {
  
  if(NROW(p) == 0){
    return(data.frame(x = NULL, y = NULL))
  }
  
  lDist <- lineDist(p, lPar)
  # print(lDist)
  p[lDist == min(lDist), ]
  
}

# finds next point on the boundary
FindNextBoudaryPt <- function(curPt, data, direction, slope){

  if(direction == 'x'){

    if(slope > 0){
      
      ssData <- filter(data,  y > x + (curPt$y - curPt$x)) %>%
        FindLineNN(list(m = -slope, k = curPt$y + curPt$x)) %>%
        filter(x == min(x))
      
    } else {
      
      ssData <- filter(data,  y < x + (curPt$y - curPt$x)) %>%
        FindLineNN(list(m = slope, k = curPt$y + curPt$x)) %>%
        filter(x == max(x))
      
    }

  } else{
    
    if(slope > 0){
      
      ssData <- filter(data,  y > -x + (curPt$y + curPt$x)) %>%
        FindLineNN(list(m = slope, k = curPt$y - curPt$x)) %>%
        filter(y == max(y))
      
    } else {
      
      ssData <- filter(data,  y < -x + (curPt$y + curPt$x)) %>%
        FindLineNN(list(m = -slope, k = curPt$y - curPt$x)) %>%
        filter(y == min(y))
    }
    
  }
  
  ssData
  
}

# Traces the sides of points that will have infinite areas
TraceSide <- function(strt, input, direction, slope) {
  
  cont <- TRUE
  i <- 1
  nxtPt <- strt
  side <- strt
  
  while(cont && i < 100){
    
    nxtPt <- FindNextBoudaryPt(nxtPt, input, direction, slope)
    
    if(NROW(nxtPt) == 0){
      cont <- FALSE
    } else if(NROW(nxtPt) ==1){
      side <- rbind(side, nxtPt)
    } else {
      stop("FindNextBoundaryPt returned more than one next point")
    }
    
    i <- i + 1
  }
  
  side
  
}

# returns bounds for graph
GetBounds <- function(input, buf = c(5,5)){
  
  map(input, range)
  
}

# determines which point is nearest for 
AssignNearest <- function(input, grid){
  
  ptCt <- NROW(input)
  
  map2(grid[[1]], grid[[2]], 
       ~FindPointNN(list(x = .x, y = .y), input)) %>%
    # if tied for min distance, mark as ptCt + 1
    map_dbl(~ifelse(length(.x) > 1, ptCt + 1, .x))  
  
}

# returns boundary/infinite area points
ExcludePoints <- function(input){
  
  strt <- arrange(input, x, y)
  
  s1 <- TraceSide(strt[1,], input, "x", 1)
  s2 <- TraceSide(tail(s1,1), input, "y", 1)
  s3 <- TraceSide(tail(s2,1), input, "x", -1)
  s4 <- TraceSide(tail(s3,1), input, "y", -1)
  
  rbind(s1, s2, s3, s4) %>% distinct()
  
}

# Plots boundary, connecting points with infinite area
PlotBoundary <- function(pts, bPts){
  
  ggplot(pts, aes(x,y)) + 
    geom_point() +
    geom_path(data = rbind(bPts, bPts[1,])) +
    coord_equal() + theme_minimal() 

}

# Builds Points List for grid
BuildGrid <- function(bounds){
  
  x <- bounds$x
  y <- bounds$y

  expand.grid(seq(x[1], x[2]), seq(y[1], y[2])) %>%
    set_names(c("x", "y"))
  
}

# Determines area for point for which there is max safe distance
GetMaxSafeArea <- function(input){
  
  grd <-GetBounds(input) %>%
    BuildGrid()
  
  # equidistant points are marked by nrow(input) + 1
  exclPts <- ExcludePoints(input)
  
  PlotBoundary(input, exclPts) %>% print()
  
  gridNear <- AssignNearest(input, grd)
  
  gridNear[gridNear <= NROW(input)] %>%
    table() %>%
    data.frame(input$x, input$y) %>%
    set_names(c("index", "area", "x", "y")) %>%
    anti_join(exclPts, by = c("x", "y")) %>%
    pluck("area") %>%
    max()

}

test1Ans <- GetMaxSafeArea(testData)
TEST1CHK <- 17

if(test1Ans != TEST1CHK){
  stop("Incorrect maximum safe distance for test case. ",
       sprintf("Result is %s instead of %s", test1Ans, TEST1CHK),
       call. = FALSE)
} else {
  message("Test 1 Passed")
}

print(sprintf("Answer for part 1: %i", GetMaxSafeArea(data)))

#### Part 2 ----

# Calculate the distance from grid points to all input points
CalcRegionDistToPoints <- function(input, grd){
  
  grd <-GetBounds(input) %>%
    BuildGrid()
  
  map2(grd[[1]], grd[[2]], 
       ~ManDist(list(x = .x, y = .y), input) %>%
         sum())
  
}

CalcCloseRegionArea <- function(input, distLimit){
  
  grd <-GetBounds(input) %>%
    BuildGrid()
  
  totDists <- CalcRegionDistToPoints(input, grd)
  
  sum(totDists < distLimit)
}

test2Ans <- CalcCloseRegionArea(testData, 32)
TEST2CHK <- 16

if(test2Ans != TEST2CHK){
  stop("Incorrect region area for test case. ",
       sprintf("Result is %s instead of %s", test2Ans, TEST2CHK),
       call. = FALSE)
} else {
  message("Test 2 Passed")
}

print(sprintf("Answer for part 2: %i", CalcCloseRegionArea(data, 10000)))
