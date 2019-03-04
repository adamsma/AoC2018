library(stringr)
library(R6)
library(magrittr)
library(purrr)

testData <- readLines("Sample13.txt")
inData <- readLines("Input13.txt")

# look ups for definining movements
TURN_ORDER <- c("left", "straight", "right")
DIR_MOVES <- list("^" = c(-1, 0), "v" = c(1, 0),
               "<" = c(0, -1), ">" = c(0, 1))
TURN_TRANSFORM <- list(
  "^" = c("left" = "<", "straight" = "^", "right" = ">"), 
  "v" = c("left" = ">", "straight" = "v", "right" = "<"), 
  "<" = c("left" = "v", "straight" = "<", "right" = "^"), 
  ">" = c("left" = "^", "straight" = ">", "right" = "v")
)
CORNER_TURNS <- list(
  "^" = c( "\\" = "left", "/" = "right"),
  "v" = c("\\" = "left", "/" = "right"),
  "<" = c("/" = "left", "\\" = "right"),
  ">" = c("/" = "left", "\\" = "right")
)

#### Part 1 ----

# Generates matrix for the course
GenCourse <- function(input){
  
  trackWidth <- max(str_length(input))
  
  # create matrix from input for track sections
  str_split_fixed(input, pattern = "", n = trackWidth) %>%
    # replace carts with track piece, assume no corners or intersections
    str_replace_all(c("\\^|v" = "|", "<|>" = "-")) %>%
    matrix(ncol = trackWidth)
  
}

# Extracts data framecart locations and symbols
ExtractCarts <- function(input){
  
  trackWidth <- max(str_length(input))
  
  # create matrix from input for track sections
  rawCourse <- str_split_fixed(input, "", n = trackWidth)
  
  # identify where the carts are on the map
  map_dfr(c(">" = ">", "<" = "<", "v" = "v", "^" = "^"), 
          ~as.data.frame(which(rawCourse == .x, arr.ind = TRUE))) %>%
    transpose() %>%
    # look up the sympol at that space
    map_dfr(~data.frame(row = .x$row, col = .x$col, 
                        symb = rawCourse[.x$row, .x$col],
                        stringsAsFactors = FALSE))
  
}

# Creates Cart class to track location
Cart <- R6Class("Cart",
  class = TRUE,
  portable = FALSE,
  cloneable = FALSE,
  
  public = list(
    initialize = function(row, col, initDir, course){
      private$coord <- c(row = row, col = col)
      private$dir <- initDir
      private$course <- course
    },
    GetPosition = function() private$coord,
    GetDirection = function() private$dir,
    ShowPosition = function(addCarts = NULL, radius = 7){

      cPos <- private$coord
      pCourse <- private$course
      courseDims <- dim(pCourse)

      # determine printing box bounds
      rMin <- max(cPos[1] - radius, 1)
      rMax <- min(cPos[1] + radius, max(courseDims[1]))
      
      cMin <- max(cPos[2] - radius, 1) #1
      cMax <-  min(cPos[2] + radius, max(courseDims[2])) #courseDims[2]

      # mark cart on course with direction indicator
      pCourse[cPos[1], cPos[2]] <- private$dir
      
      # include additional carts provided
      if(!is.null(addCarts)){
        
        map(addCarts,
            function(x) {
              rc <- x$GetPosition()
              
              # check to see if spot is occupied and mark as collision if so
              if(pCourse[rc[1], rc[2]] %in% c("v", "^", "<", ">", "X")){
                pCourse[rc[1], rc[2]] <<- "X"
              } else{
                pCourse[rc[1], rc[2]] <<- x$GetDirection()
              }
            })

      }
      
      for(i in seq(rMin, rMax)) writeLines(paste0(pCourse[i, cMin:cMax], 
                                                  collapse = ""))

    },
    print = function(...) self$ShowPosition(),
    Move = function(moves = 1, show = FALSE, addCarts = NULL) {
      if(moves >= 1) {
        for(i in seq(moves)) {
          private$Advance()
          if(show) self$ShowPosition(addCarts = addCarts)
        }
      }
      invisible(private$coord)
    }

  ),

  private = list(
    coord = NULL,
    course = NULL,
    dir = NULL,
    turns = 0,
    PassIntersection = function(){

      # determine which way it should turn and adjust direction
      turn <- TURN_ORDER[private$turns + 1]
      
      private$dir <- TURN_TRANSFORM[[c(private$dir, turn)]]

      # update turn counter
      
      private$turns <- (private$turns + 1) %% length(TURN_ORDER)

    },
    RoundCorner = function(type){

      # determine which way to turn and update direction
      turn <- CORNER_TURNS[[c(private$dir, type)]]

      private$dir <- TURN_TRANSFORM[[c(private$dir, turn)]]

    },
    Advance = function(showLoc = FALSE){

      # identify the track type cart is currently on
      curTile <- private$course[private$coord[['row']], private$coord[['col']]]

      # determine if track is corner or intersection and adjust direction
      if(curTile %in% c("\\", "/")) {
        private$RoundCorner(curTile)
      } else if(curTile == "+"){
        private$PassIntersection()
      }

      # move coordinates based on current direction of travel
      private$coord <- private$coord + DIR_MOVES[[private$dir]]
      
      # print course with cart if so specified
      if(showLoc) self$ShowPosition()
      
      invisible(private$coord)
      
    }
  )
)

# Function to calculate distance among carts
# Takes list a of carts and returns dist object
dist.cart <- function(carts) {
  
  map(carts, ~.x$GetPosition()) %>% 
    unlist() %>% 
    matrix(ncol = 2, byrow = TRUE) %>% 
    dist(method = "manhattan")
  
}

# Function that Runs carts and determines first crash
# takes as input course map with locations of carts marked
RunCarts <- function(input, max.moves = 1000, verbose = FALSE) {
  
  # generate data to initialize cart objects
  course <- GenCourse(input)
  cartInit <- ExtractCarts(input)
  
  # initialize list of carts
  theCarts <- transpose(cartInit) %>%
    map(~Cart$new(.x$row, .x$col, .x$symb, course))
  
  # determine distances 
  minDist <- min(dist.cart(theCarts))
  i <- 0
  
  while(minDist > 0 && i < max.moves){
    
    # if a set of carts are close, run individual moves
    # checking for collisions after each move
    # otherwise rapidly advance
    if(minDist <= 2) {
      
      # when close move order matters
      # gather locations to use as ordering
      cLocs <- map_dfr(theCarts, function(x) {
        loc <- x$GetPosition() 
        data.frame(row = loc[1], col = loc[2])}
      )
      
      for(j in order(cLocs[1], cLocs[2])) {
        
        theCarts[[j]]$Move()
        minDist <- min(dist.cart(theCarts))
        
        if(minDist == 0) break
        
      }
    } else{

      walk(theCarts, ~.x$Move(minDist/2))
      minDist <- min(dist.cart(theCarts))

    }
    
    i <- i + 1
  }
  
  if(i == max.moves) stop("No collision detected after max moves")

  # determine index of one of the carts in the collision
  # transform to matrix to use arr.ind argument of which
  # but first must transform diag to non zero values
  # since dist matrix is symmetric, take first row
  distMat <- as.matrix(dist.cart(theCarts))
  diag(distMat) <- 100
  collideCart <- which(distMat == 0, arr.ind = TRUE)[1,1]
 
  # show collision
  if(verbose) theCarts[[collideCart]]$ShowPosition(addCarts = theCarts)
 
  # return coordinates of collision in col, row with 0 based index
  rev(theCarts[[collideCart]]$GetPosition()  - 1) 
    
}

test1Ans <- RunCarts(testData)
TEST1CHK <- c(7, 3)

if(paste(test1Ans, collapse = ",") != paste(TEST1CHK, collapse = ",")){
  stop("Incorrect collision location for test case. ",
       sprintf("Result is %s instead of %s",
               paste(test1Ans, collapse = ","),
               paste(TEST1CHK, collapse = ",")),
       call. = FALSE)
} else {
  message("Test 1 Passed")
}

collideLoc <- RunCarts(inData)
print(sprintf("Answer for part 1: %s", paste(collideLoc, collapse = ",")))

cc <- GenCourse(inData)
cI <- ExtractCarts(inData)
cL <- transpose(cI) %>%
  map(~Cart$new(.x$row, .x$col, .x$symb, cc))


#### Part 2 ----

testData2 <- readLines("Sample13-2.txt")

# 17 Carts Enter, 1 Cart Leaves!
ThunderDome<- function(input, max.moves = 1000, verbose = FALSE) {
  
  # generate data to initialize cart objects
  course <- GenCourse(input)
  cartInit <- ExtractCarts(input)
  
  # initialize list of carts
  theCarts <- transpose(cartInit) %>%
    map(~Cart$new(.x$row, .x$col, .x$symb, course))
  
  while(length(theCarts) > 1){

    xCarts <- BoomBoomCarts(theCarts, max.moves)
    if(verbose) walk(xCarts, ~print(paste("See Ya in Hell Cart", .x)))
    theCarts <- theCarts[!(seq_along(theCarts) %in% xCarts)]
    
  }
  
  if(verbose) theCarts[[1]]$ShowPosition()
  theCarts[[1]]
  
}

# Function takes list of carts and runs them until a collision happens
# Returns indices of carts that collided
BoomBoomCarts <- function(theCarts, max.moves){
  
  # determine distances 
  minDist <- min(dist.cart(theCarts))
  i <- 0
  
  # wrecked carts need to be excluded as soon as possible 
  # but the rest of the carts need to be advanced
  wreckedCarts <- integer(0)
  
  while((length(wreckedCarts) == 0) && i < max.moves){
    
    # if a set of carts are close, run individual moves
    # checking for collisions after each move
    # otherwise rapidly advance
    if(minDist <= 2) {
      
      # when close move order matters
      # gather locations to use as ordering
      cLocs <- map_dfr(theCarts, function(x) {
        loc <- x$GetPosition() 
        data.frame(row = loc[1], col = loc[2])}
      )
      
      moveOrd <- order(cLocs[1], cLocs[2])
      
      for(j in moveOrd) {
        
        if(!(j %in% wreckedCarts)){
          
          theCarts[[j]]$Move()
          
          # only check distances for carts not wrecked already
          # check all if no wrecked carts yet
          if(length(wreckedCarts)) {
            
            # if only one non-wrecked cart, min dist is inf
            noWreck <- theCarts[-wreckedCarts]
            minDist <- ifelse(length(noWreck) == 1, 
                              Inf, 
                              min(dist.cart(noWreck)))
            
          } else {
            
            minDist <- min(dist.cart(theCarts))
          }
          
          if(minDist == 0) {
            
            # determine index of one of the carts in the collision
            # transform to matrix to use arr.ind argument of which
            # but first must transform diag to non zero values
            # want all carts with collisions now to get correct indices
            distMat <- as.matrix(dist.cart(theCarts))
            diag(distMat) <- 100
            wreckedCarts <- which(distMat == 0, arr.ind = TRUE) %>%
              as.vector() %>%
              unique()
            
          }
        }
      }
    } else {
      
      walk(theCarts, ~.x$Move(minDist/2))
      minDist <- min(dist.cart(theCarts))
      
      if(minDist == 0) {
        
        # determine index of one of the carts in the collision
        # transform to matrix to use arr.ind argument of which
        # but first must transform diag to non zero values
        # want all carts with collisions now to get correct indices
        distMat <- as.matrix(dist.cart(theCarts))
        diag(distMat) <- 100
        wreckedCarts <- which(distMat == 0, arr.ind = TRUE) %>%
          as.vector() %>%
          unique()
        
      }
    }
    
    i <- i + 1
  }
  
  if(i == max.moves) stop("No collision detected after max moves")
  
  wreckedCarts

}


test2Ans <- rev(ThunderDome(testData2)$GetPosition() - 1) 
TEST2CHK <- c(6, 4)

if(paste(test2Ans, collapse = ",") != paste(TEST2CHK, collapse = ",")){
  stop("Incorrect collision location for test case. ",
       sprintf("Result is %s instead of %s",
               paste(test2Ans, collapse = ","),
               paste(TEST2CHK, collapse = ",")),
       call. = FALSE)
} else {
  message("Test 2 Passed")
}

lastCart <- ThunderDome(inData)
print(sprintf("Answer for part 2: %s",
              paste(rev(lastCart$GetPosition() - 1), 
                    collapse = ",")))