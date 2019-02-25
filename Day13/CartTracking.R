library(stringr)
library(R6)
library(magrittr)
library(purrr)

testData <- readLines("Sample13.txt")
inData <- readLines("Input13.txt")

# look ups for definining movements
TURN_ORDER <- c("left", "straight", "right")
DIR_MOVES <- c("^" = c(-1, 0), "v" = c(1, 0),
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
  class = FALSE,
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
    Advance = function(){
      
      # identify the track type cart is currently on
      curTile <- private$course[private$coord[['row']], private$coord[['col']]]
      
      # determine if track is corner or intersection and adjust direction
      if(curTile %in% c("\\", "/")) {
        private$RoundCorner()
      } else if(curTile == "+"){
        private$PassIntersection()
      }
      
      # move coordinates based on current direction of travel
      private$coord <- private$coord + DIR_MOVES[[private$dir]]
      
    }
  ),
  
  private = list(
    coord = NULL,
    course = NULL,
    dir = NULL,
    turns = 0,
    PassIntersection <- function(){
      
      # determine which way it should turn and adjust direction
      turn <- TURN_ORDER[private$turns + 1]
      private$dir <- TURN_TRANSFORM[[c(private$dir, turn)]]
      
      # update turn counter
      private$turns <- (private$dir + 1) %% length(TURN_ORDER)
      
    },
    RoundCorner <- function(type){
      
      # determine which way to turn and update direction
      turn <- CORNER_TURNS[[c(private$dir, type)]]
      private$dir <- TURN_TRANSFORM[[c(private$dir, turn)]]
      
    }
  )
                
)