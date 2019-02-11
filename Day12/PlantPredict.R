library(dplyr)
library(purrr)
library(stats)
library(stringr)
library(tidyr)
library(zoo)

source("Input12.R")

testInit <- "#..#.#..##......###...###"
testKey <- c("...## => #",
             "..#.. => #",
             ".#... => #",
             ".#.#. => #",
             ".#.## => #",
             ".##.. => #",
             ".#### => #",
             "#.#.# => #",
             "#.### => #",
             "##.#. => #",
             "##.## => #",
             "###.. => #",
             "###.# => #",
             "####. => #") 

#### Part 1 ----

# Transforms ./# notation to bit char
SymDecode <- function(x){
  
  str_replace_all(x, c("#" = "1", "\\." = "0")) 
    
}

# Transforms bit char to integer
DecodeBits <- function(x){
  
  x %>%
    str_pad(width = 32, side = "left", pad = "0") %>%
    str_split_fixed("", n = 32) %>%
    as.integer() %>%
    as.logical() %>%
    rev() %>%
    packBits(type = "integer")
  
}

# function takes key and creates a join table for next generation
DecodeKey <-function(key){
  
  dKey <- key %>%
    str_split_fixed(" => ", n = 2) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    set_names(nm = c("currentGen", "nextGen")) %>%
    mutate(currentGen = map_int(currentGen, ~SymDecode(.x) %>% DecodeBits()),
           nextGen = ifelse(nextGen == "#", "1", "0"))
  
  
  if(NROW(dKey) < 32){
    # fill in missing entries to complete table
    dKey <- data.frame(currentGen = 0:31) %>%
      left_join(dKey, by = "currentGen") %>%
      replace_na(list(nextGen = "0"))
    
  }
  
  dKey
  
}

# Function takes an initial state and returns the next gen based on the key
# curState is a character vector of 1 and 0
# next gen will have additional point on each end
PredictNextGen <- function(curState, key){
  
  # pad to be able to predict growth
  c(rep("0", 3), curState, rep("0", 3)) %>%
    rollapply(width = 5, str_c, collapse = "") %>%
    map_int(DecodeBits) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    set_names("currentGen") %>%
    left_join(key, by = "currentGen") %>%
    pluck("nextGen")
  
}

# takes initial state, key, and predicts next n generations
ForecastNGen <-function(stateRaw, keyRaw, n){
  
  startState <- SymDecode(stateRaw) %>%
    strsplit("") %>%
    unlist()
  
  dKey <- DecodeKey(keyRaw)
  
  accumulate(seq(n), ~PredictNextGen(.x, dKey), .init = startState)
  
}

# Function Prints Generation 
PrintGen <- function(gen, zPt, minNeg, maxPos){
  
  gen %>%
    tail(if(1-zPt - minNeg >= 0) length(gen) else 1-zPt - minNeg) %>%
    c(rep("0", 
          if((maxPos - (length(gen) - zPt + 1)) > 0) {
            maxPos - (length(gen) - zPt + 1)
          } else {
            0
          })
      ) %>% 
    head(maxPos - minNeg) %>%
    as.numeric() %>%
    symnum(cutpoints = -1:1, symbols = c(".", "#")) %>%
    paste0(collapse = "")
           
  
}

# Takes a generation and sums the pot numbers with plants
SumGen <- function(gen, genNum){
  
  sum(which(gen == 1) - (genNum + 1))
  
}

test1Gen <- ForecastNGen(testInit, testKey, 20)
test1Ans <- SumGen(test1Gen[[21]], 20)
TEST1CHK <- 325


if(all(unlist(test1Ans) != unlist(TEST1CHK))){
  stop("Incorrect check sum for for test case. ",
       sprintf("Result is %s instead of %s", test1Ans, TEST1CHK),
       call. = FALSE)
} else {
  message("Test 1 Passed")
  map2_chr(test1Gen, seq(1, 21), PrintGen, minNeg = -3, maxPos = 35 ) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>%
    walk(~message(paste0(.x, "\n")))
}

plantGen20 <- ForecastNGen(si, key, 20)

print(sprintf("Answer for part 1: %s", 
              SumGen(plantGen20[[21]], 20 )
              )
      )

#### Part 2 ----

# Prints Multi Generations
PrintMultiGen <- function(multiGen){
  
  gens <- NROW(multiGen) - 1
  maxP <- length(tail(multiGen, 1, 1)[[1]]) - gens
  
  map2_chr(multiGen, seq_along(multiGen), 
           PrintGen, minNeg = -3, maxPos = maxP ) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>%
    walk(~message(paste0(.x, "\n")))
  
}

# Pick large generation to see if 
plantGen125 <- ForecastNGen(si, key, 125)
gen125Sum <- SumGen(plantGen125[[126]], 125)

trackIncrease <- map2_dbl(plantGen125, 
                          seq_along(plantGen125) - 1, 
                          ~SumGen(.x, .y)) %>% 
  diff()

plot(1:125, trackIncrease, type = "l") # increase becomes constant @ Gen 99

increPerGen <- tail(trackIncrease, 1)

gen50BSum <- (50e9 - 125) * increPerGen + gen125Sum

print(sprintf("Answer for part 2: %s", gen50BSum))

