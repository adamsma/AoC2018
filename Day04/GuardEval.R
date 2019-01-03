library(stringr)
library(purrr)

testData <- c("[1518-11-01 00:00] Guard #10 begins shift",
              "[1518-11-01 00:05] falls asleep",
              "[1518-11-01 00:25] wakes up",
              "[1518-11-01 00:30] falls asleep",
              "[1518-11-01 00:55] wakes up",
              "[1518-11-01 23:58] Guard #99 begins shift",
              "[1518-11-02 00:40] falls asleep",
              "[1518-11-02 00:50] wakes up",
              "[1518-11-03 00:05] Guard #10 begins shift",
              "[1518-11-03 00:24] falls asleep",
              "[1518-11-03 00:29] wakes up",
              "[1518-11-04 00:02] Guard #99 begins shift",
              "[1518-11-04 00:36] falls asleep",
              "[1518-11-04 00:46] wakes up",
              "[1518-11-05 00:03] Guard #99 begins shift",
              "[1518-11-05 00:45] falls asleep",
              "[1518-11-05 00:55] wakes up")

testScrambled <- c("[1518-11-04 00:46] wakes up",
                   "[1518-11-01 23:58] Guard #99 begins shift",
                   "[1518-11-01 00:00] Guard #10 begins shift",
                   "[1518-11-05 00:03] Guard #99 begins shift",
                   "[1518-11-01 00:05] falls asleep",
                   "[1518-11-05 00:45] falls asleep",
                   "[1518-11-01 00:25] wakes up",
                   "[1518-11-03 00:29] wakes up",
                   "[1518-11-01 00:30] falls asleep",
                   "[1518-11-01 00:55] wakes up",
                   "[1518-11-03 00:05] Guard #10 begins shift",
                   "[1518-11-02 00:40] falls asleep",
                   "[1518-11-02 00:50] wakes up",
                   "[1518-11-03 00:24] falls asleep",
                   "[1518-11-05 00:55] wakes up",
                   "[1518-11-04 00:02] Guard #99 begins shift",
                   "[1518-11-04 00:36] falls asleep"
                   ) %>%
  str_sort()

data <- readr::read_lines("Input04.txt") %>%
  str_sort()

#### Part 1 ----

GetGuardList <- function(input){
  
  input %>%
    str_subset("(?<=#)[0-9]+") %>%
    str_extract("(?<=#)[0-9]+") %>%
    unique()
  
}

GenTrackList <- function(guardList){
  
  map(guardList, function(.x) rep(0,60)) %>%
    set_names(guardList)
  
}

is.ShiftStart <- function(entry){
  str_detect(entry,"(?<=#)[0-9]+")
}

ExtractMinute <- function(entry){
  str_extract(entry, "(?<=:)[0-9]{2}(?=])") %>%
    as.numeric()
}

ExtractGuard <- function(entry){
  str_extract(entry,"(?<=#)[0-9]+")
}

ParseSleepLog <- function(trackList, input){

  i <- 1
  
  while(i <= length(input)){
    
    if(is.ShiftStart(input[i])){
      
      # two consecutive shift starts
      # i + 1 will never be out of bounds as last two entries 
      # known to be non-shift starting entries
      if(is.ShiftStart(input[i+1])){

        # nothing to log here
        i <- i + 1
        
      } else {
        
        # set current guard 
        curGuard <- ExtractGuard(input[i])
        
        # add time to log, -1 for instantly awake
        sleepTime <- seq(from = ExtractMinute(input[i+1]),
                         to = ExtractMinute(input[i+2]) - 1) +1
        trackList[[curGuard]][sleepTime] <- 
          trackList[[curGuard]][sleepTime] +1
        
        # skip ahead three entries
        i <- i + 3
        
      }
      
    } else {
      
      # no change of guards
      # assume falls asleep always and only proceeds wakes up
      # -1 for instantly awake
      sleepTime <- seq(from = ExtractMinute(input[i]),
                       to = ExtractMinute(input[i+1]) - 1) + 1
      trackList[[curGuard]][sleepTime] <- 
        trackList[[curGuard]][sleepTime] +1
      
      # skip ahead two entries
      i <- i + 2
      
    }
  }
  
  # return tallied sleep log
  trackList
  
}

SummarizeGuardSleep <- function(sleepLog){
  
  map_dfr(sleepLog, ~data.frame(totSleep = sum(.x), 
                            whichMin = which.max(.x)-1, 
                            howMany = max(.x)),
          .id = "guard")
  
}

GetSleepSummary <- function(input){
  
  input %>%
    GetGuardList() %>%
    GenTrackList() %>%
    ParseSleepLog(input) %>%
    SummarizeGuardSleep()
  
}

BestTarget.s1 <- function(sleepSummary){
  
  sleepSummary %>%
    dplyr::top_n(1, totSleep)
  
}

CalcCode <- function(entry){
  
  with(entry, as.integer(guard) * whichMin)
  
}



Part1Ans <- function(sumInput){

  sumInput %>%
    BestTarget.s1() %>%
    CalcCode()
  
}


sleepAnalysis <- GetSleepSummary(data)

if(Part1Ans(GetSleepSummary(testData)) != 
   Part1Ans(GetSleepSummary(testScrambled))){
  stop("Test answers don't match with data is scrambled.")
} else {
  print(sprintf("The answer to part 1 is: %i", Part1Ans(sleepAnalysis)))
}

#### Part 2 ----
BestTarget.s2 <- function(sleepSummary){
  
  sleepSummary %>%
    dplyr::top_n(1, howMany)
  
}

Part2Ans <- function(sumInput){
  
  sumInput %>%
    BestTarget.s2() %>%
    CalcCode()
  
}

if(Part2Ans(GetSleepSummary(testData)) != 
   Part2Ans(GetSleepSummary(testScrambled))){
  stop("Test answers don't match with data is scrambled.")
} else {
  print(sprintf("The answer to part 1 is: %i", Part2Ans(sleepAnalysis)))
}