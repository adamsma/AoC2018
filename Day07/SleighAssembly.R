library(purrr)
library(stringr)
library(dplyr)

testData <- c("Step C must be finished before step A can begin.",
              "Step C must be finished before step F can begin.",
              "Step A must be finished before step B can begin.",
              "Step A must be finished before step D can begin.",
              "Step B must be finished before step E can begin.",
              "Step D must be finished before step E can begin.",
              "Step F must be finished before step E can begin.") %>%
  str_extract_all("(?<= )[[:upper:]](?= )") %>%
  reduce(rbind) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>%
  set_names(c("pReq", "dependent"))

data <- readr::read_lines("Input07.txt") %>%
  str_extract_all("(?<= )[[:upper:]](?= )") %>%
  reduce(rbind) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>%
  set_names(c("pReq", "dependent"))

#### Part 1 ----

# Get the unquie set of steps from instructions
GetLetterSet <- function(prereqLst){
  
  unlist(prereqLst) %>% unique()
  
}

# Determines steps blocked by pre-reqs
BlockedSteps <- function(reqLst){
  
  unique(reqLst[[2]])
  
}

# Creates available step list
AvailableSteps <- function(psblSteps, compSteps, blckSteps){
  
  setdiff(psblSteps, compSteps) %>%
    setdiff(blckSteps)
  
}

# Determines next step and updates completed steps and requirement list
PerformNextStep <- function(taskInfo, stepNum, 
                            possSteps = LETTERS, printStep = FALSE){
  
  if(printStep) print(sprintf("Performing task #%i", stepNum))
  
  
  nxtStep <- AvailableSteps(possSteps, 
                            taskInfo$completeSteps, 
                            BlockedSteps(taskInfo$reqs)) %>%
    sort() %>%
    head(1)
  
  newComp <- c(taskInfo$completeSteps, nxtStep)
  newReqs <- filter(taskInfo$reqs, pReq != nxtStep)
  
  list(completeSteps = newComp, reqs = newReqs)
  
}

# Determines step list
MapStepList <- function(input){
  # browser()
  allSteps <- GetLetterSet(input)
  
  taskInfo <- list(completedSteps = NULL, reqs = input)
  
  rdStr <- c(list(taskInfo), as.list(seq_along(allSteps)))
  
  reduce(rdStr, PerformNextStep, possSteps = allSteps, printStep = FALSE) %>%
    pluck("completeSteps") %>%
    str_c(collapse = "")
  
}

test1Ans <- MapStepList(testData)
TEST1CHK <- "CABDFE"

if(test1Ans != TEST1CHK){
  stop("Incorrect step order for for test case. ",
       sprintf("Result is %s instead of %s", test1Ans, TEST1CHK),
       call. = FALSE)
} else {
  message("Test 1 Passed")
}

print(sprintf("Answer for part 1: %s", MapStepList(data)))


#### Part 2 ----

# Generates Blank Schedule
GenNewSchedule <- function(workers, max.time){
  
  cbind(Second = seq(0, max.time), 
        map_dfc(seq(workers), ~character(1)), 
        data.frame(Done = character(1), stringsAsFactors = FALSE)) %>%
    set_names("Second", paste0("Worker",seq(workers)), "Done")
  
}

# Determine available workers at a given time
AvailWorkers <- function(sched, t){
  
  avail <- select(sched, starts_with("Worker")) %>%
    slice(t+1) 
    
  which(avail == "")
  
}

# Calculate Time Required for given step
TimeReq <- function(step, base){
  
  base + which(LETTERS == step)
  
}

# Assign available steps to available workers at given time
AssignWork <- function(taskInfo, t, baseTime, possSteps = LETTERS){
  
  # Determine available workers
  whoAvail <- AvailWorkers(taskInfo$sched, t)
  
  # Determine in progress steps
  inProg <- select(taskInfo$sched, starts_with("Worker")) %>%
    slice(t+1) %>%
    str_subset("^[[:upper:]]$")
  
  # Determine if any work has been completed since last step
  if(t > 0){
    
    # determine new completions
    prevInProg <- select(taskInfo$sched, starts_with("Worker")) %>%
      slice(t) %>%
      str_subset("^[[:upper:]]$")
    
    prevComp <- taskInfo$completeSteps
    prevCompStr <- paste0(prevComp, collapse = "")
    
    newComp <- setdiff(prevInProg, inProg)
    newCompStr <- paste0(newComp, collapse ="")
    
    # update done column for schedule, and completed work vector
    taskInfo$sched[t+1, "Done"] <- paste0(prevCompStr, newCompStr)
    taskInfo$completeSteps <- c(prevComp, newComp)
    
    # update reqs if new work has been completed
    if(length(newComp)){
      taskInfo$reqs <- filter(taskInfo$reqs, !(pReq %in% newComp))
    }
      
  }
  
  # Determine available steps to assign
  availSteps <- AvailableSteps(possSteps, 
                            taskInfo$completeSteps, 
                            BlockedSteps(taskInfo$reqs)) %>%
    sort() %>%
    setdiff(inProg)
  
  # if there are available workers and steps, assign steps
  i <- 1
  while((NROW(whoAvail) > 0) && (NROW(availSteps) > 0) && i < 5000){
    
    nxtStep <- availSteps[1]
    nxtWorker <- paste0("Worker", whoAvail[1])
    t2Sche <- seq(t+1, TimeReq(nxtStep, baseTime) + t)
    
    taskInfo$sched[t2Sche, nxtWorker] <- nxtStep
    
    whoAvail <- whoAvail[-1]
    availSteps <- availSteps[-1]
    i <- i + 1
  }
  
  taskInfo
  
}

# Determine Work Schedule
MakeWorkSchedule <- function(input, workers, baseTime, possSteps){
  
  maxTime <- length(possSteps)*baseTime + 
    sum(which(LETTERS %in% possSteps))
  
  blankSched <- GenNewSchedule(workers, maxTime)
  
  taskInfo <- list(sched = blankSched,
                   completeSteps = NULL, 
                   reqs = input)  
  
  reObj <- c(list(taskInfo), as.list(blankSched$Second))
  
  finTask <- reduce(reObj, AssignWork, 
                    baseTime = baseTime, possSteps = possSteps)
  
  finTask$sched
    
}

# Determine Time to Complete
CalcWorkTime <- function(input, workers, baseTime, printSched = FALSE){
  
  possSteps <-  GetLetterSet(input)
  
  sched <- MakeWorkSchedule(input, workers, baseTime, possSteps) 
  
  if(printSched) print(sched)
  
  sched %>%
    filter(str_length(Done) == length(possSteps)) %>%
    pluck("Second") %>%
    min()
  
}

test2Ans <- CalcWorkTime(testData, 2, 0)
TEST2CHK <- 15

if(test2Ans != TEST2CHK){
  stop("Incorrect time for test case. ",
       sprintf("Result is %s instead of %s", test2Ans, TEST2CHK),
       call. = FALSE)
} else {
  message("Test 2 Passed")
}

print(sprintf("Answer for part 2: %s", CalcWorkTime(data, 5, 60)))