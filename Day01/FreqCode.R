library(readr)
library(dplyr)

#### Part 1 ----

smpl <- data.frame(x1 = c(1, -2, 3, 1))
data <- read_csv("Input01A.txt", col_names = FALSE)

print(sprintf("Part 1 answer is: %i", sum(data)))


#### Part 2 ----
min <- 100
max <- 1000
cur <- 550
cont <- TRUE
i <- 1

while(cont && i <= 100) {
  
  cyl <- rep(data[[1]], cur) %>%
    cumsum() %>%
    table() %>%
    data.frame() %>%
    rename(f = ".", ct = "Freq") %>%  
    mutate(f = as.numeric(as.character(f))) %>% 
    filter(ct > 1)
  
  solCt <- nrow(cyl)
  lastCur <- cur
  
  sprintf("Iteration %i (rep = %i) found %i solutions", i, cur, solCt) %>% 
    print()
    
  if(solCt > 1){
    
    max <- cur
    cur <- ceiling((max + min)/2)
    
  } else if(solCt == 0){
    
    min <- cur
    cur <- ceiling((max + min)/2)
    
  } else{
    cont <- FALSE
  }
  
  if(lastCur == cur){
    cont <- FALSE
  }
  
  i <- i + 1
  
  
}

if(solCt != 1){
  
  stFrq <- sum(rep(data[[1]], cur - 1))
  
  nxtFs <- cumsum(data[[1]]) + stFrq
  
  dup <- nxtFs[nxtFs %in% cyl$f]
  
  print(sprintf("Part 2 answer is: %i", dup[1]))
  
} else {
  print(sprintf("Part 2 answer is: %i", cyl$f[1]))
}


 
  
