library(stringr)
library(purrr)
library(dplyr)
library(ggplot2)
library(gganimate)

testData <- readLines("TestInput10.txt") %>%
  str_extract_all("-?[[:digit:]]+") %>%
  map_dfr(~data.frame(x = as.numeric(.x[1]), y = as.numeric(.x[2]), 
                  vx = as.numeric(.x[3]), vy = as.numeric(.x[4])))
  
data <- readLines("Input10.txt") %>%
  str_extract_all("-?[[:digit:]]+") %>%
  map_dfr(~data.frame(x = as.numeric(.x[1]), y = as.numeric(.x[2]), 
                      vx = as.numeric(.x[3]), vy = as.numeric(.x[4])))

(testData[,1:2] + 3*testData[,3:4]) %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point(size = 7) +
  theme_minimal() 

# function that enumerates the position of the point over n time points
EnumCourse <- function(pInfo, n, from = 1, by = 1){
  
  pInfo <- as.integer(pInfo)
  
  xi <- pInfo[1]
  yi <- pInfo[2]
  vx <- pInfo[3]
  vy <- pInfo[4]
  t <- seq(from = from, to = n, by = by) 
  
  data_frame(t, x = xi + t*vx, y = yi + t*vy)
  
}

# calculates distance betwen points
TotDist <- function(x, y){
  
  sum(dist(x))/2
  
}

# mirrors points about the horizontal axis
mirPoints <- function(img) {
  
  as.matrix(img) %*% matrix(c(1,0,0,-1), nrow = 2) %>%
    as.data.frame() %>%
    set_names(c("x", "y"))
  
}

#### test message ----
n_test <- 5
testTraj <- map_dfr(seq_along(testData[[1]]), 
                    ~bind_cols(n = rep(.x, n_test), 
                               EnumCourse(pInfo = testData[.x,], n = n_test))
                    )


testMinDis <- testTraj %>% 
  group_by(t) %>% 
  summarise(pDis = TotDist(data.frame(x,y)) ) %>%
  filter(pDis == min(pDis))

# static plot of minimum distance
filter(testTraj, t == as.numeric(testMinDis$t)) %>% 
  ggplot(aes(x, y)) +
  geom_point() 

# animate motion
tGplot <- testTraj %>% 
  ggplot(aes(x, y)) +
  geom_point() 

tGplot +
  transition_time(t) +
  labs(title = 'Time: {frame_time}')

tGplot + 
  transition_states(t, state_length = 0.5) +
  labs(title = 'Time: {closest_state}')

#### Part 1 message ----

start <- 10458
end <- 10460
by <- 0.2

mesTraj <- map_dfr(seq_along(data[[1]]), 
                   ~bind_cols(n = rep(.x, (end - start)/by + 1), 
                              EnumCourse(pInfo = data[.x,], 
                                         n = end, from = start, by = by))
)

dataDist <- mesTraj %>% 
  group_by(t) %>% 
  summarise(pDis = TotDist(data.frame(x,y)) ) 

ggplot(dataDist, aes(t, pDis)) + geom_line()


dataMinDis <- dataDist %>%
  filter(pDis == min(pDis))

# static plot of minimum distance
filter(mesTraj, t == as.numeric(dataMinDis$t)) %>%
  select(x,y) %>%
  mirPoints() %>%
  ggplot(aes(x, y)) + 
  geom_point() +
  # geom_line() +
  xlim(190, 270) + ylim(-170, -150) +
  theme_minimal()

flipTraj <- mesTraj
flipTraj[,c("x", "y")] <- mesTraj %>%
  select(x,y) %>%
  mirPoints()

mesGplot <- flipTraj %>%
  ggplot(aes(x = x, y = y, group = n)) +
  geom_point()

ani <- mesGplot + 
  transition_states(t, state_length = 3) +
  # transition_time(t) +
  labs(title = 'Time: {closest_state}') 
  # labs(title = 'Time: {frame_time}')
  # view_follow()

animate(ani)
# animate(ani, renderer = file_renderer())

print(sprintf("Answer for part 1: %s", "NEXPLRXK"))
print(sprintf("Answer for part 2: %i", 10459))