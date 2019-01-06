
# visualize test data with inequality lines
ggplot(testData, aes(x,y)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = "red") + 
  geom_abline(slope = -1, intercept = 2, colour = "red") +
  
  geom_abline(slope = 1, intercept = 5, colour = "green") + 
  geom_abline(slope = -1, intercept = 7, colour = "green") +
  
  geom_abline(slope = 1, intercept = -5, colour = "blue") + 
  geom_abline(slope = -1, intercept = 11, colour = "blue") +
  
  geom_abline(slope = 1, intercept = 1, colour = "orange") + 
  geom_abline(slope = -1, intercept = 17, colour = "orange") +
  
  geom_hline(yintercept = 1) + geom_hline(yintercept = 9) +
  geom_vline(xintercept = 1) + geom_vline(xintercept = 8) +
  
  xlim(c(0, 10)) + ylim(c(0,10)) +
  
  coord_equal() + theme_minimal()

# visualize data with inequality lines
ggplot(data, aes(x,y)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 90-61, colour = "red") + 
  geom_abline(slope = -1, intercept = 90+61, colour = "red") +
  
  geom_abline(slope = 1, intercept = 44-218, colour = "green") + 
  geom_abline(slope = -1, intercept = 44+218, colour = "green") +
  
  geom_abline(slope = 1, intercept = 66-358, colour = "blue") + 
  geom_abline(slope = -1, intercept = 66+358, colour = "blue") +
  
  geom_abline(slope = 1, intercept = 355-94, colour = "orange") + 
  geom_abline(slope = -1, intercept = 355+94, colour = "orange") +
  
  geom_hline(yintercept = 44) + geom_hline(yintercept = 355) +
  geom_vline(xintercept = 61) + geom_vline(xintercept = 358) +
  
  coord_equal() + theme_minimal()

# boundary for test data
tstrt1 <- arrange(testData, x, y)

ts1 <- TraceSide(tstrt1[1,], testData, "x", 1)
ts2 <- TraceSide(tail(ts1,1), testData, "y", 1)
ts3 <- TraceSide(tail(ts2,1), testData, "x", -1)
ts4 <- TraceSide(tail(ts3,1), testData, "y", -1)

tSComb <- rbind(ts1, ts2, ts3, ts4) %>% distinct()

ggplot(testData, aes(x,y)) + 
  geom_point() +
  geom_path(data = rbind(tSComb, ts1[1,])) +
  coord_equal() + theme_minimal()

# boundary for data
strt1 <- arrange(data, x, y)

s1 <- TraceSide(strt1[1,], data, "x", 1)
s2 <- TraceSide(tail(s1,1), data, "y", 1)
s3 <- TraceSide(tail(s2,1), data, "x", -1)
s4 <- TraceSide(tail(s3,1), data, "y", -1)

sComb <- rbind(s1, s2, s3, s4) %>% distinct()

ggplot(data, aes(x,y)) + 
  geom_point() +
  geom_path(data = rbind(sComb, s1[1,])) +
  coord_equal() + theme_minimal()