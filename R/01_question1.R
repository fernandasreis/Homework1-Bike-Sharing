matriculas <- c(582795, 578061, 579540) 
M <- max(matriculas) 
r <- 1 + (M %% 100) #96

s <- r + 300 - 1 #395
data_group <- HW1_bike_sharing[r:s, ] 

firstdate <- data_group$dteday[1] 
lastdate <- data_group$dteday[300]
