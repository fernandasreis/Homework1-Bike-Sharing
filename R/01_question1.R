matriculas <- c(582795, 578061, 579540) 
M <- max(matriculas) 
r <- 1 + (M %% 100) 

s <- r + 300 - 1 
data_group <- HW1_bike_sharing[96:395, ] 

firstdate <- data_group$dteday[1] 
lastdate <- data_group$dteday[300]
