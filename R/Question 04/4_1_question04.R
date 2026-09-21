data_group$dteday <- as.Date(data_group$dteday) #converte dteday para um formato de data reconhecida pelo R

plot(data_group$dteday, data_group$total_user,
     type = "l",
     xlab = "Data",
     ylab = "Total de usuários",
     main = "Série temporal de total_user") #gráfico

abline(h = Q1, lty = 2) #linha do quartil 1
