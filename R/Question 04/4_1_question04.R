data_group$dteday <- as.Date(data_group$dteday)

plot(data_group$dteday, data_group$total_user,
     type = "l",
     xlab = "Data",
     ylab = "Total de usuários",
     main = "Série temporal de total_user")

abline(h = Q1, lty = 2)
