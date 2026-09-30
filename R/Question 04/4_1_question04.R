dados <- read.csv("data/HW1_bike_sharing.csv")
data_group <- dados[96:395, ]                     # observações 96 a 395
data_group$total_user <- data_group$casual + data_group$registered
Q1 <- quantile(data_group$total_user, 0.25)       # 3343.75
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

data_group$dteday <- as.Date(data_group$dteday)

# média mensal
aggregate(total_user ~ format(dteday, "%Y-%m"), data = data_group, FUN = mean)

# dias de maior e menor utilização
data_group[which.max(data_group$total_user), c("dteday", "total_user")]
data_group[which.min(data_group$total_user), c("dteday", "total_user")]

# gráfico
plot(data_group$dteday, data_group$total_user,
     type = "l",
     xlab = "Data",
     ylab = "Total de usuários",
     main = "Série temporal de total_user")

abline(h = Q1, lty = 2)   # linha do primeiro quartil (critério de low_usage)
