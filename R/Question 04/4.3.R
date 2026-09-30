
dados <- read.csv("data/HW1_bike_sharing.csv")
data_group <- dados[96:395, ]                     # observações 96 a 395
data_group$total_user <- data_group$casual + data_group$registered
Q1 <- quantile(data_group$total_user, 0.25)       # 3343.75
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

#10 primeiras observações
data_group10 <- data_group[1:10, ]
table(data_group10$low_usage)  
cor(data_group10$temp[data_group10$low_usage == 1],
    data_group10$total_user[data_group10$low_usage == 1])   # 0.42

# 300 observações ---> médias por grupo
aggregate(cbind(temp, total_user) ~ low_usage, data = data_group, FUN = mean)

# correlação em cada grupo
cor(
  data_group$temp[data_group$low_usage == 1],
  data_group$total_user[data_group$low_usage == 1]
)   # 0.04

cor(
  data_group$temp[data_group$low_usage == 0],
  data_group$total_user[data_group$low_usage == 0]
)   # 0.42

# gráfico de dispersão
plot(
  data_group$temp,
  data_group$total_user,
  type = "n",
  xlab = "Temperatura (°C)",
  ylab = "Total de usuários",
  main = "Temperatura x Total de usuários"
)

# identificação dos grupos
points(
  data_group$temp[data_group$low_usage == 0],
  data_group$total_user[data_group$low_usage == 0],
  pch = 1
)

points(
  data_group$temp[data_group$low_usage == 1],
  data_group$total_user[data_group$low_usage == 1],
  pch = 19
)

legend(
  "topleft",
  legend = c("low_usage = 1", "low_usage = 0"),
  pch = c(19, 1)
)
