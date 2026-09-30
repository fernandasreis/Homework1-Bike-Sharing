# Questão 3.3 - Relação entre temperatura e total_user

dados <- read.csv("data/HW1_bike_sharing.csv")
data_group <- dados[96:395, ]                     # observações 96 a 395
data_group$total_user <- data_group$casual + data_group$registered
Q1 <- quantile(data_group$total_user, 0.25)       # 3343.75
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

#10 primeiras observações
data_group10 <- data_group[1:10, ]
cor(data_group10$temp, data_group10$total_user)   # 0.5471

# 300 observações
cor(data_group$temp, data_group$total_user)       # 0.60

# Gráfico de dispersão!
plot(
  data_group$temp,
  data_group$total_user,
  xlab = "Temperatura (°C)",
  ylab = "Total de usuários",
  main = "Relação entre temperatura e total de usuários"
)
