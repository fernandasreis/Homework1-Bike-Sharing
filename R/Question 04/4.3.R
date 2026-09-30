cor(
  data_group$temp[data_group$low_usage == 1],
  data_group$total_user[data_group$low_usage == 1]
)

cor(
  data_group$temp[data_group$low_usage == 0],
  data_group$total_user[data_group$low_usage == 0]
)

# gráfico de dispersão
plot(
  data_group$temp,
  data_group$total_user,
  xlab = "Temperatura (°C)",
  ylab = "Total de usuários",
  main = "Temperatura × Total de usuários"
)

# identificação dos grupos
points(
  data_group$temp[data_group$low_usage == 1],
  data_group$total_user[data_group$low_usage == 1],
  pch = 19
)

points(
  data_group$temp[data_group$low_usage == 0],
  data_group$total_user[data_group$low_usage == 0],
  pch = 1
)

legend(
  "topleft",
  legend = c("low_usage = 1", "low_usage = 0"),
  pch = c(19, 1)
)
