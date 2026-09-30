
dados <- read.csv("data/HW1_bike_sharing.csv")
data_group <- dados[96:395, ]                     # observações 96 a 395
data_group$total_user <- data_group$casual + data_group$registered
Q1 <- quantile(data_group$total_user, 0.25)       # 3343.75
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

quartis <- function(variavel) {
  Q1 <- quantile(variavel, 0.25)
  Q2 <- quantile(variavel, 0.50)
  Q3 <- quantile(variavel, 0.75)
  IQR_valor <- Q3 - Q1
  c(Q1 = Q1[[1]], Q2 = Q2[[1]], Q3 = Q3[[1]], IQR = IQR_valor[[1]],
    limite_inferior = Q1[[1]] - 1.5 * IQR_valor[[1]],
    limite_superior = Q3[[1]] + 1.5 * IQR_valor[[1]])
}

# Teste com as 10 primeiras!
cat("======= TESTE: QUARTIS - AMOSTRA DE 10 =======\n")
amostra_10 <- data_group[1:10, ]
res10 <- quartis(amostra_10$total_user)
print(res10)
outliers_10 <- amostra_10[amostra_10$total_user < res10["limite_inferior"] |
                          amostra_10$total_user > res10["limite_superior"], ]
cat("Número de outliers:", nrow(outliers_10), "\n\n")

# Conjunto completo
cat("======= QUARTIS E OUTLIERS - 300 OBSERVAÇÕES =======\n")
res300 <- quartis(data_group$total_user)
print(res300)
outliers <- data_group[data_group$total_user < res300["limite_inferior"] |
                       data_group$total_user > res300["limite_superior"], ]
cat("Número de outliers:", nrow(outliers), "\n\n")
print(outliers[, c("dteday", "total_user", "weathersit", "temp")])
