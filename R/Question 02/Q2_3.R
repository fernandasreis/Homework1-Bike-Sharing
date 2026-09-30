# Questão 2.3 
setwd("C:/Users/htorq/Downloads")
df <- read.csv("data_group_completed.csv")

# 2. Teste com amostra de 10 primeiras observações (instant 96-105)
cat("======= TESTE: QUARTIS - AMOSTRA DE 10 ======\n")

amostra_10 <- df[1:10, ]
variavel <- amostra_10$total_user

Q1 <- quantile(variavel, 0.25)
Q2 <- quantile(variavel, 0.50)
Q3 <- quantile(variavel, 0.75)
IQR_valor <- Q3 - Q1
limite_inferior <- Q1 - 1.5 * IQR_valor
limite_superior <- Q3 + 1.5 * IQR_valor

cat("Q1:", Q1, "| Q2:", Q2, "| Q3:", Q3, "\n")
cat("IQR:", IQR_valor, "\n")
cat("Limites:", limite_inferior, "a", limite_superior, "\n")

outliers_10 <- amostra_10[amostra_10$total_user < limite_inferior | amostra_10$total_user > limite_superior, ]
cat("Número de outliers:", nrow(outliers_10), "\n\n")

# 3. Dataset completo (300 observações)
cat("======= QUARTIS E OUTLIERS - DATASET COMPLETO =======\n")

variavel <- df$total_user

Q1 <- quantile(variavel, 0.25)
Q2 <- quantile(variavel, 0.50)
Q3 <- quantile(variavel, 0.75)
IQR_valor <- Q3 - Q1
limite_inferior <- Q1 - 1.5 * IQR_valor
limite_superior <- Q3 + 1.5 * IQR_valor

cat("Q1:", Q1, "\n")
cat("Q2 (mediana):", Q2, "\n")
cat("Q3:", Q3, "\n")
cat("IQR:", IQR_valor, "\n")
cat("Limite inferior:", limite_inferior, "\n")
cat("Limite superior:", limite_superior, "\n\n")

outliers <- df[df$total_user < limite_inferior | df$total_user > limite_superior, ]
cat("Número de outliers:", nrow(outliers), "\n\n")
print(outliers[, c("dteday", "total_user")])
