library(dplyr); library(ggplot2)

dados <- read.csv(file.choose())
dados$clima <- factor(dados$weathersit, levels = 1:3,
                      labels = c("Céu limpo", "Nublado", "Chuva fraca"))
dados$faixa_temp <- cut(dados$temp, breaks = c(0, 10, 15, 20, 25, 30, 40))

resumo_42 <- function(df) {
  cat("Temperatura: mediana por faixa \n")
  print(as.data.frame(df %>% group_by(faixa_temp) %>%
    summarise(n = n(), mediana = median(total_user))))
  cat("Clima: quartis e IQR \n")
  print(as.data.frame(df %>% group_by(clima) %>%
    summarise(n = n(), Q1 = quantile(total_user, 0.25), mediana = median(total_user),
              Q3 = quantile(total_user, 0.75), IQR = IQR(total_user))))
}
cat(" 10 observações");  resumo_42(head(dados, 10))
cat("\n 300 observações"); resumo_42(dados)

g_temp <- ggplot(dados, aes(faixa_temp, total_user)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "total_user por faixa de temperatura",
       x = "Faixa de temperatura (°C)", y = "Usuários por dia") + theme_minimal()

resumo_clima <- dados %>% group_by(clima) %>%
  summarise(Q1 = quantile(total_user, 0.25), mediana = median(total_user),
            Q3 = quantile(total_user, 0.75))
g_clima <- ggplot(resumo_clima, aes(clima, mediana)) +
  geom_col(fill = "lightblue") +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2) +
  labs(title = "Mediana de total_user por condição meteorológica (barras = Q1 a Q3!)",
       x = "Condição meteorológica", y = "Mediana de usuários por dia") + theme_minimal()

dev.new(); print(g_temp)
dev.new(); print(g_clima)
