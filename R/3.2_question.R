library(dplyr); library(ggplot2); library(scales)

dados <- read.csv(file.choose()) #tive uns problemas em ter meu arquivo "achado" , então pesquisei e achei q isso abre os arquivos no windows
dados$clima <- factor(dados$weathersit, levels = 1:3,
                      labels = c("Limpo/Parc. nublado", "Névoa/Nublado", "Chuva/Neve leve"))

resumo <- function(df) {
  df %>% group_by(clima) %>%
    summarise(n = n(),
              media = mean(total_user),
              dp = sd(total_user),
              n_low = sum(low_usage),
              prop_low = mean(low_usage))
}

# 10 primeiras amostras
print(as.data.frame(resumo(head(dados, 10))))

# 300 amostrars
resumo300 <- resumo(dados)
print(as.data.frame(resumo300))

# gráficos
g1 <- ggplot(dados, aes(clima, total_user, fill = clima)) +
  geom_boxplot() +
  labs(title = "total_user por condição meteorológica",
       x = "Condição meteorológica", y = "Usuários por dia") +
  theme_minimal() + theme(legend.position = "none")

g2 <- ggplot(resumo300, aes(clima, prop_low, fill = clima)) +
  geom_col() +
  geom_text(aes(label = percent(prop_low, accuracy = 0.1)), vjust = -0.4) +
  scale_y_continuous(labels = percent, limits = c(0, 1.1)) +
  labs(title = "Proporção de dias low_usage por condição meteorológica",
       x = "Condição meteorológica", y = "Proporção") +
  theme_minimal() + theme(legend.position = "none")

dev.new(); print(g1)
dev.new(); print(g2)
