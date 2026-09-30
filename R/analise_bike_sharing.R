
# Homework 1 - Bike Sharing
# Script unificado: Questões 1 a 4

# Dependências: R (>= 4.0) e os pacotes dplyr, ggplot2 e scales
#   install.packages(c("dplyr", "ggplot2", "scales"))

# Execução: defina a pasta raiz do repositório como diretório de trabalho
# (a que contém as pastas data/ e R/) e rode:
#   source("R/analise_bike_sharing.R", echo = TRUE, encoding = "UTF-8")
# Entrada: data/HW1_bike_sharing.csv
# Saída:   figuras em figures/final/


library(dplyr)
library(ggplot2)
library(scales)

pasta_fig <- file.path("figures", "final")
dir.create(pasta_fig, recursive = TRUE, showWarnings = FALSE)

salvar <- function(grafico, nome, largura = 8, altura = 5) {
  ggsave(file.path(pasta_fig, nome), grafico,
         width = largura, height = altura, dpi = 300, bg = "white")
}


# QUESTÃO 1 - Construção do conjunto de dados do grupo

original <- read.csv(file.path("data", "HW1_bike_sharing.csv"))

M <- 582795                 # maior número de matrícula do grupo
r <- 1 + (M %% 100)         # r = 96
data_group <- original[r:(r + 299), ]   # observações 96 a 395, ordem original

cat("r =", r, "| observações", r, "a", r + 299,
    "| n =", nrow(data_group), "\n")

# Temperatura: no arquivo fornecido, temp já está em graus Celsius
# (valores entre 2,4 e 35,3), e não normalizada; nenhuma conversão é aplicada.

data_group$total_user <- data_group$casual + data_group$registered
data_group$dteday     <- as.Date(data_group$dteday)

# Rótulos das variáveis categóricas
# Estações: rótulos na ordem do enunciado (1 = primavera, 2 = verão,
# 3 = outono, 4 = inverno).
data_group$estacao <- factor(data_group$season, levels = 1:4,
                             labels = c("Primavera", "Verão", "Outono", "Inverno"))
data_group$clima   <- factor(data_group$weathersit, levels = 1:4,
                             labels = c("Céu limpo", "Nublado",
                                        "Chuva fraca", "Chuva forte"))

amostra10 <- head(data_group, 10)   # usada nos cálculos manuais


# QUESTÃO 2.1 - Tipos de variáveis, categorias e valores ausentes

str(data_group[, c("instant", "dteday", "season", "weathersit",
                   "temp", "casual", "registered")])
cat("Período:", format(min(data_group$dteday)), "a",
    format(max(data_group$dteday)), "\n")
print(table(data_group$estacao))
print(table(data_group$clima))
cat("Valores ausentes por variável:\n")
print(colSums(is.na(data_group)))


# QUESTÃO 2.2 - Medidas de tendência central

moda <- function(x) {
  f <- table(x)
  if (max(f) == 1) return("sem moda")
  paste0(paste(names(f)[f == max(f)], collapse = "; "),
         " (", max(f), " ocorrências)")
}

tendencia_central <- function(df) {
  vars <- c("temp", "casual", "registered", "total_user")
  data.frame(variavel = vars,
             soma    = sapply(vars, function(v) sum(df[[v]])),
             media   = sapply(vars, function(v) mean(df[[v]])),
             mediana = sapply(vars, function(v) median(df[[v]])),
             moda    = sapply(vars, function(v) moda(df[[v]])),
             row.names = NULL)
}
cat("\n== 2.2: 10 observações ==\n");  print(tendencia_central(amostra10))
cat("\n== 2.2: 300 observações ==\n"); print(tendencia_central(data_group))


# QUESTÃO 2.3 - Quartis, IQR 

quartis_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.50, 0.75))
  iqr <- q[[3]] - q[[1]]
  c(Q1 = q[[1]], Q2 = q[[2]], Q3 = q[[3]], IQR = iqr,
    lim_inf = q[[1]] - 1.5 * iqr, lim_sup = q[[3]] + 1.5 * iqr)
}
cat("\n== 2.3: 10 observações ==\n");  print(quartis_iqr(amostra10$total_user))
lim <- quartis_iqr(data_group$total_user)
cat("\n== 2.3: 300 observações ==\n"); print(lim)

atipicos <- data_group %>%
  filter(total_user < lim[["lim_inf"]] | total_user > lim[["lim_sup"]]) %>%
  select(dteday, total_user, clima, estacao, temp)
cat("Valores atípicos:", nrow(atipicos), "\n"); print(atipicos)


# qUESTÃO 2.4  Histograma e boxplot de total_user

cat("\n== 2.4 ==\n")
cat("Média:", mean(data_group$total_user),
    "| Mediana:", median(data_group$total_user),
    "| DP:", sd(data_group$total_user),
    "| Mín:", min(data_group$total_user),
    "| Máx:", max(data_group$total_user), "\n")

g_hist <- ggplot(data_group, aes(total_user)) +
  geom_histogram(binwidth = 400, boundary = 0,
                 fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean(data_group$total_user),
             linetype = "dashed", color = "red") +
  geom_vline(xintercept = median(data_group$total_user),
             linetype = "solid", color = "darkgreen") +
  labs(title = "Histograma de total_user",
       subtitle = "Linha tracejada vermelha: média | linha verde: mediana",
       x = "Total de usuários por dia", y = "Frequência (dias)") +
  theme_minimal()
salvar(g_hist, "q2_4_histograma.png")

g_box <- ggplot(data_group, aes(x = "", y = total_user)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, width = 0.4) +
  labs(title = "Boxplot de total_user",
       x = NULL, y = "Total de usuários por dia") +
  theme_minimal()
salvar(g_box, "q2_4_boxplot.png", largura = 5)


# 2.5 - Variável low_usage

Q1 <- quantile(data_group$total_user, 0.25)
data_group$low_usage <- as.integer(data_group$total_user < Q1)
amostra10 <- head(data_group, 10)
cat("\n== 2.5 ==\nQ1 =", Q1, "| dias low_usage =", sum(data_group$low_usage),
    "| proporção =", mean(data_group$low_usage), "\n")
cat("low_usage nas 10 primeiras:", sum(amostra10$low_usage), "de 10\n")


# Q3 - Função de resumo por grupo

resumo_grupo <- function(df, grupo) {
  df %>%
    group_by({{ grupo }}) %>%
    summarise(n = n(),
              media    = mean(total_user),
              mediana  = median(total_user),
              dp       = sd(total_user),
              n_low    = sum(low_usage),
              prop_low = mean(low_usage),
              .groups = "drop") %>%
    as.data.frame()
}

# 3.1  Estações
cat("\n== 3.1: 10 observações ==\n");  print(resumo_grupo(amostra10, estacao))
res_estacao <- resumo_grupo(data_group, estacao)
cat("\n== 3.1: 300 observações ==\n"); print(res_estacao)

g31 <- ggplot(data_group, aes(estacao, total_user, fill = estacao)) +
  geom_boxplot() +
  labs(title = "total_user por estação do ano",
       x = "Estação", y = "Total de usuários por dia") +
  theme_minimal() + theme(legend.position = "none")
salvar(g31, "q3_1_boxplot_estacao.png")

# 3.2  Condições meteorológicas
cat("\n== 3.2: 10 observações ==\n");  print(resumo_grupo(amostra10, clima))
res_clima <- resumo_grupo(data_group, clima)
cat("\n== 3.2: 300 observações ==\n"); print(res_clima)

dados_clima <- data_group %>% mutate(clima = droplevels(clima))

g32a <- ggplot(dados_clima, aes(clima, total_user, fill = clima)) +
  geom_boxplot() +
  labs(title = "total_user por condição meteorológica",
       x = "Condição meteorológica", y = "Total de usuários por dia") +
  theme_minimal() + theme(legend.position = "none")
salvar(g32a, "q3_2_boxplot_clima.png")

g32b <- ggplot(res_clima, aes(clima, prop_low, fill = clima)) +
  geom_col() +
  geom_text(aes(label = paste0(percent(prop_low, accuracy = 0.1),
                               " (", n_low, "/", n, ")")), vjust = -0.4) +
  scale_y_continuous(labels = percent, limits = c(0, 1.1)) +
  labs(title = "Proporção de dias low_usage por condição meteorológica",
       x = "Condição meteorológica", y = "Proporção de dias") +
  theme_minimal() + theme(legend.position = "none")
salvar(g32b, "q3_2_prop_low_clima.png")

# 3.3  Temperatura x total_user
cat("\n== 3.3 ==\n")
cat("Correlação (10 obs):",  cor(amostra10$temp, amostra10$total_user), "\n")
cat("Correlação (300 obs):", cor(data_group$temp, data_group$total_user), "\n")

g33 <- ggplot(data_group, aes(temp, total_user)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(title = "Temperatura x total_user",
       x = "Temperatura (°C)", y = "Total de usuários por dia") +
  theme_minimal()
salvar(g33, "q3_3_dispersao.png")

# 3.4  Temperatura média por estação 
cat("\n== 3.4 ==\n")
print(as.data.frame(data_group %>% group_by(estacao) %>%
  summarise(temp_media = mean(temp), media_usuarios = mean(total_user))))


# 4.1 Série temporal

cat("\n== 4.1 ==\n")
print(as.data.frame(data_group %>%
  group_by(mes = format(dteday, "%Y-%m")) %>%
  summarise(media = mean(total_user))))
print(data_group[which.max(data_group$total_user), c("dteday", "total_user")])
print(data_group[which.min(data_group$total_user), c("dteday", "total_user")])

g41 <- ggplot(data_group, aes(dteday, total_user)) +
  geom_line(color = "grey40") +
  geom_hline(yintercept = Q1, linetype = "dashed", color = "red") +
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%Y") +
  labs(title = "Série temporal de total_user",
       subtitle = "Linha tracejada: primeiro quartil (critério de low_usage)",
       x = "Data", y = "Total de usuários por dia") +
  theme_minimal()
salvar(g41, "q4_1_serie_temporal.png", largura = 10)


# Q 4.2  Temperatura  e condição meteorológica

data_group$faixa_temp <- cut(data_group$temp, breaks = c(0, 10, 15, 20, 25, 30, 40))
amostra10 <- head(data_group, 10)

resumo_42 <- function(df) {
  cat("-- Temperatura: mediana por faixa --\n")
  print(as.data.frame(df %>% group_by(faixa_temp) %>%
    summarise(n = n(), mediana = median(total_user),
              Q1 = quantile(total_user, 0.25), Q3 = quantile(total_user, 0.75),
              IQR = IQR(total_user))))
  cat("-- Clima: quartis e IQR --\n")
  print(as.data.frame(df %>% group_by(clima) %>%
    summarise(n = n(), Q1 = quantile(total_user, 0.25),
              mediana = median(total_user),
              Q3 = quantile(total_user, 0.75), IQR = IQR(total_user))))
}
cat("\n== 4.2: 10 observações ==\n");  resumo_42(amostra10)
cat("\n== 4.2: 300 observações ==\n"); resumo_42(data_group)

g42a <- ggplot(data_group, aes(faixa_temp, total_user)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "total_user por faixa de temperatura",
       x = "Faixa de temperatura (°C)", y = "Total de usuários por dia") +
  theme_minimal()
salvar(g42a, "q4_2_boxplot_faixas.png")

q_clima <- dados_clima %>% group_by(clima) %>%
  summarise(Q1 = quantile(total_user, 0.25), mediana = median(total_user),
            Q3 = quantile(total_user, 0.75))
g42b <- ggplot(q_clima, aes(clima, mediana)) +
  geom_col(fill = "lightblue") +
  geom_errorbar(aes(ymin = Q1, ymax = Q3), width = 0.2) +
  labs(title = "Mediana de total_user por condição meteorológica",
       subtitle = "Barras de erro: intervalo entre Q1 e Q3",
       x = "Condição meteorológica", y = "Mediana de usuários por dia") +
  theme_minimal()
salvar(g42b, "q4_2_mediana_clima.png")


#  4.3 - Temperatura x total_user por grupo de low_usage

data_group$grupo <- factor(data_group$low_usage, levels = c(0, 1),
                           labels = c("Demais dias", "low_usage"))
amostra10 <- head(data_group, 10)

resumo_43 <- function(df) {
  as.data.frame(df %>% group_by(grupo) %>%
    summarise(n = n(), temp_media = mean(temp), usuarios_media = mean(total_user),
              r = if (n() > 2) cor(temp, total_user) else NA_real_))
}
cat("\n== 4.3: 10 observações ==\n");  print(resumo_43(amostra10))
cat("\n== 4.3: 300 observações ==\n"); print(resumo_43(data_group))

g43 <- ggplot(data_group, aes(temp, total_user, color = grupo)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("Demais dias" = "steelblue",
                                "low_usage" = "darkorange")) +
  labs(title = "Temperatura x total_user por grupo",
       x = "Temperatura (°C)", y = "Total de usuários por dia", color = NULL) +
  theme_minimal() + theme(legend.position = "top")
salvar(g43, "q4_3_dispersao_grupos.png")

cat("\nAnálise concluída. Figuras salvas em", pasta_fig, "\n")
