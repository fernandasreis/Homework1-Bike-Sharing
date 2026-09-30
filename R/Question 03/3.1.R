
dados <- read.csv("data/HW1_bike_sharing.csv")
data_group <- dados[96:395, ]                     # observações 96 a 395
data_group$total_user <- data_group$casual + data_group$registered
Q1 <- quantile(data_group$total_user, 0.25)       # 3343.75
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)


# estações em nomes, na ordem do enunciado (1 = primavera)
data_group$season_name <- factor(
  data_group$season,
  levels = c(1, 2, 3, 4),
  labels = c("Primavera", "Verão", "Outono", "Inverno")
)

resumo_estacao <- function(df) {
  data.frame(
    Numero_de_dias      = as.vector(table(df$season_name)),
    Media               = tapply(df$total_user, df$season_name, mean),
    Mediana             = tapply(df$total_user, df$season_name, median),
    Desvio_Padrao       = tapply(df$total_user, df$season_name, sd),
    Dias_low_usage      = tapply(df$low_usage,  df$season_name, sum),
    Proporcao_low_usage = tapply(df$low_usage,  df$season_name, mean)
  )
}

#10 primeiras
resumo_estacao(data_group[1:10, ])

# 300 observações
resultado <- resumo_estacao(data_group)
resultado$Proporcao_percentual <- resultado$Proporcao_low_usage * 100
resultado

# boxplot
boxplot(
  total_user ~ season_name,
  data = data_group,
  main = "Utilização do sistema por estação",
  xlab = "Estação do ano",
  ylab = "Total de usuários",
  col = "lightgray"
)
