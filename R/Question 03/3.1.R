# leitura da base de dados
dados <- read.csv(file.choose())

# criação da variável total_user
dados$total_user <- dados$casual + dados$registered

# seleção das 300 observações do grupo
data_group <- dados[96:395, ]

# calcular o primeiro quartil
Q1 <- quantile(data_group$total_user, 0.25)

# criar a variável low_usage
data_group$low_usage <- ifelse(
  data_group$total_user < Q1, 1, 0
)

# estações em nomes
data_group$season_name <- factor(
  data_group$season,
  levels = c(1, 2, 3, 4),
  labels = c("Primavera", "Verão", "Outono", "Inverno")
)

# cálculo das estatísticas por estação

media <- tapply(
  data_group$total_user,
  data_group$season_name,
  mean
)

mediana <- tapply(
  data_group$total_user,
  data_group$season_name,
  median
)

desvio_padrao <- tapply(
  data_group$total_user,
  data_group$season_name,
  sd
)

numero_dias <- table(data_group$season_name)

numero_low_usage <- tapply(
  data_group$low_usage,
  data_group$season_name,
  sum
)

proporcao_low_usage <- tapply(
  data_group$low_usage,
  data_group$season_name,
  mean
)

# tabela com os resultados

resultado <- data.frame(
  Estacao = names(media),
  Numero_de_dias = as.vector(numero_dias),
  Media = as.vector(media),
  Mediana = as.vector(mediana),
  Desvio_Padrao = as.vector(desvio_padrao),
  Dias_low_usage = as.vector(numero_low_usage),
  Proporcao_low_usage = as.vector(proporcao_low_usage)
)

resultado

# proporção em porcentagem

resultado$Proporcao_percentual <-
  resultado$Proporcao_low_usage * 100

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
