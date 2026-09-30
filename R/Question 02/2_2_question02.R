# Medidas de tendência central

dados <- read.csv("data/HW1_bike_sharing.csv")
data_group <- dados[96:395, ]                     # observações 96 a 395
data_group$total_user <- data_group$casual + data_group$registered
Q1 <- quantile(data_group$total_user, 0.25)       # 3343.75
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

modefunc <- function(x) {
  valores_unicos <- unique(x)
  posicoes <- match(x, valores_unicos)
  posi_frequencia <- tabulate(posicoes)
  posi_moda <- which.max(posi_frequencia)
  valor_moda <- valores_unicos[posi_moda]
  return(valor_moda)
}

# 10 primeiras observações
data_group10 <- data_group[1:10, ]

# temp
mean(data_group10$temp)                 # 17.86
median(data_group10$temp)               # 17.7
any(duplicated(data_group10$temp))      # FALSE -> não há moda

# casual
mean(data_group10$casual)               # 571.5
median(data_group10$casual)             # 550
any(duplicated(data_group10$casual))    # FALSE -> não há moda

# registered
mean(data_group10$registered)           # 2099.2
median(data_group10$registered)         # 2174
any(duplicated(data_group10$registered))# FALSE -> não há moda

# total_user
mean(data_group10$total_user)           # 2670.7
median(data_group10$total_user)         # 2851.5
any(duplicated(data_group10$total_user))# FALSE -> não há moda

# 300 observações

# temp
mean(data_group$temp)                   # 21.795
median(data_group$temp)                 # 22.15
modefunc(data_group$temp)               # 26 (6 ocorrências)

# casual
mean(data_group$casual)                 # 766.6633
median(data_group$casual)               # 677
modefunc(data_group$casual)             # 775 (3 ocorrências)

# registered
mean(data_group$registered)             # 3140.84
median(data_group$registered)           # 3308
modefunc(data_group$registered)         # vários valores com 2 ocorrências

# total_user
mean(data_group$total_user)             # 3907.503
median(data_group$total_user)           # 4098
modefunc(data_group$total_user)         # vários valores com 2 ocorrências

sapply(data_group[, c("temp", "casual", "registered", "total_user")],
       function(x) max(table(x)))
