# carregar conjunto de dados #

data <- read.csv(file.choose())

# 2. DEFINIR A MAIOR MATRÍCULA DO GRUPO #

M <- 582795

# calcular r #

r <- 1 + (M %% 100)

print(r)

data_group <- data[r:(r + 299), ]

# num de observações
nrow(data_group)


# conferir primeiras observações
head(data_group)


# conferir últimas observações
tail(data_group)


# criar total_user #

data_group$total_user <- data_group$casual + data_group$registered

head(data_group$total_user)

# medidas úteis # 

media <- mean(data_group$total_user)
mediana <- median(data_group$total_user)
variancia <- var(data_group$total_user)
desvio_padrao <- sd(data_group$total_user)
minimo <- min(data_group$total_user)
maximo <- max(data_group$total_user)
amplitude <- maximo - minimo

media
mediana
variancia
desvio_padrao
minimo
maximo
amplitude

# resumo #

summary(data_group$total_user)

# criação do histograma # 

hist(data_group$total_user,
     main = "Histograma do Total de Usuários",
     xlab = "Total de usuários por dia",
     ylab = "Frequência")

# criação do boxplot # 

boxplot(data_group$total_user,
        main = "Boxplot do Total de Usuários",
        ylab = "Total de usuários por dia") 

boxplot.stats(data_group$total_user)

# histograma e boxplot juntos # 

par(mfrow = c(1, 2))

hist(data_group$total_user,
     main = "Histograma do Total de Usuários",
     xlab = "Total de usuários por dia",
     ylab = "Frequência")

boxplot(data_group$total_user,
        main = "Boxplot do Total de Usuários",
        ylab = "Total de usuários por dia")

par(mfrow = c(1, 1))
