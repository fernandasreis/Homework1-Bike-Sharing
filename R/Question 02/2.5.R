# leitura da base de dados
dados <- read.csv(file.choose())

# total_user = usuários casuais + usuários registrados
dados$total_user <- dados$casual + dados$registered

# definicão das 300 observações
data_group <- dados[96:395, ]

# calculo do primeiro quartil
Q1 <- quantile(data_group$total_user, 0.25)

# criação da variável binária low_usage
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

Q1

# contar quantos dias possuem baixa utilização
numero_baixa <- sum(data_group$low_usage)
numero_baixa

# calcular a proporção de dias de baixa utilização
proporcao_baixa <- mean(data_group$low_usage)
proporcao_baixa

porcentagem_baixa <- proporcao_baixa * 100
porcentagem_baixa

# mostrar a quantidade de dias em cada categoria
table(data_group$low_usage)
