data_group$total_user <- data_group$casual + data_group$registered

modefunc <- function(x){ 
  valores_unicos <- unique(x) 
  posicoes <- match(x,valores_unicos) 
  posi_frequencia <- tabulate(posicoes) 
  posi_moda <- which.max(posi_frequencia) 
  valor_moda <- valores_unicos[posi_moda] 
  
  return(valor_moda) 
}

#PARA VERIFICAÇÃO DOS CÁLCULOS MANUAIS ----------------------------------------------
data_group10 <- data_group[1:10, ]

#temp (10 primeiros)
mean(data_group10$temp) #17.86
median(data_group10$temp) #17.7
any(duplicated(data_group10$temp)) #FALSE 
modefunc(data_group10$temp) #16 :mas não há moda!

#casual (10 primeiros)
mean(data_group10$casual) #571.5
median(data_group10$casual) #550
any(duplicated(data_group10$casual)) #FALSE
modefunc(data_group10$casual) #413 :mas não há moda!

#registered (10 primeiros)
mean(data_group10$registered) #2099.1
median(data_group10$registered) #2174
any(duplicated(data_group10$registered)) #FALSE
modefunc(data_group10$registered) #2395 :mas não há moda!

#total_user(10 primeiros)
mean(data_group10$total_user) #2670.7
median(data_group10$total_user) #2851.5
any(duplicated(data_group10$total_user)) #FALSE
modefunc(data_group10$total_user) #2808 :mas não há moda!

#PARA O DATA_GROUP INTEIRO ----------------------------------------------

#temp
mean(data_group$temp) #21.795
median(data_group$temp) #22.15
any(duplicated(data_group$temp)) #TRUE 
modefunc(data_group$temp) #26 :moda real

#casual
mean(data_group$casual) #766.6633
median(data_group$casual) #677
any(duplicated(data_group$casual)) #TRUE
modefunc(data_group$casual) #775 :moda real

#registered
mean(data_group$registered) #3140.84
median(data_group$registered) #3308
any(duplicated(data_group$registered)) #TRUE
modefunc(data_group$registered) #1707 :moda real

#total_user
mean(data_group$total_user) #3907.503
median(data_group$total_user) #4098
any(duplicated(data_group$total_user)) #TRUE
modefunc(data_group$total_user) #5312: moda real
