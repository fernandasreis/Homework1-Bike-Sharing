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
modefunc(data_group10$registered) #2395 :sem moda!
