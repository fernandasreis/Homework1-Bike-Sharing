modefunc <- function(x){ 
  valores_unicos <- unique(x) 
  posicoes <- match(x,valores_unicos) 
  posi_frequencia <- tabulate(posicoes) 
  posi_moda <- which.max(posi_frequencia) 
  valor_moda <- valores_unicos[posi_moda] 
  
  return(valor_moda) 
}

#PARA VERIFICAÇÃO DOS CÁLCULOS MANUAIS
data_group10 <- data_group[1:10, ]
