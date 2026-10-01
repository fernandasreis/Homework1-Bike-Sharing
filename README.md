# Homework 1 – Bike Sharing
Trabalho da disciplina TI0111 – Estatística para Engenharia.
Análise estatística de um sistema de compartilhamento de bicicletas em uma cidade dos Estados Unidos. A partir do conjunto `HW1_bike_sharing.csv` (731 observações diárias), foi trabalhado uma amostra de 300 observações consecutivas (observações 96 a 395, de 06/04/2011 a 30/01/2012), definida pela maior matrícula do grupo. O objetivo é caracterizar os dados, descrever a distribuição do número total de usuários (`total_user`) e investigar como estação do ano, condição meteorológica e temperatura estão relacionadas à utilização do sistema.
# Integrantes
Nome	e Matrícula:

Fernanda Silveira Reis	582795

Ian da Silva Torquato	578061

Vitor Policarpo Caetano	579540

Estrutura do repositório:
```
data/      conjunto de dados original (HW1_bike_sharing.csv) e amostra do grupo
R/         scripts da análise
  analise_bike_sharing.R   script unificado que reproduz todo o relatório
  Question 01..04/         scripts desenvolvidos por questão ao longo do projeto
figures/   gráficos produzidos (figures/final/ contém os gráficos do relatório)
report/    relatório final em PDF
```
Como executar
Dependências: R (versão 4.0 ou superior) e os pacotes `dplyr`, `ggplot2` e `scales`.
```r
install.packages(c("dplyr", "ggplot2", "scales"))
```
Execução:
Clone ou baixe o repositório.
No R ou no RStudio, defina a pasta raiz do repositório como diretório de trabalho (a pasta que contém `data/` e `R/`).
Execute:
```r
source("R/analise_bike_sharing.R", echo = TRUE, encoding = "UTF-8")
```
O script lê `data/HW1_bike_sharing.csv` e estrutura a amostra do grupo (`data_group`), printa no console todos os resultados usados no relatório (verificação com as 10 primeiras observações e resultados com as 300) e salva os gráficos em `figures/final/`.
Os scripts em `R/Question 01` a `R/Question 04` registram o desenvolvimento de cada questão e também podem ser executados individualmente, a partir da mesma pasta raiz, por exemplo:
```r
source("R/Question 03/3.1.R", encoding = "UTF-8")
```
O relatório final está em `report/`.
# Contribuições
Integrante	e Contribuições:

Fernanda Silveira Reis	Questão 1 Questão 2.3 , Questão 3.3 , Questão 4.1 , revisão do relatório e organização do repositório.

Ian da Silva Torquato	Questão 1 , Questões 2.1 e 2.2 , Questão 3.2 , Questão 3.4 , Questão 4.2 , relatório e organização do repositório.

Vitor Policarpo Caetano	Questão 1 , Questão 2.4, Questão 2.5 , Questão 3.1 , Questão 4.3 , Questão 4.4 , relatório e revisão do repositório.
