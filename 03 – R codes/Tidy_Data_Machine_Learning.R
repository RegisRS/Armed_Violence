library(tidyverse)
library(stringr)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")

#Ler arquivo vetor com nome dos arquivos a serem carregados
files_vector <- read_csv("Vector_Data_Analize.csv", col_names = FALSE)

#Cria funcao para ler os arquivos
ler <- function(nome)
{
  nome <- read_csv(nome)
}

#Le arquivos e cria lista com os dados de cada um 
lista_tab <- sapply(files_vector$X1, ler)

#Cria o tibble base para fazer o join
Dados <- as_tibble(lista_tab$`03_Summary_Gun_Homicide.csv`)

#Limpa colunas indesejadas dos dados Base
Dados <- select(Dados, Country, ABV, Continente, Hom_1000 = HomMed, Gun_100 = GunMed)

#Junta tabelas dentro da lista com os dados Base
for (i in 2:length(lista_tab))
{
  tmp <- lista_tab[[i]][,c(2,ncol(lista_tab[[i]]))]
  Dados <- left_join(Dados, tmp, by = "ABV")
}

#Limpa dados com NA
for(i in 6:ncol(Dados))
{
  tmp <- !is.na(Dados[,i])
  Dados <- Dados[tmp, ]
}

#Colocar prvisor na ultima coluna conforme padrão Tidyverse
Dados <- Dados[,c(1:3,5:ncol(Dados), 4)]

#Coloca a primeira letra do pais em mauscula
Dados$Country <- str_to_title(Dados$Country)

#Salva dados da tabela gerada
write_csv(Dados, "18_Machine_Learning_data.csv")
