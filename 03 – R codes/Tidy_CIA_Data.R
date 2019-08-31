library(tidyverse)
library(rvest)
library(stringr)
library(htmlwidgets)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/02 - Raw Data")

#Define o vetor com nomes dos arquivos a serem lidos
ArqNome <- read_csv("CIA_File_Names.csv")

CriaDados <- function(Nome)
{
  #Define diretório de trabalho novamente, pois será alterado quando salvar o arquivo
  setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/02 - Raw Data")
  
  
  #Le os dados do arquivo html
  dados <- read_html(Nome)
  
  #Cria tabela de Dados
  tabela <- html_table(dados)
  tabela <- as.data.frame(tabela)
  tabela <- as_tibble(tabela)
  
  #Pega texo da página
  texto <- html_text(dados)
  
  #Filtra elimnar textos indesejados
  filtro1 <- str_split(texto, "— The World Factbook", simplify = TRUE)
  filtro2 <- str_split(filtro1[1], " :: ", simplify = TRUE)
  Descricao <- filtro2[2]
  
  #Remove espaços.
  Descricao <- str_trim(Descricao, side = c("both"))
  Descricao <- str_replace_all(Descricao, " ", "_")
  Descricao <- str_replace_all(Descricao, "/", "_")
  Arquivonome <- str_sub(Nome, 0, -6)
  Descricao <- paste(Arquivonome, "-" , Descricao, ".csv", sep = "")
  
  #Define diretorio para salvar
  setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")
  
  #Salva arquivo
  write_csv(tabela, Descricao)
}
sapply(ArqNome$Nome, CriaDados)










