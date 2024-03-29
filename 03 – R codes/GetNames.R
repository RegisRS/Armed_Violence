#Carrega bibliotecas necess�rias
library(tidyverse)
library(rvest)
library(stringr)
library(htmlwidgets)

#Endere�o da p�gina a ser analisada
url <- "https://www.gunpolicy.org/"

#Ler a p�gina que vai ser analisada
dados <- read_html(url)

#Transforma os dado em uma tabela e verifica a classe
tabela <- html_form (dados)
str(tabela)

#Seleciona o dados que s�o pertinentes para a an�lise, foi analisado no console a sele��o abaixo � resultado
tmp <- tabela[[2]]$fields$country$options

#Transforma os dado sem um data frame
tmp <- as.data.frame(tmp, stringsAsFactors = FALSE)

#Filtra para eliminar linhas em branco
tmp <- filter(tmp, tmp != "")

#Cria uma fun��o para trabalhar as observa��e do data frame
ajunta <- function(v)
{
  v <- str_replace_all(v, " ", "-")
  v <- str_replace_all(v, "&", "and")
  v <- str_to_lower(v)
  paste("https://www.gunpolicy.org/firearms/region/",v, sep = "")
}

#Aplica a fun��o ao data frame
tmp2 <- sapply(tmp, ajunta)

#Converte o resultado para data frame, pois muda de classe
tmp3 <- as.data.frame(tmp2, stringsAsFactors = FALSE)

#Grava dado em arquivo, esse foi usado para avaliar se todas as conex�es estavam certas.
#write_csv(tmp3, "VetorEnd.csv")

#Depois de analisar os endere�os gerados foi detectado um erro de 3% ou 7 endere�os, vamos corrigir esses.  
#o write acima vai ser comentado.

tmp3$tmp[46] <- "https://www.gunpolicy.org/firearms/region/congo-drc"
tmp3$tmp[47] <- "https://www.gunpolicy.org/firearms/region/congo-roc"
tmp3$tmp[50] <- "https://www.gunpolicy.org/firearms/region/cote-d-ivoire"
tmp3$tmp[132] <- "https://www.gunpolicy.org/firearms/region/micronesia-fsm"
tmp3$tmp[169] <- "https://www.gunpolicy.org/firearms/region/reunion"
tmp3$tmp[225] <- "https://www.gunpolicy.org/firearms/region/virgin-islands-uk"
tmp3$tmp[226] <- "https://www.gunpolicy.org/firearms/region/virgin-islands-us"

#Grava Vetor com endere�os
write_csv(tmp3, "VetorEnd.csv")



