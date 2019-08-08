library(tidyverse)
library(rvest)
library(stringr)
library(htmlwidgets)

vurl <- read_csv("VetorEnd.csv")

#vurl <- as.data.frame (vurl)

vurl1 <- c("https://www.gunpolicy.org/firearms/region/united-states","https://www.gunpolicy.org/firearms/region/united-kingdom")

CriaDados <- function(url)
{
  Nome <- str_sub(url, 43, -1)
  Nome <- paste(Nome, ".csv", sep = "")
  
  dados <- read_html(url)
  
  tabela <- html_table(dados)
  
  tabelaDF <- as.data.frame(tabela)
  
  tabelaDF[,1] <- NULL
  
  tabelaDF <- setNames(tabelaDF, "Texto")
  
  tabelaLines <- str_split(tabelaDF$Texto,"\n" )
  
  tabelaLines <- as.data.frame(tabelaLines)
  
  crit_1 <- "Rate of Civilian Firearm Possession per 100 Population"
  crit_2 <- "Rate of Gun Homicide per 100,000 People"
  crit_3 <- "Rate of Male Gun Homicide per 100,000 People"
  crit_4 <- "Rate of Female Gun Homicide per 100,000 People"
  
  #criterio <- "[Rate of Civilian Firearm Possession per 100 Population].*?\\n"
  tabelaLines <- setNames(tabelaLines, "Texto")
  
  pos_1 <- str_detect(tabelaLines$Texto, crit_1)
  pos_2 <- str_detect(tabelaLines$Texto, crit_2)
  pos_3 <- str_detect(tabelaLines$Texto, crit_3)
  pos_4 <- str_detect(tabelaLines$Texto, crit_4)
  
  Texto_1 <- tabelaLines[pos_1,]
  Texto_2 <- tabelaLines[pos_2,]
  Texto_3 <- tabelaLines[pos_3,]
  Texto_4 <- tabelaLines[pos_4,]
  
  Gun_per_100 <- as.character(Texto_1)
  Homicide_per_100000 <- as.character(Texto_2)
  Homicide_male_per_100000 <- as.character(Texto_3)
  Homicide_female_per_100000 <- as.character(Texto_4)
  
  VetorTexto <- c(Gun_per_100, Homicide_per_100000, Homicide_male_per_100000, Homicide_female_per_100000)
  
  VetorTexto <- as.data.frame(VetorTexto, stringsAsFactors = FALSE)
  
  write_csv(VetorTexto, Nome)
}

sapply(vurl$tmp, CriaDados)