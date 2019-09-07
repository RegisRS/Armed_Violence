library(tidyverse)
library(stringr)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/02 - Raw Data")

#Define o vetor com abreviações e regiões
abv <- read_csv("Abreviation.csv")

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")

#Define o vetor com nomes dos paises conforme tabelas da CIA
countries <- read_csv("279rank-Area.csv")

#Remover atributos não necessários
countries$Rank <- NULL
countries[,2:3] <- NULL

#Colocar paises em minusculo
abv[,1] <- str_to_lower(abv$Country)
countries <- str_to_lower(countries$Country)

#Junta tabelas para verificar problemas:
countries <- as_tibble(countries)
countries <- setNames(countries, "Country")
abvnome <- left_join(countries, abv)

#Corrigindo abreviaturas faltantes
abvnome[abvnome$Country == "russia",2] <- "RUS"
abvnome[abvnome$Country == "congo, democratic republic of the",2] <- "COD"
abvnome[abvnome$Country == "tanzania",2] <- "TZA"
abvnome[abvnome$Country == "congo, republic of the",2] <- "COG"
abvnome[abvnome$Country == "vietnam",2] <- "VNM"
abvnome[abvnome$Country == "laos",2] <- "LAO"
abvnome[abvnome$Country == "syria",2] <- "SYR"
abvnome[abvnome$Country == "korea, north",2] <- "PRK"
abvnome[abvnome$Country == "korea, south",2] <- "KOR"
abvnome[abvnome$Country == "bahamas, the",2] <- "BHS"
abvnome[abvnome$Country == "czechia",2] <- "CZE"
abvnome[abvnome$Country == "svalbard",2] <- "SJM"
abvnome[abvnome$Country == "falkland islands (islas malvinas)",2] <- "FLK"
abvnome[abvnome$Country == "gambia, the",2] <- "FLK"
abvnome[abvnome$Country == "brunei",2] <- "BRN"
abvnome[abvnome$Country == "south georgia and south sandwich islands",2] <- "SGS"
abvnome[abvnome$Country == "heard island and mcdonald islands",2] <- "HMD"
abvnome[abvnome$Country == "holy see (vatican city)",2] <- "VAT"
abvnome[abvnome$Country == "jan mayen",2] <- "SJM"
abvnome[abvnome$Country == "virgin islands",2] <- "VIR"
abvnome[abvnome$Country == "jan mayen",2] <- "SJM"
abvnome[abvnome$Country == "8	micronesia, federated states of",2] <- "FSM"
abvnome[abvnome$Country == "saint helena, ascension, and tristan da cunha",2] <- "SHN"
abvnome[abvnome$Country == "saint martin",2] <- "MAF"
abvnome[abvnome$Country == "sint maarten",2] <- "SXM"
abvnome[abvnome$Country == "burma",2] <- "MMR"
abvnome[abvnome$Country == "eswatini",2] <- "SWZ"
abvnome[abvnome$Country == "kosovo",2] <- "RKS"
abvnome[abvnome$Country == "west bank",2] <- "PSE"
abvnome[abvnome$Country == "cabo verde",2] <- "CPV"
abvnome[abvnome$Country == "micronesia, federated states of",2] <- "FSM"
abvnome[abvnome$Country == "gaza strip",2] <- "GZA"
abvnome[abvnome$Country == "dhekelia",2] <- "SBA"
abvnome[abvnome$Country == "akrotiri",2] <- "AND"
abvnome[abvnome$Country == "pitcairn islands",2] <- "PCN"
abvnome[abvnome$Country == "macau",2] <- "MAC"
abvnome[abvnome$Country == "wake island",2] <- "WAK"
abvnome[abvnome$Country == "midway islands",2] <- "MID"
abvnome[abvnome$Country == "clipperton island",2] <- "PYF"
#abvnome[abvnome$Country == "paracel islands",2] <- ""
#abvnome[abvnome$Country == "navassa island",2] <- ""
#abvnome[abvnome$Country == "ashmore and cartier islands",2] <- ""
#abvnome[abvnome$Country == "spratly islands",2] <- ""
#abvnome[abvnome$Country == "jarvis island",2] <- ""
#abvnome[abvnome$Country == "coral sea islands",2] <- ""
#abvnome[abvnome$Country == "johnston atoll",2] <- ""
#abvnome[abvnome$Country == "howland island",2] <- ""
#abvnome[abvnome$Country == "kingman reef",2] <- ""

MissABV <- filter(abvnome, is.na(Abr))
MissABV

#Salva arquivo
write_csv(abvnome, "Abreviation_CIA.csv")










