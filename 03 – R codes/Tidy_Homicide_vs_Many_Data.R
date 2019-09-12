library(tidyverse)
library(stringr)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")

#Carrega arquivos de a serem trabalhados
EduHom <- read_csv("06_Summary_Homicide_Inv_Education_per_capta.csv")
GunHom <- read_csv("03_Summary_Gun_Homicide.csv")
abv <- read_csv("Abreviation_CIA.csv")
#208rank já foi usado no HomEdu
#335rank já foi usado HomEdu
#369rank já foi usado HomEdu
Renda_Per_Capta <- read_csv("211rank-GDP_-_per_capita_(PPP).csv")
Forca_Trabalho <- read_csv("218rank-Labor_force.csv")
Desempregados <- read_csv("220rank-Unemployment_rate.csv")
Distri_Renda <- read_csv("223rank-Distribution_of_family_income_-_Gini_index.csv")
Impostos <- read_csv("225rank-Taxes_and_other_revenues.csv")
Inflacao <- read_csv("229rank-Inflation_rate_(consumer_prices).csv")
Taxa_Juro <- read_csv("231rank-Commercial_bank_prime_lending_rate.csv")
Exp <- read_csv("239rank-Exports.csv")
Imp <- read_csv("242rank-Imports.csv")
Area <- read_csv("279rank-Area.csv")
Idade_Media <- read_csv("343rank-Median_age.csv")
Desempregados_Jovens <- read_csv("373rank-Unemployment,_youth_ages_15-24.csv")

#Elimina colunas que nao serao usadas
Renda_Per_Capta$Rank <- NULL
Renda_Per_Capta$Date.of.Information <- NULL
Forca_Trabalho$Rank <- NULL
Forca_Trabalho$Date.of.Information <- NULL
Desempregados$Rank <- NULL
Desempregados$Date.of.Information <- NULL
Distri_Renda$Rank <- NULL
Distri_Renda$Date.of.Information <- NULL
Impostos$Rank <- NULL
Impostos$Date.of.Information <- NULL
Inflacao$Rank <- NULL
Inflacao$Date.of.Information <- NULL
Taxa_Juro$Rank <- NULL
Taxa_Juro$Date.of.Information <- NULL
Exp$Rank <- NULL
Exp$Date.of.Information <- NULL
Imp$Rank <- NULL
Imp$Date.of.Information <- NULL
Area$Rank <- NULL
Area$Date.of.Information <- NULL
Idade_Media$Rank <- NULL
Idade_Media$Date.of.Information <- NULL
Desempregados_Jovens$Rank <- NULL
Desempregados_Jovens$Date.of.Information <- NULL

#Acerta o nome dos atributos
colnames(EduHom)[4] <-  "Homicides_100000"
colnames(EduHom)[5] <-  "Inv_Edu_perc_GDP"
colnames(EduHom)[6] <-  "GDP_US_Dollar"
colnames(GunHom)[2] <-  "Gun_per_100_peop"
colnames(Renda_Per_Capta)[2] <- "Renda_Per_Capta"
colnames(Forca_Trabalho)[2] <- "Forca_Trabalho"
colnames(Desempregados)[2] <- "Desempregados"
colnames(Distri_Renda)[2] <- "Distri_Renda"
colnames(Impostos)[2] <- "Impostos"
colnames(Inflacao)[2] <- "Inflacao"
colnames(Taxa_Juro)[2] <- "Taxa_Juro"
colnames(Area)[2] <- "Area_km2"
colnames(Idade_Media)[2] <- "Idade_Media"
colnames(Desempregados_Jovens)[2] <- "Desempregados_Jovens"
colnames(Exp)[2] <- "Export_US_Dollar"
colnames(Imp)[2] <- "Import_US_Dollar"

#Converter coluna em número
Renda_Per_Capta$Renda_Per_Capta <- parse_number(Renda_Per_Capta$Renda_Per_Capta)
Desempregados$Desempregados <- Desempregados$Desempregados/100
Impostos$Impostos <- Impostos$Impostos/100
Inflacao$Inflacao <- Inflacao$Inflacao/100
Taxa_Juro$Taxa_Juro <- Taxa_Juro$Taxa_Juro/100
Exp$Export_US_Dollar <- parse_number(Exp$Export_US_Dollar)
Imp$Import_US_Dollar <- parse_number(Imp$Import_US_Dollar)
Desempregados_Jovens$Desempregados_Jovens <- parse_number(Desempregados_Jovens$Desempregados_Jovens)/100









