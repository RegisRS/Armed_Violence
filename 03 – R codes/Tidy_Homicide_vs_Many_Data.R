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
Destri_Renda <- read_csv("223rank-Distribution_of_family_income_-_Gini_index.csv")
Impostos <- read_csv("225rank-Taxes_and_other_revenues.csv")
Inflacao <- read_csv("229rank-Inflation_rate_(consumer_prices).csv")
Taxa_Juro <- read_csv("231rank-Commercial_bank_prime_lending_rate.csv")
Exp <- read_csv("239rank-Exports.csv")
Imp <- read_csv("242rank-Imports.csv")
Area <- read_csv("279rank-Area.csv")
Idade_Media <- read_csv("343rank-Median_age.csv")
Desempregados_Jovens <- read_csv("373rank-Unemployment,_youth_ages_15-24.csv")

colnames(EduHom)[4] <-  "Homicides_100000"
colnames(EduHom)[5] <-  "Inv_Edu_perc_GDP"
colnames(EduHom)[6] <-  "GDP_US_Dollar"
colnames(GunHom)[2] <-  "Gun_per_100_peop"





