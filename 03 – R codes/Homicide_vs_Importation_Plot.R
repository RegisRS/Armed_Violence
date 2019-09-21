library(tidyverse)
library(stringr)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")

#Carrega arquivos de a serem trabalhados
Homicide_per_100000 <- read_csv("03_Summary_Gun_Homicide.csv")
Imp <- read_csv("242rank-Imports.csv")
abv <- read_csv("Abreviation_CIA.csv")

#Elimina colunas que nao serao usadas
Homicide_per_100000$GunMed <- NULL
Homicide_per_100000$GunDes <- NULL
Homicide_per_100000$GunQTD <- NULL
Homicide_per_100000$HomDes <- NULL
Homicide_per_100000$HomQTD <- NULL
Homicide_per_100000$rankGun <- NULL
Homicide_per_100000$rankHom <- NULL
Imp$Date.of.Information <- NULL
Imp$Rank <- NULL

#Acerta o nome dos atributos
colnames(Imp)[2] <-  "Import_Dolar"
colnames(abv)[2] <- "ABV"

#Converter coluna em número
Imp$Import_Dolar <- parse_number(Imp$Import_Dolar)

#Coloca tudo em minusculo
Imp$Country <- str_to_lower(Imp$Country)

#Adicionar abreviação ao Renda per capta
Imp <- left_join(Imp, abv)

#Junta as duas tabelas
Dados <- left_join(Homicide_per_100000, Imp, by = "ABV")

#Excluindo paises sem dados de renda per capta
Dados <- filter(Dados, !is.na(Dados$Country.y))

#Remove coluna pais não duplicada e renomea
Dados$Country.y <- NULL
colnames(Dados)[1] <-  "Country"

#Reordenar colunas
Dados <- Dados[,c(1,3,4,2,5)]

#Coloca a primeira letra do pais em mauscula
Dados$Country <- str_to_title(Dados$Country)

#Salva dados da tabela gerada
write_csv(Dados, "17_Summary_Homicide_Import.csv")

#Gerendo Grafico Completo
Grafico <- Dados %>% ggplot(aes(Import_Dolar, HomMed, label = ABV, col = Continente)) + geom_point(size = 3) +
  geom_text(nudge_x = 5) + 
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlab("Exportacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico sem Legenda 
Grafico <- Dados %>% ggplot(aes(Import_Dolar, HomMed, col = Continente)) + geom_point(size = 2)  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlab("Exportacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico por Continente
Grafico <- Dados %>% ggplot(aes(Import_Dolar, HomMed, col = Continente)) + geom_point(size = 1)  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlab("Exportacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico + facet_grid(Continente ~ .)

#Grpáficos por Continente
AF <- filter(Dados, Continente == "Africa")
AN <- filter(Dados, Continente == "America_do_Norte")
AS <- filter(Dados, Continente == "America_do_Sul")
ASI <- filter(Dados, Continente == "Asia")
EU <- filter(Dados, Continente == "Europa")
OC <- filter(Dados, Continente == "Oceania")

GAF <- AF %>% ggplot(aes(Import_Dolar, HomMed, label = ABV)) + geom_point(size = 3, col = "red")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 3000000000) + 
  xlab("Importacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Africa") 
GAN <- AN %>% ggplot(aes(Import_Dolar, HomMed, label = ABV)) + geom_point(size = 3, col = "gold")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 90000000000) + 
  xlab("Importacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Norte")
GAS <- AS %>% ggplot(aes(Import_Dolar, HomMed, label = ABV)) + geom_point(size = 3, col = "darkgreen")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 7000000000) + 
  xlab("Importacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Sul")
GASI <- ASI %>% ggplot(aes(Import_Dolar, HomMed, label = ABV)) + geom_point(size = 3, col = "dimgray")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 30000000000) + 
  xlab("Importacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Asia")
GEU <- EU %>% ggplot(aes(Import_Dolar, HomMed, label = ABV)) + geom_point(size = 3, col = "coral")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 50000000000) + 
  xlab("Importacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Europa")
GOC <- OC %>% ggplot(aes(Import_Dolar, HomMed, label = ABV)) + geom_point(size = 3, col = "darkmagenta")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 10000000000) + 
  xlab("Importacoes em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Oceania")

GAF
GAN
GAS
GASI
GEU
GOC

