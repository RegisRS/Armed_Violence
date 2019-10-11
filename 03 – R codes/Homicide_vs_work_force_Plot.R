library(tidyverse)
library(stringr)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")

#Carrega arquivos de a serem trabalhados
Homicide_per_100000 <- read_csv("03_Summary_Gun_Homicide.csv")
Forca_Trabalho <- read_csv("218rank-Labor_force.csv")
abv <- read_csv("Abreviation_CIA.csv")

#Elimina colunas que nao serao usadas
Homicide_per_100000$GunMed <- NULL
Homicide_per_100000$GunDes <- NULL
Homicide_per_100000$GunQTD <- NULL
Homicide_per_100000$HomDes <- NULL
Homicide_per_100000$HomQTD <- NULL
Homicide_per_100000$rankGun <- NULL
Homicide_per_100000$rankHom <- NULL
Forca_Trabalho$Rank <- NULL
Forca_Trabalho$Date.of.Information <- NULL

#Acerta o nome dos atributos
colnames(Forca_Trabalho)[2] <-  "Forca_de_Trabalho"
colnames(abv)[2] <- "ABV"

#Coloca tudo em minusculo
Forca_Trabalho$Country <- str_to_lower(Forca_Trabalho$Country)

#Adicionar abreviação ao Renda per capta
Forca_Trabalho <- left_join(Forca_Trabalho, abv)

#Junta as duas tabelas
Dados <- left_join(Homicide_per_100000, Forca_Trabalho, by = "ABV")

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
write_csv(Dados, "11_Summary_Homicide_work_force.csv")

#Gerendo Grafico Completo
Grafico <- Dados %>% ggplot(aes(Forca_de_Trabalho, HomMed, label = ABV, col = Continente)) + geom_point(size = 3) +
  geom_text(nudge_x = 5) + 
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlab("Forca de trabalho") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico sem Legenda 
Grafico <- Dados %>% ggplot(aes(Forca_de_Trabalho, HomMed, col = Continente)) + geom_point(size = 2)  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlab("Forca de trabalho") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico por Continente
Grafico <- Dados %>% ggplot(aes(Forca_de_Trabalho, HomMed, col = Continente)) + geom_point(size = 1)  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  xlab("Forca de trabalho") +
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

GAF <- AF %>% ggplot(aes(Forca_de_Trabalho, HomMed, label = ABV)) + geom_point(size = 3, col = "red")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 1300000) + 
  xlab("Forca de trabalho") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Africa") 
GAN <- AN %>% ggplot(aes(Forca_de_Trabalho, HomMed, label = ABV)) + geom_point(size = 3, col = "gold")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 7000000) + 
  xlab("Forca de trabalho") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Norte")
GAS <- AS %>% ggplot(aes(Forca_de_Trabalho, HomMed, label = ABV)) + geom_point(size = 3, col = "darkgreen")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 4000000) + 
  xlab("Forca de trabalho") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Sul")
GASI <- ASI %>% ggplot(aes(Forca_de_Trabalho, HomMed, label = ABV)) + geom_point(size = 3, col = "dimgray")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 17000000) + 
  xlab("Forca de trabalho") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Asia")
GEU <- EU %>% ggplot(aes(Forca_de_Trabalho, HomMed, label = ABV)) + geom_point(size = 3, col = "coral")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 2000000) + 
  xlab("Forca de trabalho") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Europa")
GOC <- OC %>% ggplot(aes(Forca_de_Trabalho, HomMed, label = ABV)) + geom_point(size = 3, col = "darkmagenta")  +
  scale_x_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE)) +
  geom_text(nudge_x = 500000) + 
  xlab("Forca de trabalho") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Oceania")

GAF
GAN
GAS
GASI
GEU
GOC
