library(tidyverse)
library(stringr)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")

#Carrega arquivos de a serem trabalhados
Homicide_per_100000 <- read_csv("03_Summary_Gun_Homicide.csv")
Imposto <- read_csv("225rank-Taxes_and_other_revenues.csv")
abv <- read_csv("Abreviation_CIA.csv")

#Elimina colunas que nao serao usadas
Homicide_per_100000$GunMed <- NULL
Homicide_per_100000$GunDes <- NULL
Homicide_per_100000$GunQTD <- NULL
Homicide_per_100000$HomDes <- NULL
Homicide_per_100000$HomQTD <- NULL
Homicide_per_100000$rankGun <- NULL
Homicide_per_100000$rankHom <- NULL
Imposto$Date.of.Information <- NULL
Imposto$Rank <- NULL

#Acerta o nome dos atributos
colnames(Imposto)[2] <-  "Imposto_perc_PIB"
colnames(abv)[2] <- "ABV"

#Coloca tudo em minusculo
Imposto$Country <- str_to_lower(Imposto$Country)


#Adicionar abreviação ao Renda per capta
Imposto <- left_join(Imposto, abv)

#Junta as duas tabelas
Dados <- left_join(Homicide_per_100000, Imposto, by = "ABV")

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
write_csv(Dados, "13_Summary_Homicide_Tax.csv")

#Gerendo Grafico Completo
Grafico <- Dados %>% ggplot(aes(Imposto_perc_PIB, HomMed, label = ABV, col = Continente)) + geom_point(size = 3)  +
  geom_text(nudge_x = 5) + 
  xlab("Impostos % PIB") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico sem Legenda 
Grafico <- Dados %>% ggplot(aes(Imposto_perc_PIB, HomMed, col = Continente)) + geom_point(size = 2)  +
  xlab("Impostos % PIB") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico por Continente
Grafico <- Dados %>% ggplot(aes(Imposto_perc_PIB, HomMed, col = Continente)) + geom_point(size = 1)  +
  xlab("Impostos % PIB") +
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

GAF <- AF %>% ggplot(aes(Imposto_perc_PIB, HomMed, label = ABV)) + geom_point(size = 3, col = "red")  +
  geom_text(nudge_x = 1) + 
  xlab("Impostos % PIB") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Africa") 
GAN <- AN %>% ggplot(aes(Imposto_perc_PIB, HomMed, label = ABV)) + geom_point(size = 3, col = "gold")  +
  geom_text(nudge_x = 1.9) + 
  xlab("Impostos % PIB") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Norte")
GAS <- AS %>% ggplot(aes(Imposto_perc_PIB, HomMed, label = ABV)) + geom_point(size = 3, col = "darkgreen")  +
  geom_text(nudge_x = 1.1) + 
  xlab("Impostos % PIB") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Sul")
GASI <- ASI %>% ggplot(aes(Imposto_perc_PIB, HomMed, label = ABV)) + geom_point(size = 3, col = "dimgray")  +
  geom_text(nudge_x = 1.3) + 
  xlab("Impostos % PIB") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Asia")
GEU <- EU %>% ggplot(aes(Imposto_perc_PIB, HomMed, label = ABV)) + geom_point(size = 3, col = "coral")  +
  geom_text(nudge_x = 1.9) + 
  xlab("Impostos % PIB") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Europa")
GOC <- OC %>% ggplot(aes(Imposto_perc_PIB, HomMed, label = ABV)) + geom_point(size = 3, col = "darkmagenta")  +
  geom_text(nudge_x = 0.7) + 
  xlab("Impostos % PIB") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Oceania")

GAF
GAN
GAS
GASI
GEU
GOC
