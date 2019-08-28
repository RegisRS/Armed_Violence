library(tidyverse)
library(rvest)
library(stringr)
library(htmlwidgets)
library(purrr)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/02 - Raw Data")

#Define o vetor com nomes dos arquivos a serem lidos
Originalvend <- read_csv("VetorEnd.csv")
NomePaises <- str_sub(Originalvend$tmp, 43, -1)
Abreviation <- read_csv("Abreviation.csv")
AbreviationReg <- read_csv("AbreviationReg.csv")
  
#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")
Gun_per_100 <- read_csv("01_Summary_Gun_per_100.csv")
Homicide_per_100000 <- read_csv("02_Sumnary_Homicide_per_100000.csv")

#Filtra valores anteriores a 10 anos (menor que 2009)
Gun_per_100 <- filter(Gun_per_100, Year >= 2009)
Homicide_per_100000 <- filter(Homicide_per_100000, Year >= 2009)

#Gerando media e desvio padrao agrupado por pais
Gun_per_100 <- Gun_per_100 %>% group_by(Country) %>% 
  summarize(GunMed = mean(Gun_per_100_Value), GunDes = sd(Gun_per_100_Value), GunQTD = n())
Homicide_per_100000 <- Homicide_per_100000 %>% group_by(Country) %>% 
  summarize(HomMed = mean(Homicide_per_100000_Value), HomDes = sd(Homicide_per_100000_Value), HomQTD = n())

#Juntando os valores 
Dados <- full_join(Gun_per_100, Homicide_per_100000)

#Filtrando valores NA em Gun e Homicides
Dados <- filter(Dados, !is.na(Dados$GunMed) & !is.na(Dados$HomMed))

#Adicionar Abreviacoes 
abv <- tibble(Country = str_to_lower(Abreviation$Country), ABV = Abreviation$Abr)

abv <- tibble(Country = str_replace_all(abv$Country, " ", "-"), ABV = Abreviation$Abr)
Dados <- left_join(Dados, abv)

#Corrigindo abreviaturas faltantes
MissABV <- filter(Dados, is.na(ABV))
Dados[str_detect(Dados$Country, as.character(MissABV[2,1])),8] <- "COD"
Dados[str_detect(Dados$Country, as.character(MissABV[4,1])),8] <- "KOR"
Dados[str_detect(Dados$Country, as.character(MissABV[5,1])),8] <- "KNA"
Dados[str_detect(Dados$Country, as.character(MissABV[6,1])),8] <- "LCA"
Dados[str_detect(Dados$Country, as.character(MissABV[9,1])),8] <- "VGB"
Dados[str_detect(Dados$Country, as.character(MissABV[10,1])),8] <- "VIR"
MissABV <- filter(Dados, is.na(ABV))

for (i in 1:nrow(MissABV))
{
  Dados[str_detect(Dados$Country, as.character(MissABV[i,1]) ),8] <- abv[str_detect(abv$Country, as.character(MissABV[i,1])),2]
  print(Dados[str_detect(Dados$Country, as.character(MissABV[i,1]) ),])
}

#Adicionado Continentes aos paises
Dados <- left_join(Dados, AbreviationReg)

#Salva dados da tabela gerada
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")
write_csv(Dados, "03_Summary_Gun_Homicide.csv")

#Gerendo Grafico Completo
Grafico <- Dados %>% ggplot(aes(GunMed, HomMed, label = ABV, col = Continente)) + geom_point(size = 3)  +
  geom_text(nudge_x = 5) + 
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico


#Grafico sem Legenda 
Grafico <- Dados %>% ggplot(aes(GunMed, HomMed, col = Continente)) + geom_point(size = 2)  +
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico por Continente
Grafico <- Dados %>% ggplot(aes(GunMed, HomMed, col = Continente)) + geom_point(size = 1)  +
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico + facet_grid(Continente ~ .)

#Removendo USA e HND,pois foi considerado OutLayer
NDados <- Dados[Dados$ABV != "USA",]
NDados <- NDados[NDados$ABV != "HND",]

Grafico <- NDados %>% ggplot(aes(GunMed, HomMed, col = Continente)) + geom_point(size = 2)  +
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 pessoas") +
  ggtitle("Mortes por arma de fogo")
Grafico
  
Grafico <- NDados %>% ggplot(aes(GunMed, HomMed, col = Continente)) + geom_point(size = 1)  +
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 pessoas") +
  ggtitle("Mortes por arma de fogo")
Grafico + facet_grid(. ~ Continente)

  
#Grpáficos por Continente
AF <- filter(Dados, Continente == "Africa")
AN <- filter(Dados, Continente == "America_do_Norte")
AS <- filter(Dados, Continente == "America_do_Sul")
ASI <- filter(Dados, Continente == "Asia")
EU <- filter(Dados, Continente == "Europa")
OC <- filter(Dados, Continente == "Oceania")

GAF <- AF %>% ggplot(aes(GunMed, HomMed, label = ABV)) + geom_point(size = 3, col = "red")  +
  geom_text(nudge_x = 0.7) + 
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Africa") 
GAN <- AN %>% ggplot(aes(GunMed, HomMed, label = ABV)) + geom_point(size = 3, col = "gold")  +
  geom_text(nudge_x = 4.5) + 
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Norte")
GAS <- AS %>% ggplot(aes(GunMed, HomMed, label = ABV)) + geom_point(size = 3, col = "darkgreen")  +
  geom_text(nudge_x = 1.2) + 
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Sul")
GASI <- ASI %>% ggplot(aes(GunMed, HomMed, label = ABV)) + geom_point(size = 3, col = "dimgray")  +
  geom_text(nudge_x = 1.2) + 
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Asia")
GEU <- EU %>% ggplot(aes(GunMed, HomMed, label = ABV)) + geom_point(size = 3, col = "coral")  +
  geom_text(nudge_x = 1.3) + 
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Europa")
GOC <- OC %>% ggplot(aes(GunMed, HomMed, label = ABV)) + geom_point(size = 3, col = "darkmagenta")  +
  geom_text(nudge_x = 1.1) + 
  xlab("Taxa de posse de arma de fogo civil por 100 habitantes") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Oceania")

GAF
GAN
GAS
GASI
GEU
GOC

