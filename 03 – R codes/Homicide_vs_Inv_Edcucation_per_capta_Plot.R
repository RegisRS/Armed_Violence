library(tidyverse)
library(stringr)

#Define diretório de trabalho
setwd("C:/Users/regis/Desktop/R_Projects/Armed_Violence/04 – Tidy Data")

#Carrega arquivos de a serem trabalhados
EduHom <- read_csv("05_Summary_Education_expenditures.csv")
gdp <- read_csv("208rank-GDP_(purchasing_power_parity).csv")
pop <- read_csv("335rank-Population.csv")
abv <- read_csv("Abreviation_CIA.csv")

#Elimina colunas que nao serao usadas
gdp$Rank <- NULL
gdp$Date.of.Information <- NULL
pop$Rank <- NULL
pop$Date.of.Information <- NULL

#Acerta o nome dos atributos
colnames(gdp)[2] <-  "US_Dollar"
colnames(abv)[2] <- "ABV"

#Converter coluna em número
gdp$US_Dollar <- parse_number(gdp$US_Dollar)

#Coloca tudo em minusculo
gdp$Country <- str_to_lower(gdp$Country)
pop$Country <- str_to_lower(pop$Country)

#Adicionar abreviação as tabelas que não tem
gdp <- left_join(gdp, abv)
pop <- left_join(pop, abv)

#Junta as tabelas
Dados <- left_join(EduHom, gdp, by = "ABV")
Dados <- left_join(Dados, pop, by = "ABV")

#Remove coluna pais duplicada e renomea
Dados$Country.y <- NULL
Dados$Country <- NULL
colnames(Dados)[1] <-  "Country"

#Calculando investimento per capto em educação
Dados$Inv_per_cap <- ((Dados$PIB_PORCENTO/100)*Dados$US_Dollar)/Dados$Population

#Coloca a primeira letra do pais em mauscula
Dados$Country <- str_to_title(Dados$Country)

#Salva dados da tabela gerada
write_csv(Dados, "06_Summary_Homicide_Inv_Education_per_capta.csv")

#Gerendo Grafico Completo
Grafico <- Dados %>% ggplot(aes(Inv_per_cap, HomMed, label = ABV, col = Continente)) + geom_point(size = 3)  +
  geom_text(nudge_x = 250) + 
  xlab("Investimento em Educação por habitante em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico sem Legenda 
Grafico <- Dados %>% ggplot(aes(Inv_per_cap, HomMed, col = Continente)) + geom_point(size = 2)  +
  xlab("Investimento em Educação por habitante em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo")
Grafico

#Grafico por Continente
Grafico <- Dados %>% ggplot(aes(Inv_per_cap, HomMed, col = Continente)) + geom_point(size = 1)  +
  xlab("Investimento em Educação por habitante em US$") +
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

GAF <- AF %>% ggplot(aes(Inv_per_cap, HomMed, label = ABV)) + geom_point(size = 3, col = "red")  +
  geom_text(nudge_x = 40) + 
  xlab("Investimento em Educação por habitante em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Africa") 
GAN <- AN %>% ggplot(aes(Inv_per_cap, HomMed, label = ABV)) + geom_point(size = 3, col = "gold")  +
  geom_text(nudge_x = 120) + 
  xlab("Investimento em Educação por habitante em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Norte")
GAS <- AS %>% ggplot(aes(Inv_per_cap, HomMed, label = ABV)) + geom_point(size = 3, col = "darkgreen")  +
  geom_text(nudge_x = 30) + 
  xlab("Investimento em Educação por habitante em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - America do Sul")
GASI <- ASI %>% ggplot(aes(Inv_per_cap, HomMed, label = ABV)) + geom_point(size = 3, col = "dimgray")  +
  geom_text(nudge_x = 200) + 
  xlab("Investimento em Educação por habitante em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Asia")
GEU <- EU %>% ggplot(aes(Inv_per_cap, HomMed, label = ABV)) + geom_point(size = 3, col = "coral")  +
  geom_text(nudge_x = 210) + 
  xlab("Investimento em Educação por habitante em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Europa")
GOC <- OC %>% ggplot(aes(Inv_per_cap, HomMed, label = ABV)) + geom_point(size = 3, col = "darkmagenta")  +
  geom_text(nudge_x = 80) + 
  xlab("Investimento em Educação por habitante em US$") +
  ylab("Taxa de homicidios por 100.000 habitantes") +
  ggtitle("Mortes por arma de fogo - Oceania")

GAF
GAN
GAS
GASI
GEU
GOC
