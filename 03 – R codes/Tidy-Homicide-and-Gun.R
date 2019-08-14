library(tidyverse)
library(rvest)
library(stringr)
library(htmlwidgets)
library(purrr)

#Configura o Internet Explorer para abir sites
options(browser="C:\\Program Files\\internet explorer\\iexplore.exe")


#Define diretório de trabalho
setwd("C:/Users/aaaaaaaaaaaaaaaaaaaa/Desktop/R_Projects/Armed_Violence/Armed_Violence/02 - Raw Data")

#Define o vetor com nomes dos arquivos a serem lidos
Originalvend <- read_csv("VetorEnd.csv")
vend <- str_sub(Originalvend$tmp, 43, -1)

CriaDados <- function(end)
{
  #Define novamente o diretório de trabalho, pois vai ser alterado no vim da função para gravação
  setwd("C:/Users/aaaaaaaaaaaaaaaaaaaa/Desktop/R_Projects/Armed_Violence/Armed_Violence/02 - Raw Data")
  
  #Remove http... do nome dos arquivos para deixar apenas o nome do pais
  Nome <- end
  
  #Adiciona a exteção do arquivo a ser lido, nesse caso csv
  NomeArq <- paste(Nome, ".csv", sep = "")
  
  #Le o arquivo raw tipo csv correspondente ao país slecionado 
  vdata <- read_csv(NomeArq)
  
  #Tranforma a informação em tibble
  vdata <- as.data.frame(vdata)
  
  #Verifica e existencia dos dados e a posição. Retorna posição como boleano
  pos_Gun <- str_detect(vdata$VetorTexto, "Civilian Firearm Possession per 100")
  pos_Hom <- str_detect(vdata$VetorTexto, "of Gun Homicide per 100,000")
  
  #Aplicamos filtro para remover infomrações não necessárias. O filtro abaixo funciona apenas para esses casos
  Gun_per_100 <- str_split(vdata$VetorTexto[pos_Gun], " is ", simplify = TRUE)
  Homicide_per_100000 <- str_split(vdata$VetorTexto[pos_Hom], " is ", simplify = TRUE)
 
  #Enderaçar as strings a uma string, pois quando é filtrado ainda fica com duas colunas, sendo uma inútil
  Gun_per_100 <- Gun_per_100[2]
  Homicide_per_100000 <- Homicide_per_100000[2]  
  
  #Separando valore de datas. Esses filtro só funcionam para esse caso especifico
  Gun_per_100_date <- str_extract_all(Gun_per_100, "\\d{4}:") 
  Gun_per_100_valor <- str_remove_all(Gun_per_100, "\\d{4}:")
  Homicide_per_100000_date <- str_extract_all(Homicide_per_100000, "\\d{4}:")
  Homicide_per_100000_valor <- str_remove_all(Homicide_per_100000, "\\d{4}:")

  #Transforma os dados em data frame para serem trabalhados
  Gun_per_100_date <- as.data.frame(Gun_per_100_date, stringsAsFactors = FALSE)
  Homicide_per_100000_date<- as.data.frame(Homicide_per_100000_date, stringsAsFactors = FALSE)
  
  #Renomea as colunas para datas
  Gun_per_100_date <- setNames(Gun_per_100_date, "Date")
  Homicide_per_100000_date <- setNames(Homicide_per_100000_date, "Date")
  
  #Nova rodada para separar e limpara ":" indesejado
  Gun_per_100_valor <- str_trim(Gun_per_100_valor)
  Homicide_per_100000_valor <- str_trim(Homicide_per_100000_valor)
  Gun_per_100_valor <- str_split(Gun_per_100_valor, " ")
  Gun_per_100_date <- str_remove(Gun_per_100_date$Date , ":")
  Homicide_per_100000_valor <- str_split(Homicide_per_100000_valor, "\\s")
  Homicide_per_100000_date <- str_remove(Homicide_per_100000_date$Date , ":")
  
  #Transforma os dados em tibble
  Gun_per_100_date <- as_tibble(Gun_per_100_date, .name_repair = "unique")
  Gun_per_100_valor <- as_tibble(Gun_per_100_valor, .name_repair = "unique")
  Homicide_per_100000_date <- as_tibble(Homicide_per_100000_date, .name_repair = "unique")
  Homicide_per_100000_valor <- as_tibble(Homicide_per_100000_valor, .name_repair = "unique")
  
  
  #Cria um list unico para Gun_per_100 adicinando, nome do pais, ano e valores
  Gun_per_100 <- list(Country = Nome, 
                        Year = parse_integer(Gun_per_100_date$value), 
                        Gun_per_100_Value = parse_double(Gun_per_100_valor$...1))
  
  #Cria um list unico para Homicide_per_100000 adicinando, nome do pais, ano e valores
  Homicide_per_100000 <- list(Country = Nome, 
                                Year = parse_integer(Homicide_per_100000_date$value), 
                                Homicide_per_100000_Value = parse_double(Homicide_per_100000_valor$...1))
  
  #Retorna Valor em lista dos dois indices
  return(list(Gun_per_100, Homicide_per_100000))

}


#Executa a funcao para cada elemento do vetor de nomes/endereços
Sumnary_list <- sapply(vend, CriaDados)


Summary_Gun_per_100 <- as_tibble(Sumnary_list[[1,1]])
Sumnary_Homicide_per_100000 <- as_tibble(Sumnary_list[[2,1]])


for (i in 2:length(Sumnary_list[1,])) 
{
  tmp_1 <- as_tibble(Sumnary_list[[1,i]]) 
  Summary_Gun_per_100 <- bind_rows(Summary_Gun_per_100, tmp_1)
  
  tmp_2 <- as_tibble(Sumnary_list[[2,i]]) 
  Sumnary_Homicide_per_100000 <- bind_rows(Sumnary_Homicide_per_100000, tmp_2)
}

#Remove variáveis temporárias
remove(tmp_1)
remove(tmp_2)

#Os seguinte paises foram alterados os RAW files pis os dados estavam com "Between":
#Nova Zelandia, Su��a e Philipinas

#Verificar erros de valores altos (nesse caso tomou-se o US e Brazil como base) 
filter(Summary_Gun_per_100, Summary_Gun_per_100$Gun_per_100_Value  > 150)
filter(Sumnary_Homicide_per_100000, 
       Sumnary_Homicide_per_100000$Homicide_per_100000_Value  > 50)

#Corrige os erros com altos valores. Nesse caso apenas cyprus e montserrat estav�o errados
Summary_Gun_per_100[Summary_Gun_per_100$Country == "cyprus" & 
                      Summary_Gun_per_100$Gun_per_100_Value == 341, 3] = 34.0

Sumnary_Homicide_per_100000[
  Sumnary_Homicide_per_100000$Country == "montserrat" & 
    Sumnary_Homicide_per_100000$Homicide_per_100000_Value == 2553, 3] = 255

#Verificando problemas das observa��es num�ricas, normalmente o n�meros muito afetados
#n�o ter�o casas decimais assim podemos procurar:
tmp_1 <- filter(Summary_Gun_per_100, Summary_Gun_per_100$Gun_per_100_Value - 
                  as.integer(Summary_Gun_per_100$Gun_per_100_Value) == 0)

tmp_2 <- filter(Sumnary_Homicide_per_100000, Sumnary_Homicide_per_100000$Homicide_per_100000_Value - 
                  as.integer(Sumnary_Homicide_per_100000$Homicide_per_100000_Value) == 0)


#Corrige dados de Summary_Gun_per_100 influenciados pelas observa��es do site: belize, chad, chad, costa-rica,      
#cyprus, el-salvador, estonia, ghana, ghana, guinea, moldova, nauru, papua-new-guinea,
#peru, senegal, taiwan e  tonga           
NewValue_1 <- c(10, 1, 10, 34, 12, 5, 8, 2, 1, 3, 0,  1,  2,  2,  0,  8)

for (i in 1:nrow(tmp_1)) 
    {
    Summary_Gun_per_100[Summary_Gun_per_100$Country == tmp_1$Country[i] & 
                          Summary_Gun_per_100$Gun_per_100_Value == tmp_1$Gun_per_100_Value[i], 3] = NewValue_1[i]
  }


#Abrir site para verificar o que esta acotecendo com valores de tmp_2
tmp_3 <- tmp_2 %>% group_by(Country) %>% summarize()

for(i in 1:length(tmp_3$Country))
{
  CheckSite <- filter(Originalvend, 
         Originalvend$tmp == paste("https://www.gunpolicy.org/firearms/region/", tmp_3$Country[i], sep = "" ))
  browseURL(CheckSite$tmp)
}

#Corrige dados de Sumnary_Homicide_per_100000 influenciados pelas observa��es do site:
NewValue_2 <- c(0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 1, 1, 3, 0, 0, 1, 0, 0,
                0, 0, 0, 0, 0, 0, 21, 0,  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 24, 26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15)

for (i in 1:nrow(tmp_2)) 
{
  Sumnary_Homicide_per_100000[Sumnary_Homicide_per_100000$Country == tmp_2$Country[i] & 
                                Sumnary_Homicide_per_100000$Homicide_per_100000_Value == tmp_2$Homicide_per_100000_Value[i], 3] = NewValue_2[i]
}


#Define local para salvar dados
setwd("C:/Users/aaaaaaaaaaaaaaaaaaaa/Desktop/R_Projects/Armed_Violence/Armed_Violence/04 - Tidy Data")

#Salva dados Gun_per_100 em arquivo csv
write_csv(Summary_Gun_per_100, "01_Summary_Gun_per_100.csv")

#Salva dados Homicide_per_100000 do pais selecinado em arquivo csv
write_csv(Sumnary_Homicide_per_100000, "02_Sumnary_Homicide_per_100000.csv")

