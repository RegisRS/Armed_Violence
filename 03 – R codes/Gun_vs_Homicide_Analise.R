

Gun_per_100 <- filter(Gun_per_100, Year >= 2009)
Homicide_per_100000 <- filter(Homicide_per_100000, Year >= 2009)

DataComp <- tibble(Country = Nome,
                   Gun_per_100_N = nrow(Gun_per_100),
                   Gun_per_100_Mean = mean(Gun_per_100$Value), 
                   Gun_per_100_SD = sd(Gun_per_100$Value), 
                   Homicide_per_100000_N <- nrow(Homicide_per_100000),
                   Homicide_per_100000_Mean = mean(Homicide_per_100000$Value),
                   Homicide_per_100000_SD = sd(Homicide_per_100000$Value))

