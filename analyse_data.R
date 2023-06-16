# Charger les données à partir du fichier CSV
data <- read.csv("new_cities.csv", sep = ",", header = TRUE)

table_1 <- table(data$descr_grav, data$descr_type_col)
print(table_1)

chi2_test1 <- chisq.test(table_1)
print(chi2_test1$p.value)

#png("ressources/mosaiqueplot_descr_grav&age.png")

mosaicplot(table_1, main = "descr_grav&age")

#dev.off()

# On réalise l'ACP-AFC-ACM sur les données avec le nombre d'accidents selon la grzvité pour 100000 habitants par région
library(tidyr)
library(dplyr)
library(FactoMineR)

# On récupère la population par région
population <- read.csv("population.csv", sep = ";", header = TRUE)

# On récupère le nombre d'accidents par région
data_nb_accident_reg <- data %>% group_by(region_name) %>% count()

# On réalise le taux d'accidents par région
population$taux_accident <- 0
population$taux_accident <- data_nb_accident_reg$n[match(gsub("-", " ", tolower(population$region_name)), gsub("-", " ", tolower(data_nb_accident_reg$region_name)))] / population$population * 100000

# On réalise l'ACP-AFC-ACM sur les données avec le nombre d'accidents selon la gravité pour 100000 habitants par région
# On récupère les données pour l'ACP-AFC-ACM
data_acp <- data %>% group_by(region_name, descr_grav) %>% count()
data_acp <- data_acp %>% spread(descr_grav, n)
data_acp <- data_acp %>% select(-region_name)

# On ajoute le taux d'accidents par région
data_acp$taux_accident <- 0
data_acp$taux_accident <- population$taux_accident[match(gsub("-", " ", tolower(data_acp$region_name)), gsub("-", " ", tolower(population$region_name)))]

# On remplace les region_name par des chiffres
data_acp$region_name <- population$region_number[match(gsub("-", " ", tolower(data_acp$region_name)), gsub("-", " ", tolower(population$region_name)))]

# On réalise l'ACP-AFC-ACM
acp <- PCA(data_acp, scale.unit = TRUE, ncp = 5, graph = FALSE)
print(summary(acp))