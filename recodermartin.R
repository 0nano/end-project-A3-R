#---------------PACKAGES---------------#
#install.packages("tidyverse")

#---------------LECTURE---------------#
nb <- 74000 # Nombre de lignes à lire
# On lit le fichier csv et on crée les données
data <- read.csv("stat_acc_V3.csv", sep=";", nrows=nb)


#---------------RECODER---------------#
# Recoder la variable "descr_cat_veh" en int
data$descr_cat_veh <- as.integer(factor(data$descr_cat_veh))

# Recoder la variable "descr_grav" en int
data$descr_grav <- as.integer(factor(data$descr_grav))


# Convertir la colonne de dates en objet de type Date
data$date <- as.Date(data$date)


#---------------AGGREGATION---------------#
# Par mois
accidents <- aggregate(rep(1, nrow(data)) ~ format(data$date, "%Y-%m"), data, sum)
#colnames(accidents) <- c("Année-Mois", "Nombre d'accidents")
#barplot(accidents$`Nombre d'accidents`, names.arg = accidents$`Année-Mois`, xlab = "Mois", ylab = "Nombre d'accidents", main = paste("Accidents par mois pour", nb, "lignes"))
#paste sert a concat les strings avec la variable nb

# Par semaine
accidents2 <- aggregate(rep(1, nrow(data)) ~ format(data$date, "%Y-%U"), data, sum)
#colnames(accidents2) <- c("Année-Semaine", "Nombre d'accidents")
#barplot(accidents2$`Nombre d'accidents`, names.arg = accidents2$`Année-Semaine`, xlab = "Semaines", ylab = "Nombre d'accidents", main = paste("Accidents par semaine pour", nb, "lignes"))


#---------------CONSTRUCTION JEU DE DONNEES---------------#
# On compare les données de infos_cities.csv avec les données de stat_acc_V3.csv, on prend les villes qui sont dans les deux fichiers et on affiche leur région
# On crée un jeu de données avec les villes et leur région

#1 - On raoute la colone de région au CSV (on merge les infos)
# Lecture du premier fichier CSV
data_cities <- read.csv("infos_cities.csv", sep = ",", header = TRUE)
# Lecture du deuxième fichier CSV
data_accidents <- read.csv("stat_acc_V3.csv", sep = ";", header = TRUE)

# merge les deux datas selon le code_insee
data_merged <- merge(data_accidents, data_cities,  by = "id_code_insee")

newdata <- data_merged[c("id_code_insee","Num_Acc","num_veh","id_usa","date","ville","latitude.x","longitude.x","descr_cat_veh","descr_agglo","descr_athmo","descr_lum","descr_etat_surf","description_intersection","an_nais","age","place","descr_dispo_secu","descr_grav","descr_motif_traj","descr_type_col","department_name", "region_name")]

# Sauvegarde du premier fichier CSV avec la colonne "region_name" ajoutée
write.csv(newdata, "new_cities.csv", row.names = FALSE)



#2 - On créer le jeu de données
library(dplyr)
data_nvjeu <- newdata %>% group_by(descr_grav, region_name) %>% summarise(number = n()) %>% ungroup()








print("--------------------Tests--------------------")
#print(data$descr_grav)
#print(data$descr_cat_veh)
