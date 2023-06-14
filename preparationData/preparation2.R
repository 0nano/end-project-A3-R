#---------------LECTURE---------------#
data <- read.csv("stat_acc_V3_modif.csv", sep = ",")

#---------------AGGREGATION---------------#
#On repasse en format date sans les heures pour pouvoir faire des regroupements par mois et par semaine
data$date <- as.Date(data$date)

# Par mois
png("ressources/graphique_mois.png")

accidents <- aggregate(rep(1, nrow(data)) ~ format(data$date, "%Y-%m"), data, sum)
colnames(accidents) <- c("Année-Mois", "Nombre d'accidents")
barplot(accidents$`Nombre d'accidents`, names.arg = accidents$`Année-Mois`, xlab = "Mois", ylab = "Nombre d'accidents", main = "Accidents par mois")

dev.off()

# Par semaine
png("ressources/graphique_sem.png")

accidents2 <- aggregate(rep(1, nrow(data)) ~ format(data$date, "%Y-%U"), data, sum)
colnames(accidents2) <- c("Année-Semaine", "Nombre d'accidents")
barplot(accidents2$`Nombre d'accidents`, names.arg = accidents2$`Année-Semaine`, xlab = "Semaines", ylab = "Nombre d'accidents", main = "Accidents par semaines")

dev.off()

#---------------CONSTRUCTION JEU DE DONNEES---------------#
# On compare les données de infos_cities.csv avec les données de stat_acc_V3.csv, on prend les villes qui sont dans les deux fichiers et on affiche leur région
# On crée un jeu de données avec les villes et leur région

#1 - On raoute la colone de région au CSV (on merge les infos)
# Lecture du premier fichier CSV
data_cities <- read.csv("infos_cities.csv", sep = ",", header = TRUE)
data_cities <- data_cities %>% group_by(id_code_insee) %>% filter (! duplicated(id_code_insee)) # retirer les doublons

# Lecture du deuxième fichier CSV

# merge les deux datas selon le code_insee
data_merged <- merge(data, data_cities,  by.x = "id_code_insee", by.y = "id_code_insee")

newdata <- data_merged[c("id_code_insee","Num_Acc","num_veh","id_usa","date","ville","latitude.x","longitude.x","descr_cat_veh","descr_agglo","descr_athmo","descr_lum","descr_etat_surf","description_intersection","an_nais","age","place","descr_dispo_secu","descr_grav","descr_motif_traj","descr_type_col","department_name", "department_number", "region_name")]

# Sauvegarde du premier fichier CSV avec la colonne "region_name" ajoutée
write.csv(newdata, "new_cities.csv", row.names = FALSE)



#2 - On créer le jeu de données
data_nvjeu <- newdata %>% group_by(descr_grav, region_name) %>% summarise(number = n()) %>% ungroup()