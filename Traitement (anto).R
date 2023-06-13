#preparation des donnees

#data <- read.csv("stat_acc_V3.csv", sep=";")
data <- read.csv("~/GitHub/end-project-A3-R/stat_acc_V3.csv", sep=";")

#Changement du nom des colonnes
colnames(data)[1] = "num_accident" #Num_Acc
colnames(data)[2] = "num_vehicule" #Num_Veh
colnames(data)[14] = "descr_intersection" #description_intersection




#Changement du type de variables et changement des valeurs de NULL/NA en valeurs moyenne
data$an_nais <- as.integer(data$an_nais)
data$age <- as.numeric(data$age)
data$place <- as.integer(data$place)

#Recuperation de la valeur mediane sur chaque colonne 
mean_an_nais <- mean(data$an_nais, na.rm = TRUE)
mean_age <- mean(data$age, na.rm = TRUE)
mean_place <- mean(data$place, na.rm = TRUE)

#affectation des valeurs de NA par la valeur mediane
data$an_nais[is.na(data$an_nais)] <- mean_an_nais
data$age[is.na(data$age)] <- mean_age
data$place[is.na(data$place)] <- mean_place

#Modification de l'age pour obtenir sa valeur au moment de l'accident
data$age = 2009-data$an_nais 

#conversion du format chr en format date
data$date <- as.POSIXct(data$date,'%Y-%m-%d %H:%M:%S')


#Coordonnes geographique de PARIS  pour tous les arrondissements
data$latitude[grepl("^PARIS\\s\\d+$", data$ville)] <- 48.8534
data$longitude[grepl("^PARIS\\s\\d+$", data$ville)] <- 2.3488

#Coordonnes geographique de MARSEILLE  pour tous les arrondissements
data$latitude[grepl("^MARSEILLE\\s\\d+$", data$ville)] <- 43.3
data$longitude[grepl("^MARSEILLE\\s\\d+$", data$ville)] <- 5.4

#Coordonnes geographique de LYON  pour tous les arrondissements
data$latitude[grepl("^LYON\\s\\d+$", data$ville)] <- 45.75 
data$longitude[grepl("^LYON\\s\\d+$", data$ville)] <- 4.85
