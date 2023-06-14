#---------------PACKAGES---------------#
library(dplyr)

#---------------LECTURE---------------#
# On lit le fichier csv et on crée les données
data <- read.csv("stat_acc_V3.csv", sep = ";")

#---------------RECODER---------------#
data$descr_grav <- recode(data$descr_grav,
    "Indemne" = 0,
    "Tué" = 3,
    "Blessé hospitalisé" = 2,
    "Blessé léger" = 1
)


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

write.csv(data, "stat_acc_V3_modif.csv", row.names = FALSE)

data$descr_etat_surf <- recode(data$descr_etat_surf,
    "Verglacée" = 1,
    "Enneigée" = 2,
    "Mouillée" = 3,
    "Normale" = 4,
    "Corps gras – huile" = 5,
    "Boue" = 6,
    "Flaques" = 7,
    "Inondée" = 8,
    "Autre" = 9,
)



data$descr_lum <- recode(data$descr_lum,
    "Crépuscule ou aube" = 1,
    "Plein jour" = 2,
    "Nuit sans éclairage public" = 3,
    "Nuit avec éclairage public allumé" = 4,
    "Nuit avec éclairage public non allumé" = 5,
)

data$descr_agglo <- recode(data$descr_agglo,
    "Hors agglomération" = 1,
    "En agglomération" = 2,
)


data$descr_athmo <- recode(data$descr_athmo,
    "Brouillard – fumée" = 1,
    "Neige – grêle" = 2,
    "Pluie forte" = 3,
    "Normale" = 4,
    "Temps éblouissant" = 5,
    "Pluie légère" = 6,
    "Temps couvert" = 7,
    "Vent fort – tempête" = 8,
    "Autre" = 9,
)


data$description_intersection <- recode(data$description_intersection,
    "Hors intersection" = 1,
    "Intersection en X" = 2,
    "Giratoire" = 3,
    "Intersection en T" = 4,
    "Intersection à plus de 4 branches" = 5,
    "Autre intersection" = 6,
    "Intersection en Y" = 7,
    "Passage à niveau" = 8,
    "Place" = 9,
)

data$descr_cat_veh <- recode(data$descr_cat_veh,
    "PL seul > 7,5T" = 1,
    "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque " = 2,
    "VL seul" = 3,
    "Autocar" = 4,
    "PL > 3,5T + remorque" = 5,
    "Cyclomoteur <50cm3" = 6,
    "Motocyclette > 125 cm3" = 7,
    "Tracteur routier + semi-remorque" = 8,
    "Tracteur agricole" = 9,
    "PL seul 3,5T <PTCA <= 7,5T" = 10,
    "Autobus" = 11,
    "Scooter > 50 cm3 et <= 125 cm3" = 12,
    "Train" = 13,
    "Scooter > 125 cm3" = 14,
    "Scooter < 50 cm3" = 15,
    "Voiturette (Quadricycle à moteur carrossé) (anciennement \"voiturette ou tricycle à moteur\")" = 16,
    "Bicyclette" = 17,
    "Motocyclette > 50 cm3 et <= 125 cm3" = 18,
    "VU seul 1,5T <= PTAC <= 3,5T sans remorque" = 19,
    "VU > 3,5T + remorque" = 20,
    "Engin spécial" = 21,
    "Quad lourd > 50 cm3 (Quadricycle à moteur non carrossé)" = 22,
    "Tramway" = 23,
    "Tracteur routier seul" = 24,
    "Quad léger <= 50 cm3 (Quadricycle à moteur non carrossé)" = 25,
    "Autre véhicule" = 26,
)



data$descr_motif_traj <- recode(data$descr_motif_traj,
    "Non renseigné" = 0,
    "Domicile – travail" = 1,
    "Domicile – école" = 2,
    "Courses – achats" = 3,
    "Utilisation professionnelle" = 4,
    "Promenade – loisirs" = 5,
    "Autre" = 9
)

write.csv(data, "stat_only_ints.csv", row.names = FALSE)