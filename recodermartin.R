#---------------LECTURE---------------#
nb = 74000 # Nombre de lignes à lire
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
# On crée un jeu de données avec la meme méthode que précedemment 










print("--------------------Tests--------------------")
#print(data$descr_grav)
#print(data$descr_cat_veh)
