data <- read.csv("stat_acc_V3.csv", sep=";")
data <- read.csv("~/GitHub/end-project-A3-R/stat_acc_V3.csv", sep=";")
# Get value counts for the 'pays' column

#Nombre d’accidents en fonction des conditions atmosphériques
data_sum <- table(data$descr_athmo)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction des conditions atmosphériques", xlab = "Condition atmosphérique", ylab = "Nbr d'accidents",col="#69b3a2",ylim=c(0,60000))


#Nombre d’accidents en fonction de la description de la surface
data_sum <- table(data$descr_etat_surf)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction de la description de la surface", xlab = "Condition atmosphérique", ylab = "Nbr d'accidents",col="#69b3a2")

#Nombre d’accidents selon la gravité
data_sum <- table(data$descr_grav)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents selon la gravité", xlab = "Gravité", ylab = "Nbr d'accidents",col="#69b3a2")

xnombre_accidents <- table(data$date)
heures <- format(nombre_accidents, "%H:%M:%S")
tranches <- cut(data$date, breaks = "hour")
barplot(tranches, main = "Nombre d'accidents par tranche d'heure", xlab = "Tranche d'heure", ylab = "Nombre d'accidents")


plot(x=temps, y=data$date, type="h", ylim=c(-250,50), ylab="")

###Zone de test
data$date <- strptime(data$date, '%Y-%m-%d %H:%M:%S', tz = '')
data$date <- format(data$date, '%Y-%m-%d %H:%M:%S')

###


#Nombre d’accidents par ville
data_sum <- table(data$ville)
data_ordered <- sort(value_counts, decreasing = TRUE)
first_cities = head(data_ordered,30)
#Le premier barplot affiche les 30 premières ville de France avec le plus grand nombre d'accidents, le second, toutes les villes, la lisibilité du second est faible.
barplot(sort(first_cities, decreasing = TRUE), main = "Nombre d’accidents par ville" , ylab = "Nbr d'accidents",col="#69b3a2",las=2)
#barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents par ville" , ylab = "Nbr d'accidents",col="#69b3a2",las=2)

