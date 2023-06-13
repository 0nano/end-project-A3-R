data <- read.csv("stat_acc_V3.csv", sep=";")
#data <- read.csv("~/GitHub/end-project-A3-R/stat_acc_V3.csv", sep=";")


#Nombre d’accidents en fonction des conditions atmosphériques
data_sum <- table(data$descr_athmo)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction des conditions atmosphériques", xlab = "Condition atmosphérique", ylab = "Nbr d'accidents",col="#69b3a2",ylim=c(0,60000))


#Nombre d’accidents en fonction de la description de la surface
data_sum <- table(data$descr_etat_surf)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction de la description de la surface", xlab = "Condition atmosphérique", ylab = "Nbr d'accidents",col="#69b3a2")

#Nombre d’accidents selon la gravité
data_sum <- table(data$descr_grav)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents selon la gravité", xlab = "Gravité", ylab = "Nbr d'accidents",col="#69b3a2")


#Nombre d’accidents par tranches d’heure
data$date <- as.POSIXct(data$date,'%Y-%m-%d %H:%M:%S')
hours <- format(data$date, '%H') #Recuperation de l'heure de l'accident 
breaks <- seq(0, 24, by = 2) # Definition des intervalles de temps, ici par 2h
time_intervals <- cut(as.numeric(hours), breaks = breaks) #Groupemement des heures avec les intervales correspondantes
freq_table <- table(time_intervals)
barplot(freq_table, main = "Nombre d’accidents par tranches d’heure", xlab = "Intervalle de temps", ylab = "Nombre d'accidents",col="#69b3a2")


#Nombre d’accidents par ville
data_sum <- table(data$ville)
data_ordered <- sort(value_counts, decreasing = TRUE) #Tri par villes avec le plus d'accidents
first_cities = head(data_ordered,30) #Recuperation des 30 premieres villes
###Le premier barplot affiche les 30 premières ville de France avec le plus grand nombre d'accidents, le second, toutes les villes, la lisibilité du second est faible.
barplot(sort(first_cities, decreasing = TRUE), main = "Nombre d’accidents par ville" , ylab = "Nbr d'accidents",col="#69b3a2",las=2)
#barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents par ville" , ylab = "Nbr d'accidents",col="#69b3a2",las=2)


#Nombre d’accidents par tranches d'age
#Creation des groupes d'age
breaks <- seq(-1, 109, by = 10)
age_intervals <- cut(data$age, breaks = breaks)
#Affectation des categories avec le nombre d'accidents
age_sum <- table(age_intervals)
text(x = barplot(age_sum, main = "Nombre d'accidents par tranches d'âge", 
                 xlab = "Intervalle d'âge", ylab = "Nombre d'accidents", 
                 col = "#69b3a2"), y = age_sum, labels = age_sum, pos = 3,col = "#69b3a2",ylim = 25000)



#Nombre d’accidents par mois
#data$date <- as.POSIXct(data$date, format = '%Y-%m-%d %H:%M:%S')
months <- format(data$date, '%m') #Recuperation du mois des accidents
freq_table <- table(months)
barplot(freq_table, main = "Nombre d'accidents par mois", xlab = "Mois", ylab = "Nombre d'accidents", col = "#69b3a2", names.arg = month.name)



#### PARTIE ANALYSE (BESOIN DE LA PREPARATION)
# Creatino d'une variable avec le mois et l'annee seulement
data$year_month <- format(data$date, "%Y-%m")

# Create a table of unique year-month combinations and corresponding counts
monthly_counts <- table(data$year_month)

# Create the monthly_data dataframe with two columns: year_month and nombre_accidents
monthly_data <- data.frame(year_month = names(monthly_counts), nombre_accidents = as.numeric(monthly_counts))

#Regression lineaire
regression <- lm(nombre_accidents ~ as.numeric(factor(year_month)), monthly_data)

#Resultats de la regression
summary(regression)


