data <- read.csv("stat_acc_V3.csv", sep=";")
#data <- read.csv("~/GitHub/end-project-A3-R/stat_acc_V3.csv", sep=";")


#Nombre d’accidents en fonction des conditions atmosphériques
data_sum <- table(data$descr_athmo)
#barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction des conditions atmosphériques", xlab = "Condition atmosphérique", ylab = "Nbr d'accidents",col="#69b3a2",ylim=c(0,60000))
sorted_data <- sort(data_sum, decreasing = TRUE) #Tri des valeurs par ordre decroissant
top_values <- sorted_data[1:6] #Recuperation des 6 valeurs les plus importantes
top_labels <- names(top_values) #Creation du label avec le nom des valeurs les plus importantes
other_value <- sum(sorted_data[7:length(sorted_data)])# calcul de la valeur "autre" contenant la proportion du reste des valeurs
pie_values <- c(top_values, other_value) # creation du vecteur des value a utiliser pour le pie chart
pie_labels <- c(top_labels, "Autre") # asignation des labels des 6 premieres valeurs + "autre"
percentages <- round((pie_values / sum(pie_values)) * 100, 1)#Calcul du pourcentage de chaque part
colors <- rainbow(length(pie_values)) #Palette de couleur
png(filename = "pie_chart_ConditionsAtmo.png",width=750, height=600,res=100)
pie(pie_values, labels = paste(pie_labels, percentages, "%", sep = " "), col = colors, main = "Distribution des accidents en fonction des conditions atmosphériques")
dev.off()


# Nombre d’accidents en fonction de la description de la surface
data_sum <- table(data$descr_etat_surf)
sorted_data <- sort(data_sum, decreasing = TRUE) #Tri des valeurs par ordre decroissant
top_values <- sorted_data[1:3] #Recuperation des 3 valeurs les plus importantes
top_labels <- names(top_values) #Creation du label avec le nom des valeurs les plus importantes
other_value <- sum(sorted_data[4:length(sorted_data)])# calcul de la valeur "autre" contenant la proportion du reste des valeurs
pie_values <- c(top_values, other_value) # creation du vecteur des value a utiliser pour le pie chart
pie_labels <- c(top_labels, "Autre") # asignation des labels des 3 premieres valeurs + "autre"
percentages <- round((pie_values / sum(pie_values)) * 100, 1)#Calcul du pourcentage de chaque part
colors <- rainbow(length(pie_values)) #Palette de couleur
png(filename = "pie_chart_desc_surf.png",width=750, height=600,res=100)
pie(pie_values, labels = paste(pie_labels, percentages, "%", sep = " "), col = colors, main = "Distribution des accidents en fonction de la surface")
dev.off()




#Nombre d’accidents selon la gravité
data_sum <- table(data$descr_grav)
percentages <- round((data_sum * 100) / sum(data_sum), 2)
colors <- c("#228B22", "#fca103", "#FF5733", "#8B0000")
labels <- paste(names(data_sum)," - " ,percentages, "%")
png(filename = "pie_chart_gravite.png",width=750, height=600,res=100)
pie(data_sum, col = colors, main = "Distribution  des accidents selon leur gravité", labels = labels)
dev.off()



#Nombre d’accidents par tranches d’heure
data$date <- as.POSIXct(data$date,'%Y-%m-%d %H:%M:%S')
hours <- format(data$date, '%H') #Recuperation de l'heure de l'accident 
breaks <- seq(0, 24, by = 2) # Definition des intervalles de temps, ici par 2h
time_intervals <- cut(as.numeric(hours), breaks = breaks) #Groupemement des heures avec les intervales correspondantes
freq_table <- table(time_intervals)
png(filename = "histrogram_acc_per_hours.png",width=750, height=600,res=100)
barplot(freq_table, main = "Nombre d’accidents par tranches d’heure", xlab = "Intervalle de temps", ylab = "Nombre d'accidents",col="#69b3a2")
dev.off()


#Nombre d’accidents par ville
data_sum <- table(data$ville)
#data_ordered <- sort(value_counts, decreasing = TRUE) #Tri par villes avec le plus d'accidents
first_cities = head(data_ordered,30) #Recuperation des 30 premieres villes
###Le premier barplot affiche les 30 premières ville de France avec le plus grand nombre d'accidents, le second, toutes les villes, la lisibilité du second est faible.
png(filename = "histrogram_acc_per_city.png",width=650, height=550,res=100)
par(mar = c(10.5, 4, 1, 0)) # permet d'ajuster la taille du plot
barplot(sort(first_cities, decreasing = TRUE), main = "Nombre d’accidents par ville (30 premières)" , ylab = "Nbr d'accidents",col="#69b3a2",las=2)
#barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents par ville" , ylab = "Nbr d'accidents",col="#69b3a2",las=2)
dev.off()


#Nombre d’accidents par tranches d'age
breaks <- seq(-1, 109, by = 10) #Creation des groupes d'age
age_intervals <- cut(data$age, breaks = breaks) #Affectation des categories avec le nombre d'accidents
age_sum <- table(age_intervals)
png(filename = "histrogram_acc_per_agegroup.png",width=1050, height=850,res=100)
par(mar = c(3, 4, 5, 0))
text(x = barplot(age_sum, main = "Nombre d'accidents par tranches d'âge", 
                 xlab = "Intervalle d'âge", ylab = "Nombre d'accidents",
                 col = "#69b3a2"), y = age_sum,labels = age_sum, pos = 3,col = "#69b3a2")
dev.off()





#Nombre d’accidents par mois
#data$date <- as.POSIXct(data$date, format = '%Y-%m-%d %H:%M:%S')
months <- format(data$date, '%m') #Recuperation du mois des accidents
freq_table <- table(months)
png(filename = "histrogram_acc_per_month.png",width=1250, height=550,res=100)
barplot(freq_table, main = "Nombre d'accidents par mois", xlab = "Mois", ylab = "Nombre d'accidents", col = "#69b3a2", names.arg = month.name)
dev.off()



#### PARTIE ANALYSE (BESOIN DE LA PREPARATION)
data$month <- format(data$date, "%m") #Extraction du mois parmis la data de l'accident
monthly_counts <- table(data$month) #Creation du nombre d'accident par mois
monthly_data <- data.frame(month = names(monthly_counts), nombre_accidents = as.numeric(monthly_counts), nombre_accidents_cumul = as.numeric(cumsum(monthly_counts)))
#Creation de monthly_data: un tableau avec pour chaque mois le nombre d'accident ainsi que le cumul des accidents
monthly_data$month <- as.integer(monthly_data$month) #met en tant que int les valeurs de la table qui sont en chr
names(monthly_data)#Affiche les noms utilises dans la table
cor(monthly_data$month,monthly_data$nombre_accidents_cumul) #Correlation entre le mois et le nombre d'accident cumulé
model_bymonth <- lm(monthly_data$nombre_accidents_cumul ~monthly_data$month)#Creation du modele
summary(model_bymonth) #information sur la regression
png(filename = "regression_acc_month.png",width=1050, height=550,res=100)
plot(monthly_data$month, monthly_data$nombre_accidents_cumul,main ="Regression de l’évolution du nombre d’accidents par mois (cumulée)")
abline(model_bymonth, col = 2, lwd = 3) #Coeficient de linéarité
dev.off()
plot(model_bymonth)


###PARTIE DE LA REGRESSION PAR SEMAINE

data$week <- format(data$date, "%U") #Extraction de la semaine parmis la data de l'accident
weekly_counts <- table(data$week) #Creation du nombre d'accident par semaine
weekly_data <- data.frame(week = names(weekly_counts), nombre_accidents = as.numeric(weekly_counts) ,nombre_accidents_cumul = as.numeric(cumsum(weekly_counts)))
#Creation de weekly_data: un tableau avec pour chaque semaine le nombre d'accident ainsi que le cumul des accidents
weekly_data$week <- as.integer(weekly_data$week) #met en tant que int les valeurs de la table
names(weekly_data)#Affiche les noms utilises dans la table
cor(weekly_data$week,weekly_data$nombre_accidents_cumul) #Correlation entre la semaine et le nombre d'accident cumulé
model_byweek <- lm(weekly_data$nombre_accidents_cumul ~ weekly_data$week) #Création du modèle de regression entre le nombre d'accident cumulé et l
summary(model_byweek)
anova(model_byweek)
png(filename = "regression_acc_week.png",width=1050, height=550,res=100)
plot(weekly_data$week, weekly_data$nombre_accidents_cumul,main ="Regression de l’évolution du nombre d’accidents par semaine (cumulée)")
abline(model_byweek, col = 2, lwd = 3) #Coeficient de linéarité
dev.off()
plot(model_byweek)
