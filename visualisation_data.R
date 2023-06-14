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
pie(pie_values, labels = paste(pie_labels, percentages, "%", sep = " "), col = colors, main = "Distribution des accidents en fonction des conditions atmosphériques")


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
pie(pie_values, labels = paste(pie_labels, percentages, "%", sep = " "), col = colors, main = "Distribution des accidents en fonction de la surface")




#Nombre d’accidents selon la gravité
data_sum <- table(data$descr_grav)
percentages <- round((data_sum * 100) / sum(data_sum), 2)
colors <- c("#228B22", "#fca103", "#FF5733", "#8B0000")
labels <- paste(names(data_sum)," - " ,percentages, "%")
pie(data_sum, col = colors, main = "Distribution  des accidents selon leur gravité", labels = labels)


###ZONE DE TEST 
library(ggplot2)
library(dplyr)
library(tidyverse)
data_sum <- table(data$descr_grav)
percentages <- round((data_sum * 100) / sum(data_sum), 2)
colors <- c("#228B22", "#fca103", "#FF5733", "#8B0000")
labels <- paste(names(data_sum), " - ", percentages, "%")

# Create a data frame for the pie chart
pie_data <- data.frame(categories = names(data_sum), counts = data_sum, labels = labels)

# Create the pie chart using ggplot
pie_chart <- ggplot(pie_data, aes(x = "", y = counts, fill = categories)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = colors) +
  labs(title = "Distribution des accidents selon leur gravité") +
  theme_void() +
  theme(legend.position = "bottom") +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5))

# Save the pie chart as a PNG file
ggsave("pie_chart.png", plot = pie_chart, width = 7, height = 7)



###


#Nombre d’accidents par tranches d’heure
data$date <- as.POSIXct(data$date,'%Y-%m-%d %H:%M:%S')
hours <- format(data$date, '%H') #Recuperation de l'heure de l'accident 
breaks <- seq(0, 24, by = 2) # Definition des intervalles de temps, ici par 2h
time_intervals <- cut(as.numeric(hours), breaks = breaks) #Groupemement des heures avec les intervales correspondantes
freq_table <- table(time_intervals)
barplot(freq_table, main = "Nombre d’accidents par tranches d’heure", xlab = "Intervalle de temps", ylab = "Nombre d'accidents",col="#69b3a2")


#Nombre d’accidents par ville
data_sum <- table(data$ville)
#data_ordered <- sort(value_counts, decreasing = TRUE) #Tri par villes avec le plus d'accidents
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
# Creation d'une variable avec le mois seulement
data$month <- format(data$date, "%m")

# Create a table of unique year-month combinations and corresponding counts
monthly_counts <- table(data$month)

# Create the monthly_data dataframe with two columns: year_month and nombre_accidents
monthly_data <- data.frame(month = names(monthly_counts), nombre_accidents = as.numeric(monthly_counts), nombre_accidents_cumul = as.numeric(cumsum(monthly_counts)))

monthly_data$month <- as.integer(monthly_data$month)
names(monthly_data)
cor(monthly_data$month,monthly_data$nombre_accidents_cumul)
#Creation du modele
model_bymonth <- lm(monthly_data$nombre_accidents_cumul ~monthly_data$month)
#Regression lineaire
summary(model_bymonth)
plot(monthly_data$month, monthly_data$nombre_accidents_cumul)
abline(model_bymonth, col = 2, lwd = 3)
plot(model_bymonth)


###PARTIE DE LA REGRESSION PAR SEMAINE
# Convert the date column to POSIXct format (if not already done)
# Extract the week number from the date
data$week <- format(data$date, "%U")
weekly_counts <- table(data$week)

weekly_data <- data.frame(week = names(weekly_counts), nombre_accidents = as.numeric(weekly_counts) ,nombre_accidents_cumul = as.numeric(cumsum(weekly_counts)))
weekly_data$week <- as.integer(weekly_data$week)
names(weekly_data)

cor(weekly_data$week,weekly_data$nombre_accidents_cumul)

model_byweek <- lm(weekly_data$nombre_accidents_cumul ~ weekly_data$week)


summary(model_byweek)
anova(model_byweek)
plot(weekly_data$week, weekly_data$nombre_accidents_cumul, main="Nb d'accidents cumulés par semaine")
abline(model_byweek, col = 2, lwd = 3)
plot(model_byweek)
