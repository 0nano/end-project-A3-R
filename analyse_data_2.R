#### PARTIE ANALYSE

###REGRESSION PAR MOIS
data$month <- format(data$date, "%m") #Extraction du mois parmis la data de l'accident
monthly_counts <- table(data$month) #Creation du nombre d'accident par mois
monthly_data <- data.frame(month = names(monthly_counts), nombre_accidents = as.numeric(monthly_counts), nombre_accidents_cumul = as.numeric(cumsum(monthly_counts)))
#Creation de monthly_data: un tableau avec pour chaque mois le nombre d'accident ainsi que le cumul des accidents
monthly_data$month <- as.integer(monthly_data$month) #met en tant que int les valeurs de la table qui sont en chr
names(monthly_data)#Affiche les noms utilises dans la table
cor(monthly_data$month,monthly_data$nombre_accidents_cumul) #Correlation entre le mois et le nombre d'accident cumulé
model_bymonth <- lm(monthly_data$nombre_accidents_cumul ~monthly_data$month)#Creation du modele
summary(model_bymonth) #information sur la regression
png(filename = "ressources/regression_acc_month.png",width=1050, height=550,res=100)
plot(monthly_data$month, monthly_data$nombre_accidents_cumul,main ="Regression de l’évolution du nombre d’accidents par mois (cumulée)")
abline(model_bymonth, col = 2, lwd = 3) #Coeficient de linéarité
dev.off()
plot(model_bymonth)
confint(model_bymonth, level = 0.95)



###REGRESSION PAR SEMAINE
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
png(filename = "ressources/regression_acc_week.png",width=1050, height=550,res=100)
plot(weekly_data$week, weekly_data$nombre_accidents_cumul,main ="Regression de l’évolution du nombre d’accidents par semaine (cumulée)")
abline(model_byweek, col = 2, lwd = 3) #Coeficient de linéarité
dev.off()
plot(model_byweek)
confint(model_byweek, level = 0.95)
