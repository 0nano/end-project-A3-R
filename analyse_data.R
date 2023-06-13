# Charger les données à partir du fichier CSV
data <- read.csv("new_cities.csv", sep = ",", header = TRUE)

table_1 <- table(data$descr_lum, data$descr_grav)
print(table_1)

#mosaicplot(table_1, main = "Tableau entre la luminosité et la gravité des accidents")

chi2_test <- chisq.test(table_1)
print(chi2_test$p.value)



table_2 <- table(data$descr_athmo, data$descr_grav)
print(table_2)

#mosaicplot(table_2, main = "Tableau entre l'athmosphère et la gravité des accidents")

chi2_test2 <- chisq.test(table_2)
print(chi2_test2)

table_3 <- table(data$descr_dispo_secu, data$descr_grav)
print(table_3)

#mosaicplot(table_3, main = "Tableau entre la disponibilité de la sécurité et la gravité des accidents")

chi2_test3 <- chisq.test(table_3)
print(chi2_test3)

table_4 <- table(data$descr_etat_surf, data$descr_grav)
print(table_4)

mosaicplot(table_4, main = "Tableau entre l'état du sol et la gravité des accidents")

chi2_test4 <- chisq.test(table_4)
print(chi2_test4$p.value)