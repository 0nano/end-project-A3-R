# Charger les données à partir du fichier CSV
data <- read.csv("new_cities.csv", sep = ",", header = TRUE)

table_1 <- table(data$descr_grav, data$descr_type_col)
print(table_1)

chi2_test1 <- chisq.test(table_1)
print(chi2_test1$p.value)

#png("ressources/mosaiqueplot_descr_grav&age.png")

mosaicplot(table_1, main = "descr_grav&age")

#dev.off()