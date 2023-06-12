# On lit le fichier csv et on crée les données | nrows --> Nb de lignes lues
data <- read.csv("stat_acc_V3.csv", sep=";", nrows=1000)

# Recoder la variable "descr_cat_veh" en int
data$descr_cat_veh <- as.integer(factor(data$descr_cat_veh))

# Recoder la variable "descr_grav" en int
data$descr_grav <- as.integer(factor(data$descr_grav))

# Afficher les nouvelles données 
print("--------------------Tests--------------------")
print(data$descr_grav)
print(data$descr_cat_veh)
