#preparation des donnees

data <- read.csv("stat_acc_V3.csv", sep=";")

#Changement du nom des colonnes
colnames(data)[1] = "num_accident" #Num_Acc
colnames(data)[2] = "num_vehicule" #Num_Veh
colnames(data)[14] = "descr_intersection" #description_intersection




#Changement du type de variables et changement des valeurs de NULL/NA en valeurs moyenne
data$an_nais <- as.integer(data$an_nais)
data$age <- as.integer(data$age)
data$place <- as.integer(data$place)

data$an_nais[is.na(data$an_nais)] <- mean(data$an_nais)
data$age[is.na(data$age)] <- mean(data$age)
data$place[is.na(data$place)] <- mean(data$place)

#conversion du format chr en format date
data$date <- as.POSIXct(data$date,'%Y-%m-%d %H:%M:%S')
