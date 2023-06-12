data <- read.csv("stat_acc_V3.csv", sep=";")
data <- read.csv("~/GitHub/end-project-A3-R/stat_acc_V3.csv", sep=";")
# Get value counts for the 'pays' column

data_sum <- table(data$descr_athmo)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction des conditions atmosphériques", xlab = "Condition atmosphérique", ylab = "Nbr d'accidents",col="#69b3a2",ylim=c(0,60000))



data_sum <- table(data$descr_etat_surf)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction de la description de la surface", xlab = "Condition atmosphérique", ylab = "Nbr d'accidents",col="#69b3a2")

data_sum <- table(data$descr_grav)
# Print the results
print(nom)
# Plot the value counts in ascending order
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction de la description de la surface", xlab = "Condition atmosphérique", ylab = "Nbr d'accidents",col="#69b3a2")


data_sum <- table(data$ville)

sorted_counts <- sort(value_counts, decreasing = TRUE)
# Print the results
print(sorted_counts)
# Plot the value counts in ascending order
value_counts = head(sorted_counts,30)
barplot(sort(data_sum, decreasing = TRUE), main = "Nombre d’accidents en fonction de la description de la surface" , ylab = "Nbr d'accidents",col="#69b3a2",las=2)
