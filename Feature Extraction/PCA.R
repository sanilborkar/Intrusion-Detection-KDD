options(java.parameters = "-Xmx4g")

library(caret)
library(RWeka)
library(e1071)
library(class)

data <- read.csv("/home/sanilborkar/Documents/Data Mining/Intrusion-Detection-KDD/Feature Extraction/kdd_train_dataset.csv", header = TRUE, sep = ",")

# All the data packets that are not normal are attack packets
data$attack_type = as.character(data$attack_type)
data$attack_type[!(data$attack_type == "normal.")] = "attack."
data$attack_type = factor(data$attack_type)

# Write CSV in R
write.csv(data, file = "kdd_train.csv", row.names = FALSE)

# Perform PCA
pca <- princomp(data[,c(1,5,6, 8:11, 13:20, 23:41)], cor = FALSE)
summary(pca)
pca$loadings