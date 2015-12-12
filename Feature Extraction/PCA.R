options(java.parameters = "-Xmx4g")

library(caret)
library(RWeka)
library(e1071)
library(class)

data <- read.csv("/home/sanilborkar/Documents/Data Mining/Intrusion-Detection-KDD/Dataset/kdd_train_dataset.csv", header = TRUE, sep = ",")

# All the data packets that are not normal are attack packets
data$attack_type = as.character(data$attack_type)
data$attack_type[!(data$attack_type == "normal.")] = "attack."
data$attack_type = factor(data$attack_type)

# Perform PCA
#pca <- princomp(data[,c(1,5,6, 8:11, 13:20, 23:41)], cor = FALSE)
#summary(pca)
#pca$loadings

set.seed(2968)
Train_index <- createDataPartition(data$attack_type, p = 0.80, list = FALSE, times = 1)

# Training set
Train_Data <- data[Train_index, ]

# Test set
Test_Data <- data[-Train_index, ]
#Test_Data <- read.csv("/home/sanilborkar/Documents/Data Mining/Intrusion-Detection-KDD/Dataset/kdd_full_dataset.csv", header = TRUE, sep = ",")
#index <- createDataPartition(Test_Data$attack_type, p = 0.05, list = FALSE, times = 1)
#Test_Data <- Test_Data[index, ]

# Classify using KNN
#c1 <- nrow(Train_Data[Train_Data[,42]=="normal.", ])
#c2 <- nrow(Train_Data[Train_Data[,42]=="attack.", ])
#cl <- factor(c(rep("normal.", c1), rep("attack.", c2)))
#Classifier <- knn(Train_Data[,c(20,27,30,31,40,41)], Test_Data[,c(20,27,30,31,40,41)], cl, k = 3, prob = FALSE, use.all = TRUE)

# Train the classifier with C4.5
Classifier <- J48(Train_Data$attack_type ~ ., data = Train_Data[,c(19,25,26,28,29,30,35,37,38,39,40,42)])   #<--- Comp 23 (Acc = 95.5%)
#Classifier <- naiveBayes(Train_Data[,c(19,25,26,28,29,30,37,38,39,40,42)], Train_Data$attack_type)      # Acc = 41%
#summary(Classifier)

# Predict
Prediction <- predict(Classifier, Test_Data[,1:41])
#summary(Prediction)
confusionMatrix(Prediction, Test_Data[,42])