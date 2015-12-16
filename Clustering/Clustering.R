options(java.parameters = "-Xmx4g")
install.packages("clusterSim")
library(rgl)
library(caret)
library(optpart)
library(labdsv)
library(stats)
library(igraph)
library(MCL)
library(graphics)
library(RWeka)
library(stats)
library(cluster)
library(fpc)
library(clusterSim)
set.seed(1971)
densityBased <- dbscan(datapart, 1.3, MinPts = 5)
graphBased <- hclust(dist(datapart), method = "complete")

data <- read.csv(file="kddcup.data_10_percent_corrected",header=TRUE, sep=",")


# attack_type preprocess
data$attack_type = as.character(data$attack_type)
data$attack_type[data$attack_type == "ipsweep."] = "3"
data$attack_type[data$attack_type == "portsweep."] = "3"
data$attack_type[data$attack_type == "nmap."] = "3"
data$attack_type[data$attack_type == "satan."] = "3"
data$attack_type[data$attack_type == "buffer_overflow."] = "5"
data$attack_type[data$attack_type == "loadmodule."] = "5"
data$attack_type[data$attack_type == "perl."] = "5"
data$attack_type[data$attack_type == "rootkit."] = "5"
data$attack_type[data$attack_type == "back."] = "1"
data$attack_type[data$attack_type == "land."] = "1"
data$attack_type[data$attack_type == "neptune."] = "1"
data$attack_type[data$attack_type == "pod."] = "1"
data$attack_type[data$attack_type == "smurf."] = "1"
data$attack_type[data$attack_type == "teardrop."] = "1"
data$attack_type[data$attack_type == "ftp_write."] = "4"
data$attack_type[data$attack_type == "guess_passwd."] = "4"
data$attack_type[data$attack_type == "imap."] = "4"
data$attack_type[data$attack_type == "multihop."] = "4"
data$attack_type[data$attack_type == "phf."] = "4"
data$attack_type[data$attack_type == "spy."] = "4"
data$attack_type[data$attack_type == "warezclient."] = "4"
data$attack_type[data$attack_type == "warezmaster."] = "4"
data$attack_type[data$attack_type == "normal."] = "2"
data = data[!(data$attack_type=="0.00"),]
data = data[!(data$attack_type==""),]
data$attack_type = factor(data$attack_type)

#,13:20,23:41
#1,5,6, 8:11, 13:20, 23:41
#datapart <- data[,c(19,25,26,28,29,30,35,37,38,39,40)]
datapart <- data[,c(1,5,6, 8:11, 13:20, 23:41)]
head(datapart)
preProc  <- preProcess(datapart,na.remove = TRUE,method = c("center", "scale"))
datapart <- predict(preProc,datapart)
#data.Normalization (datapart,type="n3a",normalization="column")
#for (i in 1:34) if(range(datapart[,c(i)])[2] != 0) datapart[,c(i)] <- datapart[,c(i)]/range(datapart[,c(i)])[2]
#for (i in 1:34) print(range(datapart[,c(i)])[2])
#datapart[,c(1)] <- datapart[,c(1)]/8

#nrow(subset(datapart, num_access_files == 0 ))
#range(datapart[,c(1)])
#distanceBased <- kmeans(datapart,2)

distanceBased <- kmeans(datapart,5,nstart=25)

#table(data$attack_type,data$attack_type)
confusionMatrix(distanceBased$cluster,data$attack_type)

for (i in 1:11) print(range(datapart[,c(i)]))

wss <- (nrow(datapart))*sum(apply(datapart,2,var))
for (i in 2:5) wss[i] <- sum(kmeans(datapart,centers = i)$withinss)
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="sum of squares within groups")
summary(distanceBased)

head(datapart)

plot3d(datapart[,c(2,3,18)],col = distanceBased$cluster, main = "Kmeans Cluster")
plot3d(datapart,col = densityBased$cluster+1, main = "DBScan Cluster")
plot(graphBased)

summary(distanceBased)

plot(data[,c(1)])
head(datapart)
datapart <- data[,c(1,5,6)]
pca <- princomp(datapart, cor=T) 
plot(pca,type="1")
loadings(pca)
summary(pca, loadings=T)
biplot(pca)
indexes = sample(1:nrow(data), size=1)

Price <- c(6,7,6,5,7,6,5,6,3,1,2,5,2,3,1,2) 
Software <- c(5,3,4,7,7,4,7,5,5,3,6,7,4,5,6,3) 
Aesthetics <- c(3,2,4,1,5,2,2,4,6,7,6,7,5,6,5,7)
Brand <- c(4,2,5,3,5,3,1,4,7,5,7,6,6,5,5,7) 
data <- data.frame(Price, Software, Aesthetics, Brand)
pca <- princomp(data, cor=T) 
summary(pca, loadings=T)
