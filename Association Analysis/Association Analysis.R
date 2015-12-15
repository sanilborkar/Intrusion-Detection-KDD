##----------------------------------Load libraries and functions START -----------------------------
library(arules)
library(arulesViz)
library(Matrix)
rm(list=ls())
setwd("D:\\Study\\Learning R\\")

removeRedundantRules <- function(ruleset){
  subset_matrix <- is.subset(ruleset,ruleset)
  redundant <- colSums(subset_matrix,na.rm = TRUE) >= 2
  unique_better_rules <- ruleset[!redundant]
}
##----------------------------------Load libraries and functions END -----------------------------

dataset <- read.csv("kdd_dataset.csv",header = T,sep =',') 


##Discretize the data
dataset$duration <- discretize(dataset$duration,method="frequency",categories = 2)
dataset$src_bytes <- discretize(dataset$src_bytes,method="cluster",categories = 5)
dataset$dst_bytes <- discretize(dataset$dst_bytes,method="cluster",categories = 5)
dataset$num_compromised <- discretize(dataset$num_compromised,method="frequency",categories = 2)
dataset$num_root <- discretize(dataset$num_root,method="frequency",categories = 2)
dataset$num_file_creations <- discretize(dataset$num_file_creations,method="frequency",categories = 2)
dataset$count <- discretize(dataset$count,method="frequency",categories = 51)
dataset$srv_count <- discretize(dataset$srv_count,method="frequency",categories = 51)
dataset$serror_rate <- discretize(dataset$serror_rate,method="interval",categories = 10)
dataset$srv_serror_rate <- discretize(dataset$srv_serror_rate,method="interval",categories = 10)
dataset$rerror_rate <- discretize(dataset$rerror_rate,method="interval",categories = 10)
dataset$srv_rerror_rate <- discretize(dataset$srv_rerror_rate,method="interval",categories = 10)
dataset$same_srv_rate <- discretize(dataset$same_srv_rate,method="interval",categories = 10)
dataset$diff_srv_rate <- discretize(dataset$diff_srv_rate,method="interval",categories = 10)
dataset$srv_diff_host_rate <- discretize(dataset$srv_diff_host_rate,method="interval",categories = 10)
dataset$dst_host_count <- discretize(dataset$dst_host_count,method="cluster",categories = 10)
dataset$dst_host_srv_count <- discretize(dataset$dst_host_srv_count,method="cluster",categories = 10)
dataset$dst_host_same_srv_rate <- discretize(dataset$dst_host_same_srv_rate,method="interval",categories = 10)
dataset$dst_host_diff_srv_rate <- discretize(dataset$dst_host_diff_srv_rate,method="interval",categories = 10)
dataset$dst_host_same_src_port_rate <- discretize(dataset$dst_host_same_src_port_rate,method="interval",categories = 10)
dataset$dst_host_srv_diff_host_rate <- discretize(dataset$dst_host_srv_diff_host_rate,method="interval",categories = 10)
dataset$dst_host_serror_rate <- discretize(dataset$dst_host_serror_rate,method="interval",categories = 10)
dataset$dst_host_srv_serror_rate <- discretize(dataset$dst_host_srv_serror_rate,method="interval",categories = 10)
dataset$dst_host_rerror_rate <- discretize(dataset$dst_host_rerror_rate,method="interval",categories = 10)
dataset$dst_host_srv_rerror_rate <- discretize(dataset$dst_host_srv_rerror_rate,method="interval",categories = 10)
dataset[,c(1,7:12,14:15,18:22)] <- lapply(dataset[,c(1,7:12,14:15,18:22)],as.factor)
##x <-dataset[,32]
##hist(x)
##table(discretize(x,method="cluster",categories = 10))


##testdataset <- dataset[,c(2:4,7:9,11,12,14,15,18,19,22,42)]
testdataset <- dataset[,c(2:4,7:19,22,25:31,34:41,42)]
testdataset$attack_type <- sapply(testdataset[,'attack_type'], function(x) gsub('.$','',x))
testdataset$attack_type <- as.factor(testdataset$attack_type)

##back
rules <- apriori(testdataset,parameter = list(supp = 0.003, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=back"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##buffer_overflow
rules <- apriori(testdataset,parameter = list(supp = 0.00002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=buffer_overflow"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:10])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)


##ftp_write
rules <- apriori(testdataset,parameter = list(supp = 0.000002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=ftp_write"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:10])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##guess_passwd
rules <- apriori(testdataset,parameter = list(supp = 0.00005, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=guess_passwd"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:10])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

#imap
rules <- apriori(testdataset,parameter = list(supp = 0.00001, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=imap"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##ipsweep
rules <- apriori(testdataset,parameter = list(supp = 0.001, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=ipsweep"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##land
rules <- apriori(testdataset,parameter = list(supp = 0.00004, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=land"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:10])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##loadmodule
rules <- apriori(testdataset,parameter = list(supp = 0.000002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=loadmodule"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##multihop
rules <- apriori(testdataset,parameter = list(supp = 0.000002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=multihop"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##neptune
rules <- apriori(testdataset,parameter = list(supp = 0.2, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=neptune"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##nmap
rules <- apriori(testdataset,parameter = list(supp = 0.0002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=nmap"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##perl
rules <- apriori(testdataset,parameter = list(supp = 0.000002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=perl"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##phf
rules <- apriori(testdataset,parameter = list(supp = 0.000002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=phf"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:10])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##pod
rules <- apriori(testdataset,parameter = list(supp = 0.0002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=pod"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##portsweep
rules <- apriori(testdataset,parameter = list(supp = 0.001, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=portsweep"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##rootkit
rules <- apriori(testdataset,parameter = list(supp = 0.000002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=rootkit"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##satan
rules <- apriori(testdataset,parameter = list(supp = 0.0015, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=satan"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##smurf
rules <- apriori(testdataset,parameter = list(supp = 0.5, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=smurf"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##spy
rules <- apriori(testdataset,parameter = list(supp = 0.000002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=spy"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules[1:5])

##warezclient
rules <- apriori(testdataset,parameter = list(supp = 0.001, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=warezclient"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##warezmaster
rules <- apriori(testdataset,parameter = list(supp = 0.000002, conf = 0.9, maxlen = 5),appearance = list(rhs = c("attack_type=warezmaster"),default = "lhs"))
summary(rules)
sorted_rules <- sort(rules,by="lift")
inspect(sorted_rules[1:5])

unique_better_rules <- removeRedundantRules(rules)
sorted_rules <- sort(unique_better_rules,by="lift")
inspect(sorted_rules)

##
dataset1 <- read.csv("kdd_dataset.csv",header = T,sep =',') 
temp <- dataset1[which(dataset1$attack_type == 'warezmaster.'),]
unique(temp$num_file_creations)
