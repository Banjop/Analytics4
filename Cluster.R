#chaid
#CHAID - dataset USvote #multisplit
# require(rsample) # for dataset and splitting also loads broom and tidyr
#install partkit prior to chaid
install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)  #library for performing CHAID decision tree
#used when all variables are categories and when there are more than two criteria
#Dataset
data(USvote)  #from lib CHAID
?USvote
class(USvote)
head(USvote)
str(USvote)
#Quick CHAID analysis
set.seed(101)
nrow(USvote)
sample1 = USvote[sample(1:nrow(USvote), 1000),]
chaidModel = chaid(vote3 ~ ., data = sample1, control=chaid_control(minbucket = 20, minsplit=7, minprob=0))
#minsplit	Number of observations in splitted response at which no further split is desired.
#minbucket Minimum number of observations in terminal nodes.
#minprob	Mininimum frequency of observations in terminal nodes.
print(chaidModel)
plot(chaidModel)
?chaid


#Cluster
# HH MA example  - customer

install.packages("amap")
library(amap)
library(dplyr)
##Read the data in the file
url = 'https://docs.google.com/spreadsheets/d/1PWWoMqE5o3ChwJbpexeeYkW6p4BHL9hubVb1fkKSBgA/edit#gid=2073914016'
library(gsheet)


data = as.data.frame(gsheet2tbl(url))
str(data)
head(data)

names(data)
summary(data)
str(data)
#example
marks1=ceiling(rnorm(10,6,2))
marks2=ceiling(rnorm(10,5,2))
df=data.frame(marks1,marks2)
head(df)
km=kmeans(df,centers=2)
?kmeans
km$cluster
df2=cbind(df,cluster=km$cluster)
df2[order(df2$cluster),]
plot(df$marks1,df$marks2,col=2:3,pch=c(16:17))
?points
points=(,pch=20,cex=2,col=km$cluster)    
    ###Verify the data
colnames(data)
sapply(data,class)
class(data$Age)
?apply
apply(data, 2, FUN=class)  #are all numeric
dim(data)
head(data)
summary(data)
###Run the kmeans algorithm to generate the clusters
#?amap::Kmeans
names(data)

k1 <- amap::Kmeans(data[,-1],centers=3, iter.max = 200)

#nc = NbClust(data[-1], distance="euclidean",min.nc=2, max.nc=15, method="average")

str(data)

k1$centers  # group means
###Fetch size/n of obs for the groups
k1$size
###Fetch sum of squared  for the groups
k1$withinss
###Fetch the cluster for each obs
#k1$cluster
k1$cluster
k1$centers
k1$cluster[9000:9800]
table(k1$cluster)
k1$size
data_clus_2 <- data[ k1$cluster == 2,]
(data_clus_2)
mean(data_clus_2$Age)
data_clus_2$Cust_id

# Write CSV
write.csv(data_clus_2[,1], file = "./data/data_clus_2.csv")
