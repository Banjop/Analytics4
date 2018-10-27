#load libraries
library(rpart)
library(rpart.plot)
gender= sample(x=c('M','F'),size=10000,prob=c(.6,.4),replace=T)
age=ceiling(rnorm(10000,35,5))
buy= sample(x=c('Yass','Nope'),size=10000,prob=c(.5,.5),replace=T)
df= data.frame(buy,age,gender)
head(df)
dtree1= rpart(buy~gender+age,data=df,minsplit=10,minbucket=4,cp=.001)#cp=complexity parameter, changes the generic and structure of the tree
#min bucket=one-third of min. split--creates a split according to min. no. of observations(no. of rows)
#

dtree1
dtree1$variable.importance
rpart.plot(dtree1,cex=.8,nn=T)
