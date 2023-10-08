#Lets build our Decision Tree Model

library(ISLR)
head(College)
df<-College
library(ggplot2)
ggplot(data=df,aes(x=Room.Board,y=Grad.Rate,color=Private))+geom_point(size=4,alpha=0.5)
ggplot(data=df,aes(x=F.Undergrad))+geom_histogram(aes(fill=Private),color='black',bins=50)
ggplot(data=df,aes(x=Grad.Rate))+geom_histogram(aes(fill=Private),color='black',bins=50)

subset(df,Grad.Rate>100)
df['Cazenovia College','Grad.Rate']<-100
str(df)

library(caTools)
set.seed(101)
sample<-sample.split(df$Private,SplitRatio = 0.7)
train<-subset(df,sample==T)
test<-subset(df,sample==F)
library(rpart)
tree<-rpart(Private ~ .,data=train,method='class')
tree.preds<-predict(tree,test)
tree.preds
head(tree.preds)
tree.preds<-as.data.frame(tree.preds)
joiner<-function(x)
{
  if(x>=0.5)
  {
    return('Yes')
  }else{
    return('No')
  }
}

tree.preds$Private<-sapply(tree.preds$Yes,joiner)
head(tree.preds)
table(tree.preds$Private,test$Private)

library(rpart.plot)
prp(tree)

#Lets Build our Random Forest Model

library(randomForest)
rf.model<-randomForest(Private ~ .,data=train,importance=T)
rf.model$confusion
rf.model$importance
p<-predict(rf.model,test)
p
table(p,test$Private)
