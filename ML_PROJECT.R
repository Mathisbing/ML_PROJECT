rm(list=ls())
library(rpart)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(corrplot)
set.seed(1234)

data_row<-data.frame(read.csv("C:\\zhuyibing\\ML_data\\student-alcohol-consumption\\student-por.csv",header =TRUE))
data_row[sapply(data_row,is.null)]<- NULL
x_numeric<-sapply(data_row,is.numeric)
data_num<-data_row[,x_numeric,drop=FALSE]
summary(data_num)
data_num_cor<-cor(round(data_num))
corrplot(data_num_cor, order = "hclust", addrect = 8)
# Analyze Dalc
D.b <- data_row %>%  ggplot(aes(x=Dalc))+geom_bar(stat = "count") 
W.b <- data_row %>% ggplot(aes(x=Walc))+geom_bar(stat = "count")
grid.arrange(D.b, W.b, nrow = 2)
# Dealing with features that are too relevant
data_row$G_total = round(rowSums(data_row[,c(31,32,33)])/3,0)
data_row$Dalc = round(rowSums(data_row[,c(27,28)])/2,0)
data<-data_row[,c(-28,-31,-32,-33)]
data$Dalc<-ifelse(data$Dalc>2.5,1,0)
# Analyze the relationship between Dalc(Medu,failures) and grades
avgG<-rowMeans(cbind(data_row$G1,data_row$G2,data_row$G3))
data %>% ggplot(aes(x= Dalc, y = avgG, group = Dalc))+
  geom_boxplot(fill = "darkblue", colour = "gray",
               outlier.colour = "red", outlier.shape = 1)
data %>% ggplot(aes(x= Medu, y = avgG, group = Medu))+
  geom_boxplot(fill = "darkblue", colour = "gray",
               outlier.colour = "red", outlier.shape = 1)
data$G_total<-ifelse(data$G_total>mean(data$G_total), 1,0)
data %>% ggplot(aes(x= failures, y = avgG, group = failures))+
  geom_boxplot(fill = "darkblue", colour = "gray",
               outlier.colour = "red", outlier.shape = 1)
data$G_total<-ifelse(data$G_total>mean(data$G_total), 1,0)
# Draw a heat map of numerical features
data_num<-data[,sapply(data,is.numeric),drop=FALSE]
data_num_cor<-cor(round(data_num))
corrplot.mixed(data_num_cor,number.cex =  0.75,cl.cex =0.75,tl.cex=0.55,tl.col = "black",lower.col = "black")

#Dummy coding
data$romantic <- ifelse(data$romantic == "yes", 1, 0)
data$internet <- ifelse(data$internet == "yes", 1, 0)
data$higher<- ifelse(data$higher == "yes", 1, 0)
data$nursery <- ifelse(data$nursery == "yes", 1, 0)
data$activities <- ifelse(data$activities == "yes", 1, 0)
data$paid<- ifelse(data$paid == "yes", 1, 0)
data$famsup <- ifelse(data$famsup == "yes", 1, 0)
data$schoolsup <- ifelse(data$schoolsup == "yes", 1, 0)
data$school <- ifelse(data$school == "GP", 1, 0)
data$sex <- ifelse(data$sex == "M", 1, 0)
data$address <- ifelse(data$address == "U", 1, 0)
data$famsize <- ifelse(data$famsize == "GT3", 1, 0)
data$Pstatus <- ifelse(data$Pstatus == "A", 1, 0)
data<-cbind(select_if(data,is.numeric),as.data.frame(model.matrix(~Fjob-1,data)),
            as.data.frame(model.matrix(~Mjob-1,data)),
            as.data.frame(model.matrix(~reason-1,data)),
            as.data.frame(model.matrix(~guardian-1,data)))
# Divide the data set
data[,44]<-data[,26]
data<-data[,-26]
names(data)[43]<-"G_total"
names(data)[43]<-"class"
n<- nrow(data)
train<-sample(n,2*n/3)
test<-data[-train,]
y.test <- data[-train, "class"]

# Logistic regression
fit.logreg<- glm(as.factor(class)~.,data=data,family=binomial,subset=train)
pred.logreg<-predict(fit.logreg,newdata=data[-train,],type='response')
perf <-table(y.test,pred.logreg>0.5)
print(perf)
err.logreg<-1-mean(y.test==(pred.logreg>0.5))
print(err.logreg)

# LDA
library(MASS)
lda.data<- lda(class~.,data=data, subset=train)
pred.data.lda<-predict(lda.data,newdata=data[-train,])
perf <-table(y.test,pred.data.lda$class)
print(perf)
err.lda<-1-mean(y.test==pred.data.lda$class)
print(err.lda)

# Decision tree
library(rpart)
library(rpart.plot)
tree.data<-rpart(class~ .,data=data,subset=train,
                 method="class",
                 control = rpart.control(xval = 10, 
                                         minbucket = 10, cp = 0))
rpart.plot(tree.data,type = 2,box.palette = c("pink","gray"),cex=0.7)
yhat=predict(tree.data,newdata=data[-train,],type='class')
y.test <- data[-train, "class"]
table(y.test,yhat)
err<-1-mean(y.test==yhat)
print(err)
pruned_tree<-prune(tree.data,cp=0.0066722)
rpart.plot(pruned_tree,type = 2,box.palette = c("pink","gray"),cex=0.7)
yhat1=predict(pruned_tree,newdata=data[-train,],type='class')
table(y.test,yhat1)
err.tree<-1-mean(y.test==yhat1)
print(err.tree)
plotcp(tree.data,col="blue")
plotcp(pruned_tree,col="red")


# ROC
library(pROC)
citation("pROC")
roc_curve<-roc(test$class,as.vector(pred.data.lda$x))
plot(roc_curve,col='blue', plot=TRUE, print.thres=TRUE)
roc_glm<-roc(test$class,as.vector(pred.logreg))
plot(roc_glm,add=TRUE,col='red',plot=TRUE,print.thres=TRUE)
t<-data.frame(yhat1,test$class)
roc_tree<-roc(t$yhat1,t$test.class)
plot(roc_tree,add=TRUE,col='green',print.thres=TRUE)


library('nnet')
library(NeuralNetTools)
K<-10
folds=sample(1:K,n,replace=TRUE)
lambda<-c(0.01,0.02,0.05,0.1,0.5,1,2)
N<-length(lambda)
CV<-rep(0,N)
for(i in (1:N)){
  for(k in (1:K)){
    nn<- nnet(class~. , data=data,subset = train,size=10, linout = TRUE, 
              decay=lambda[i], trace=FALSE)
    pred<-predict(nn,newdata=data[-train,])
    CV[i]<-CV[i]+ sum((pred-test$class)^2)
  }
  CV[i]<-CV[i]/n
}
plot(lambda,CV,type='l')
lambda.opt<-lambda[which.min(CV)] # Best decay coefficient
lambda.opt<-2
loss<-Inf
for(i in 1:40){
  nn<- nnet(class~.  , data=data,subset=train, size=20, linout = TRUE,maxit=200,trace=FALSE,decay=lambda.opt)
  print(c(i,nn$value))
  if(nn$value<loss){
    loss<-nn$value
    nn1.best<-nn
  }
}
pred1<- predict(nn1.best,newdata=test) 
pred1.test<-predict(nn1.best,newdata=test) 
err.nnet<-mean((pred1.test-test$class)^2)
olden(nn1.best)+ggtitle("Variable importance using connection weights")
print(err.nnet)

# Bagging
library(adabag)
data$class = factor(data$class)
bagging.data <- bagging(class~.,data=data[train,],mfinal=100 )
pred.data.bagging<-predict(bagging.data,newdata=data[-train,])
err.bagging<-1-mean(y.test==pred.data.bagging$class)
print(err.bagging)
# randomForest
library(randomForest)
fit.RF<-randomForest(as.factor(class) ~ .,data=data[train,],mtry=3,importance=TRUE)
pred.RF<-predict(fit.RF,newdata=data[-train,],type="class")
err.RF<-1-mean(y.test==pred.RF)
print(err.RF)
varImpPlot(fit.RF, main = "variable importance",n.var=10,type=1)
# Cross-validation randomForest and Decision tree
n<-nrow(data)
K<-10
rang<-rank(runif(n))
taille<- n %/% K
bloc<- as.factor((rang-1)%/%taille+1)
all.err1<-numeric(0)
all.err2<-numeric(0)
print(summary(bloc))
for (K in 1:K){
  tree.data<-rpart(class~.,data=data[bloc!=K,],
                   method="class",
                   control = rpart.control(xval = 10, 
                                           minbucket = 10, cp = 0))
  yhat=predict(tree.data,newdata=data[bloc==K,],type='class')
  err<-1-mean(data[bloc==K,]$class==yhat)
  all.err1=rbind(all.err1,err)
  
  fit.RF<-randomForest(as.factor(class) ~ .,data=data[bloc!=K,],mtry=3,importance=TRUE)
  pred.RF<-predict(fit.RF,newdata=data[bloc==K,],type="class")
  err.RF<-1-mean(data[bloc==K,]$class==pred.RF)
  all.err2=rbind(all.err2,err.RF)
}
print(mean(all.err1))
print(mean(all.err2))
