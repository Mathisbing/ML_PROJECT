rm(list=ls())
library(ggplot2)
library(dplyr)
set.seed(1234)
data_row<-data.frame(read.csv("C:\\zhuyibing\\ML_data\\student-alcohol-consumption\\student-por.csv",header =TRUE))
data_row[sapply(data_row,is.null)]<- NULL
summary(data_row)

n<-nrow(data_row)
m<-ncol(data_row)

data_row$G_total = round(rowSums(data_row[,c(31,32,33)])/3,0)
data_row$Dalc = round(rowSums(data_row[,c(27,28)])/2,0)
data<-data_row[,c(-28,-31,-32,-33)]

# unique(data$Dalc)
x_numeric<-sapply(data,is.numeric)
data_num<-data[,x_numeric,drop=FALSE]
summary(data_num)
data_num$G_total<-ifelse(data_num$G_total>mean(data_num$G_total), 1,0)
summary(data_num)
x_character<-sapply(data,is.character)
data_char<-data[,x_character,drop=FALSE]
summary(data_char)
data_num_sc<-scale(data_num)

matcor<-cor(round(data_num_sc))# 相关系数
library(pheatmap)
library(corrplot)
pheatmap(matcor, color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         cluster_row = FALSE,display_numbers = TRUE,
         number_format = "%.2f",cellwidth = 15, cellheight = 12, fontsize = 8,
         border = FALSE)

corrplot.mixed(matcor,number.cex =  0.75,cl.cex =0.75,tl.cex=0.55,tl.col = "black",lower.col = "black")
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

unique(data$G_total)
data[,44]<-data[,26]
data<-data[,-26]
names(data)[43]<-"G_total"
mean(data$G_total)
data$G_total<-ifelse(data$G_total>mean(data$G_total), 1,0)
matcor<-cor(round(data[,14:43]))# 相关系数
corrplot.mixed(matcor,number.cex =  0.75,cl.cex =0.75,tl.cex=0.55,tl.col = "black",lower.col = "black")

data_scale<-data.frame(scale(data[,-43]))
data[,1:42]=data_scale[,1:42]
n<- nrow(data)
p<-ncol(data)-1
names(data)[43]<-"class"
train<-sample(n,2*n/3)
test<-data[-train,]

# Decision tree
library(rpart)
library(rpart.plot)
tree.data<-rpart(class~.,data=data,subset=train,
                 method="class",
                 control = rpart.control(xval = 10, 
                                         minbucket = 10, cp = 0))
rpart.plot(tree.data,type = 2,box.palette = c("pink","gray"),cex=0.7)
# fancyRpartPlot(tree.data)
plot(tree.data,margin = 0.05)
text(tree.data,pretty=0,cex=0.8)

yhat=predict(tree.data,newdata=data[-train,],type='class')
y.test <- data[-train, "class"]
table(y.test,yhat)
err<-1-mean(y.test==yhat)
print(err)

plotcp(tree.data)
printcp(tree.data)


pruned_tree<-prune(tree.data,cp=0.0066722)
rpart.plot(pruned_tree,type = 2,box.palette = c("pink","gray"),cex=0.7)
plot(pruned_tree,margin = 0.1)
text(pruned_tree,pretty=0)

yhat1=predict(pruned_tree,newdata=data[-train,],type='class')
table(y.test,yhat1)
err1<-1-mean(y.test==yhat1)
print(err1)
# Random forests
library(randomForest)
fit.RF<-randomForest(as.factor(class) ~ .,data=data[train,],mtry=3,importance=TRUE)
pred.RF<-predict(fit.RF,newdata=data[-train,],type="class")
err.RF<-1-mean(y.test==pred.RF)
print(err.RF)

# Logistic regression
fit.logreg<- glm(as.factor(class)~.,data=data,family=binomial,subset=train)
pred.logreg<-predict(fit.logreg,newdata=data[-train,],type='response')
perf <-table(y.test,pred.logreg>0.5)
print(perf)
err.logreg<-1-mean(y.test==(pred.logreg>0.5))
print(err.logreg)


library(MASS)
# LDA
lda.data<- lda(class~.,data=data, subset=train)
pred.data.lda<-predict(lda.data,newdata=data[-train,])
perf <-table(y.test,pred.data.lda$class)
print(perf)
err.lda<-1-mean(y.test==pred.data.lda$class)
print(err.lda)


# ROC
library(pROC)
citation("pROC")
roc_curve<-roc(test$class,as.vector(pred.data.lda$x))
plot(roc_curve,col='blue', plot=TRUE, print.thres=TRUE, print.auc=TRUE)
roc_glm<-roc(test$class,as.vector(pred.logreg))
plot(roc_glm,add=TRUE,col='red')


t<-data.frame(yhat,test$class)
roc_tree<-roc(t$yhat,t$test.class)
plot(roc_tree,add=TRUE,col='green')


library('nnet')

# First trial with 5 hidden units
# We train 10 neural networks and keep the one with the smallest training error
loss<-Inf
for(i in 1:10){
  nn<- nnet(class~.  , data=data,subset=train, size=20, linout = TRUE,maxit=200,trace=FALSE,decay=2)
  print(c(i,nn$value))
  if(nn$value<loss){
    loss<-nn$value
    nn1.best<-nn
  }
}
pred1<- predict(nn1.best,newdata=test) # For plotting
pred1.test<-predict(nn1.best,newdata=test) 
mse1<-mean((pred1.test-test$class)^2)
mse1
K<-5
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
lambda.opt
