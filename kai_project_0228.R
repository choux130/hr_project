library(readr)
data<- read_csv("C:/Users/Administrator/Desktop/project/HR_comma_sep.csv")
View(data)
names(data)=c("satisf_level","last_eval","num_proj",
              "ave_mon_hrs","time_spend","work_accid",
              "left_or_not","promo_last_5yrs","department",
              "salary")
# Reorder the data
data=data[,c(1:6,8:10,7)]

# Correct variables' attributes
data$work_accid=as.factor(data$work_accid)
data$promo_last_5yrs=as.factor(data$promo_last_5yrs)
data$left_or_not=as.factor(data$left_or_not)
data$department=as.factor(data$department)
data$salary=as.factor(data$salary)
str(data) 
# Finding NA
which(is.na(data), arr.ind=TRUE) #the indices of NA values


#Data Exploration (Qualitative Explanotary Variables vs. Qualitative Binary Response Variable)
leave_or_not <- factor(data$left_or_not,labels = c("stay", "leave"))
workaccident<- factor(data$work_accid,labels = c("never happened", "happened"))
promotion <- factor(data$promo_last_5yrs,labels = c("not promoted", "promoted"))
library(gmodels)
CrossTable(x = workaccident, y = leave_or_not, digits=3, max.width = 5, prop.r=TRUE,prop.chisq=FALSE, prop.c=FALSE,format=c("SPSS"))
CrossTable(x = promotion, y = leave_or_not, digits=3, max.width = 5, prop.r=TRUE,prop.t=TRUE,prop.chisq=FALSE, prop.c=TRUE,format=c("SPSS"))


##Machine learning part
##split dataset into trainging and test set(4:1)
#logistic regression
library(caret) #createDataPartition()
library(irr)
set.seed(123)
datavector <- createDataPartition(data$left_or_not,p = 0.80, list = FALSE)
trainprocessdata=data[datavector,]  ##training data
testprocessdata=data[-datavector,]  ##test data
##establish general function of model
library(C50)
##cart with cross-validation
reps=100
cvresult=numeric(reps)
accuracy=numeric(reps)
for(rep in 1:length(cvresult)){
 useForCVRep <- rbinom(n=dim(trainprocessdata)[[1]],size=1,prob=0.25)>0
 dtrain=subset(trainprocessdata,!useForCVRep)
 dcv=subset(trainprocessdata,useForCVRep)
 treemodel <- C5.0(left_or_not~., data=dtrain)
 treepred <- predict(treemodel,dcv)
 treeactual <- dcv$left_or_not
 cvresult[rep] <- kappa2(data.frame(treeactual,treepred))$value
 accuracy[rep]=mean(treeactual==treepred)
}
##or
ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "oneSE")
grid <- expand.grid(.model = c("tree","rules"),.trials = c(1, 5, 10, 15, 20, 25, 30, 35),.winnow =c(TRUE,FALSE))
cart<- train(left_or_not ~ ., data = trainprocessdata, method = "C5.0",metric = "Kappa",trControl = ctrl,tuneGrid = grid)
ggplot(cart) + theme_bw()
##we choose trial=10,rules=false
result=C5.0(left_or_not~.,data=trainprocessdata,trials=10,rules=FALSE)
predictresult=predict(result,testprocessdata)
mean(predictresult==testprocessdata$left_or_not)
##get prediction accuracy=0.9836
kappa <- kappa2(data.frame(testprocessdata$left_or_not, predictresult))$value
kappa
##get kappa statistics=0.954371
CrossTable(testprocessdata$left_or_not,predictresult,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('actual condition', 'predicted condition'),format=c("SPSS"))
vrimp=varImp(result)
vrimp[,"name"]=c("satisf_level","last_eval","num_proj", "ave_mon_hrs","promo_last_5yrs","time_spend","salary","department","work_accid")
barplot(vrimp$Overall[order(vrimp$Overall, decreasing = TRUE)],names.arg=vrimp$name,ylab="importance",ylim = c(0, 100), main = "Variables Relative Importance",col = "lightblue")

##adaboost
ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "oneSE")
Grid <- expand.grid(maxdepth=10,nu=0.1,iter=50)
adamodel <- train(left_or_not~., data=trainprocessdata,method = "ada", trControl = ctrl,tuneGrid=Grid,metric = "Accuracy",preProc = c("center", "scale"))
adaprediction=predict(adamodel,testprocessdata)
mean(adaprediction == testprocessdata$left_or_not)
kappa <- kappa2(data.frame(testprocessdata$left_or_not, adaprediction))$value

##bagging
bagmodel=train(left_or_not ~ ., data =trainprocessdata, method = "treebag",trControl = ctrl)
bagprediction=predict(bagmodel,testprocessdata)
mean(bagprediction == testprocessdata$left_or_not)
kappa <- kappa2(data.frame(testprocessdata$left_or_not, bagprediction))$value

##logistic regression
glmmodel=glm(left_or_not~.,data=trainprocessdata,family=binomial("logit"))
glmprediction<-predict(glmmodel, newdata = testprocessdata,type = "response")
glmprediction <- as.numeric(glmprediction > 0.5)
mean(glmprediction == testprocessdata$left_or_not)
kappa <- kappa2(data.frame(testprocessdata$left_or_not, glmprediction))$value
##get prediction accuracy=0.7852
CrossTable(testprocessdata$left_or_not,train_class_predictions,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('actual condition', 'predicted condition'),format=c("SPSS"))


##neural network
ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "oneSE")
nnetgrid<- expand.grid(.decay = c(0.1, 0.01),.size = c(10,12,14,16,18))
nnetfit <- train(left_or_not ~ ., data =trainprocessdata, method = "nnet",maxit = 2000,metric = "Accuracy",trControl = ctrl, tuneGrid =nnetgrid, trace = F, MaxNWts = 10000)
ggplot(nnetfit) + theme_bw()
##size=12 and decay=0.1 is an appropriate combination
nnetresult=nnet(left_or_not~.,data=trainprocessdata,size=12,maxit=2000,decay=0.1)
nnetpredict<- predict(nnetresult,testprocessdata,type = "class")
mean(nnetpredict == testprocessdata$left_or_not)
kappa <- kappa2(data.frame(testprocessdata$left_or_not, nnetpredict))$value

##svm
library(e1071)
svmtune <- tune(svm,left_or_not~ ., data =trainprocessdata,kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 10, 100),
gamma = c(0.01, 0.05, 0.1, 0.5, 1)))
svmtune
svmmodel <- svmtune$best.model
svmpredict<- predict(svmmodel,testprocessdata,type = "class")
mean(svmpredict==testprocessdata$left_or_not)
kappa <- kappa2(data.frame(testprocessdata$left_or_not, svmpredict))$value

##random forest
library(randomForest)
rfranges <- list(ntree = c(500, 1000, 1500), mtry = 2:6)
rftune <- tune(randomForest,left_or_not~ ., data =trainprocessdata, ranges = rfranges)
rftune$best.parameters
rfbest <- rftune$best.model
rfpredictions <- predict(rfbest, testprocessdata)
mean(rfbestpredictions==testprocessdata$left_or_not)
kappa <- kappa2(data.frame(testprocessdata$left_or_not, rfbestpredictions))$value
varImpPlot(rfbest,type=2)
CrossTable(testprocessdata$left_or_not,rfpredictions,prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,dnn = c('actual condition', 'predicted condition'),format=c("SPSS"))