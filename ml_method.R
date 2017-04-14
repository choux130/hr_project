if(!require("data.table")) intall.pacakges("data.table")
library(data.table)

#### Read data ####
dat<- fread("/Users/chou/Google Drive/websites/github/myblog-master/data/HR_analytics.csv",
             sep=",")
data=dat 
names(data)=c("satisf_level","last_eval","num_proj",
              "ave_mon_hrs","time_spend","work_accid",
              "left_or_not","promo_last_5yrs","department",
              "salary")
data=data[,c(1:6,8:10,7)]
data[,c("work_accid","promo_last_5yrs","left_or_not")]=
  lapply(data[,c("work_accid","promo_last_5yrs","left_or_not")],as.factor)
str(data) 
which(is.na(data), arr.ind=TRUE) #the indices of NA values

##### Cross Validation ####
library(caret)
library(irr) # kappa2()
set.seed(123)
# the indice for the training set
# For other data splitting, the random sampling is done within the 
# levels of y when y is a factor in an attempt to balance the class distributions 
# within the splits.
datavector = createDataPartition(data$left_or_not,
                                 p = 0.80, list = FALSE)
d_train=data[datavector,]  #training data
dim(d_train) 
d_test=data[-datavector,] 

##### logistic regression ####
cv = trainControl(method = "cv", number = 10)
m_log=train(left_or_not ~. , 
            data=data.frame(d_train),
            method="glm", family=binomial, trControl = cv)
summary(m_log)
pred_log=predict(m_log,d_test)
# accuracy
acc=mean(pred_log == d_test$left_or_not)
acc
kappa=kappa2(data.frame(pred_log, d_test$left_or_not))$value
kappa

#### classification tree


