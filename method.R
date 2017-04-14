#########################
#### Import Raw Data ####
#########################
dat=read.csv("https://choux130.github.io/myblog/data/HR_analytics.csv",
             header=TRUE)
data=dat # keep raw data pure

##############################
#### Rename the variables ####
##############################
names(data)=c("satisf_level","last_eval","num_proj",
              "ave_mon_hrs","time_spend","work_accid",
              "left_or_not","promo_last_5yrs","department",
              "salary")

##########################
#### Reorder the data ####
##########################
data=data[,c(1:6,8:10,7)]

##################################
#### Correct variables' types ####
##################################
data[,c("work_accid","promo_last_5yrs","left_or_not")]=
  lapply(data[,c("work_accid","promo_last_5yrs","left_or_not")],as.factor)
str(data) 

####################
#### Finding NA ####
####################
which(is.na(data), arr.ind=TRUE) #the indices of NA values

##########################
#### Cross Validation ####
##########################
# https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
# http://topepo.github.io/caret/index.html
if (!require("caret")) install.packages("caret")
library(caret)















cv2=function(data, prop1, i){
  set.seed(i)
  N = nrow(data)
  n = ceiling(N*prop1)
  ind = sample(N,n)
  sep1= data[ind,]
  sep2= data[-ind,]
  
  list(dat_1=sep1, dat_2=sep2)
}

# This is the test set which will only be used in the model selection part. 
i=1234
test=cv2(data,0.9, i)$dat_2 #1499   10

# This is the remaining dataset after dropping test set
traindd=cv2(data,0.9, i)$dat_1 #13500    11

# The number of fold
k=10
traindd$cv_group=sample(rep(1:k, length.out=nrow(traindd)), 
                     nrow(traindd), replace=FALSE)

#### Fold - 1
# the fold 1 validation set
first_val=traindd[traindd$cv_group=="1",]
val_1=traindd[,-11]
# the fold 1 train set 
first_train=traindd[traindd$cv_group!="1",]
train_1=first_train[,-11]


libaray(caret)



##############################
#### Logistic Regresstion ####
##############################

# all-subsets
# https://cran.r-project.org/web/packages/bestglm/vignettes/bestglm.pdf
# https://cran.r-project.org/web/packages/bestglm/bestglm.pdf
library(bestglm)
library(dummies)
train_1[,10]=as.numeric(train_1[,10])-1
Xy= train_1

#args(bestglm)
m_log_bic=bestglm(Xy, family=binomial, IC="BIC")

table(Xy[,10])




# Training set
m_log_prob_train=predict(m_logist, type="response")
pre_train=rep("0",nrow(train_1))
pre_train[m_log_prob_train>0.5]="1"
table(pre_train, true=train_1$left_or_not)
mean(pre_train==train_1$left_or_not) # misclassification rate

# Validation set
m_log_prob_val=predict(m_logist, val_1,type="response")
pre_val=rep("0",nrow(val_1))
pre_val[m_log_prob_val>0.5]="1"
table(pre_val, true=val_1$left_or_not)
mean(pre_val==val_1$left_or_not) # misclassification rate





