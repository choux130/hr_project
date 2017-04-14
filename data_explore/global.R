library(shiny)
library(ggplot2)

data=read.csv("HR_analytics.csv")
names(data)=c("satisf_level","last_eval","num_proj",
              "ave_mon_hrs","time_spend","work_accid",
              "left_or_not","promo_last_5yrs","department",
              "salary")
data=data[,c(1:6,8:10,7)]
data[,c("work_accid","promo_last_5yrs","left_or_not")]=
  lapply(data[,c("work_accid","promo_last_5yrs","left_or_not")],as.factor)

################################
#### Summary Table by group ###
###############################
t_bygroup=function(d, xx, yy, round){
  # an elegant way to install a missing package
  if (!require("plyr")) install.packages("plyr")
  t <- ddply(d, yy, .fun = function(dd){
    c(Mean = round(mean(dd[,xx],na.rm=TRUE),round),
      Sd = round(sd(dd[,xx],na.rm=TRUE),round),
      min=round(min(dd[,xx]),round), 
      Q1=round(quantile(dd[,xx],0.25),round),
      Q2=round(quantile(dd[,xx],0.5),round),
      Q3=round(quantile(dd[,xx],0.75),round), 
      Max=round(max(dd[,xx]),round)) })
  return(t)
}

#########################################
#### Overlapping Histograms by group ####
#########################################
hist_bygroup=function(d,xx,yy,name){
  if (!require("ggplot2")) install.packages("ggplot2")
  ggplot(d, aes_string(x=xx, color=yy, fill=yy)) + 
    geom_histogram(aes(y=..density..), alpha=0.5, position="identity") +
    geom_density(alpha=.3)+
    ggtitle(name)  
}

#######################################
#### Side by side boxplot by group ####
#######################################
box_bygroup=function(d,xx,yy,name){
  if (!require("ggplot2")) install.packages("ggplot2")
  ggplot(d, aes_string(x=yy, y=xx, fill=yy)) + 
    geom_boxplot() + ggtitle(name) 
}

########################################
#### Function for hist and boxplot ####
#######################################
all_bygroup=function(d, xx, yy, round){
  
  #the histogram
  hist=hist_bygroup(d=data, xx, yy, paste("Histogram for", xx, "by", yy))
  
  #the boxplot
  box=box_bygroup(d=data, xx, yy, paste("Boxplot for", xx, "by", yy))
  
  #the package for arrange plots 
  if (!require("gridExtra")) install.packages("gridExtra")
  grid.arrange(hist,box, nrow=1) 
}

# selectInput("var1", label = h3("Variable_1"), 
#             c("Whether the employee has left (numeric)"="satisf_level", 
#               "Last evaluation (numeric)"="last_eval", 
#               "Number of projects (numeric)"="num_proj", 
#               "Average monthly hours (numeric)"="ave_mon_hrs", 
#               "Time spent at the company (numeric)"="time_spend",
#               "Work accident or not (categorical)"="work_accid",
#               "Had a promotion in the last 5 years or not (categorical)"="promo_last_5yrs",
#               "Department (categorical)"="department",
#               "Salary (categorical)"="salary",
#               "Left or not (categorical)"="left_or_not")),
