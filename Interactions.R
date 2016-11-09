#Read Data

setwd("C:\\Users\\gaura\\Documents\\MIM - UMCP\\1st Sem\\627 - Data Analytics for Information Professionals\\Project")
mydata = read.csv(("AgeWiseSmokingPopulation_Tax_Updated.csv", header = TRUE, na.strings=c("", "NULL", "NA"))
mydata = read.csv("AgeWiseSmokingPopulation_Tax_updated.csv", header=TRUE, na.strings=c("","NULL","NA"))
mydata$Age<-factor(mydata$Age)
summary(mydata$Age)

#Data Coding
mydata$State_Tax_Category[mydata$State_Tax<0.2875]<-"Very Low"
mydata$State_Tax_Category[mydata$State_Tax<=0.2875]<-"Very Low"
mydata$State_Tax_Category[mydata$State_Tax>0.2875 & mydata$State_Tax<=0.55]<-"Low"
mydata$State_Tax_Category[mydata$State_Tax>0.55 & mydata$State_Tax<=1.00]<-"Medium"
mydata$State_Tax_Category[mydata$State_Tax>1.00]<-"High"
mydata$State_Tax_Category<-factor(mydata$State_Tax_Category, levels = c("Very Low","Low","Medium","High"))
summary(mydata$State_Tax_Category)

#ANOVA test 
m = anova(Data_Value~State_Tax_Category*Age,data=mydata)
m = aov(Data_Value~State_Tax_Category*Age,data=mydata)
summary(m)

#Visualization
library(ggplot2)
library(Rmisc)
mydata.ci = summarySE(mydata,measurevar="Data_Value",groupvars=c("State_Tax_Category","Age")) 
ggplot(mydata.ci,aes(x=State_Tax_Category,y=Data_Value, fill=Age))+geom_bar(position=position_dodge(),stat="identity")+geom_errorbar(aes(ymin=Data_Value-se,ymax=Data_Value+se),width=.2,position=position_dodge(.9))+labs(x="Tax", y="%Smoking Population")
ggplot(mydata.ci,aes(x=State_Tax_Category,y=Data_Value, fill=Age))+geom_bar(position=position_dodge(),stat="identity")+geom_errorbar(aes(ymin=Data_Value-se,ymax=Data_Value+se),width=.2,position=position_dodge(.9))+labs(x="Tax", y="% Smoking Population")
##Age = 18-24
Age_18to24 = subset(mydata, Age="18 to 24 Years")
summary(aov(Data_Value~State_Tax_Category,data=Age_18to24))
##Age = 25-44
Age_25to44 = subset(mydata, Age="25 to 44 Years")
summary(aov(Data_Value~State_Tax_Category,data=Age_25to44))
##Age = 45-64
Age_45to64 = subset(mydata, Age="45 to 64 Years")
summary(aov(Data_Value~State_Tax_Category,data=Age_45to44))
summary(aov(Data_Value~State_Tax_Category,data=Age_45to64))
summary(Age_45to64)
##Age = 64 and older
Age_65AndOlder = subset(mydata, Age="65 Years and Older")
summary(aov(Data_Value~State_Tax_Category,data=Age_65AndOlder))
nrows(Age_65AndOlder)
nrow(Age_65AndOlder)
Age_25to44 = subset(mydata, Age=="25 to 44 Years")
nrow(Age_25to44)
summary(aov(Data_Value~State_Tax_Category,data=Age_25to44))
##Age = 45-64
Age_45to64 = subset(mydata, Age=="45 to 64 Years")
nrow(Age_45to64)
summary(aov(Data_Value~State_Tax_Category,data=Age_45to44))
summary(aov(Data_Value~State_Tax_Category,data=Age_45to64))
##Age = 64 and older
Age_65AndOlder = subset(mydata, Age=="65 Years and Older")
summary(aov(Data_Value~State_Tax_Category,data=Age_65AndOlder))
##Age = 18-24
Age_65AndOlder = subset(mydata, Age=="18 to 24 Years")
Age_65AndOlder = subset(mydata, Age=="65 Years and Older")
Age_18to24 = subset(mydata, Age=="18 to 24 Years")
summary(aov(Data_Value~State_Tax_Category,data=Age_18to24))
interaction.plot(mydata$State_Tax_Category, mydata$Age, mydata$Data_Value)
interaction.plot(mydata$State_Tax_Category, mmydata$Data_Valueydata$Age, mydata$Data_Value)
interaction.plot(mydata$State_Tax_Category, mydata$Data_Value, mydata$Age)
interaction.plot(mydata$State_Tax_Category, mydata$Age, mydata$Data_Value)
interaction.plot(mydata$State_Tax_Category, mydata$Age=Age, mydata$Data_Value)
interaction.plot(mydata$State_Tax_Category, mydata$Age, mydata$Data_Value,tracel.label=Age)
interaction.plot(mydata$State_Tax_Category, mydata$Age, mydata$Data_Value,tracel.label="Age")
interaction.plot(mydata$State_Tax_Category, mydata$Age=Age, mydata$Data_Value, xlab="State Excise Tax", ylab="Mean Percent Smoking Population", trace.label="Age Groups")
interaction.plot(mydata$State_Tax_Category, mydata$Age, mydata$Data_Value, xlab="State Excise Tax", ylab="Mean Percent Smoking Population", trace.label="Age Groups")

#Histogram
qqnorm(Age_18to24$Data_Value, xlab = "Smoking percentage for Age 18 - 24 age group population")
qqline(Age_18to24$Data_Value)
hist(Age_18to24$Data_Value, xlab = "Smoking percentage for Age 18 - 24 age group population")

qqnorm(Age_25to44$Data_Value)
qqline(Age_25to44$Data_Value)
hist(Age_25to44$Data_Value, xlab = "Smoking percentage for Age 25 to 44 age group population")

qqnorm(Age_45to64$Data_Value)
qqline(Age_45to64$Data_Value)
hist(Age_45to64$Data_Value, xlab = "Smoking percentage for Age 45 to 64 age group population")

qqnorm(Age_65AndOlder$Data_Value)
qqline(Age_65AndOlder$Data_Value)
hist(Age_65AndOlder$Data_Value, xlab = "Smoking percentage for Age 65 years and older age group population")

#ANOVA Test
m = anova(Data_Value~State_Tax_Category*Age,data=mydata)

library(car)
leveneTest(Data_Value~State_Tax_Category,mydata)
leveneTest(Data_Value~Age,mydata)
